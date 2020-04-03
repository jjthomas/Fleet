#include <stdint.h>
#include <sys/time.h>
#include <fstream>
#include <stdlib.h>
#include <string.h>
#include <cuda.h>
#include <assert.h>

using namespace std;

#define NUM_SMS 110
// must be power of two
#define BLOCK_SIZE 256
// must be power of two
#define NUM_THREADS_PER_SM 2048
#define NUM_BLOCKS_PER_SM (NUM_THREADS_PER_SM / BLOCK_SIZE)
#define NUM_BLOCKS (NUM_SMS * NUM_BLOCKS_PER_SM)
#define NUM_THREADS (NUM_THREADS_PER_SM * NUM_SMS)

#define MIN(x, y) (((x) < (y)) ? (x) : (y))
#define MAX(x, y) (((x) > (y)) ? (x) : (y))

#define BATCH_SIZE 4
#define LG_BATCH_SIZE 2
#define LG_BATCH_SIZE_PLUS_ONE 3
#define WORD_SIZE 32
#define LG_WORD_SIZE 5
#define NUM_WIDTHS 16
#define LG_NUM_WIDTHS 4

typedef uint8_t uint1_t;

typedef struct {
  uint1_t fixed_cheaper;
  uint8_t cost; // may not be large enough at larger batch size
} cost_info;

__device__ uint8_t bit_length(uint32_t word) {
  return word == 0 ? 1 : WORD_SIZE - __clz(word);
}

__device__ uint32_t bit_select(uint32_t word, uint32_t upper, uint32_t lower) {
  uint32_t num_bits = upper - lower + 1;
  return (word >> lower) & ((1L << num_bits) - 1);
}

__device__ cost_info compute_cost(uint8_t width, uint8_t bit_count[3]) {
  uint8_t num_exceptions = BATCH_SIZE - bit_count[0];
  uint8_t fixed_cost = LG_WORD_SIZE + num_exceptions * bit_count[1];
  uint8_t varint_cost = bit_count[2]; // will be 0 if there are no exceptions
  uint8_t common_exception_cost = (num_exceptions > 0 ? 1 : 0) + num_exceptions * MAX(LG_BATCH_SIZE, 1);
  return (cost_info) {fixed_cost <= varint_cost, width * bit_count[0] +
    common_exception_cost + (fixed_cost <= varint_cost ? fixed_cost : varint_cost)};
}

__global__ void run(uint32_t *input_full, uint32_t input_count, uint32_t *output_full, uint32_t *output_count) {
  uint64_t index = blockIdx.x * blockDim.x + threadIdx.x;
  uint32_t *input_buf = input_full + index * input_count;
  uint32_t *output_buf = output_full + index * 2 * input_count;

  uint32_t input_idx = 0;
  uint32_t output_buf_idx = 0;
  uint32_t out_buf = 0;
  uint8_t out_buf_bits = 0;
  uint8_t bits_to_varint_len[33];
  uint8_t bit_widths[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 16, 20, 32};
  uint8_t bit_counts[NUM_WIDTHS][3]; // uint8_t may not be large enough if we have a larger batch size
  uint8_t words_consumed = 0;
  uint32_t buffer[BATCH_SIZE];

  for (int i = 1; i < 33; i++) {
    bits_to_varint_len[i] = (i + 7 - 1) / 7 * 8;
  }
  for (int i = 0; i < NUM_WIDTHS; i++) {
    bit_counts[i][0] = 0;
    bit_counts[i][1] = 0;
    bit_counts[i][2] = 0;
  }

  #define BUF_SIZE 256
  uint32_t input[BUF_SIZE];
  uint32_t output[BUF_SIZE * 2];
  for (uint32_t ii = input_idx; ii < input_count; ii += BUF_SIZE) {
    for (uint32_t i = ii; i < MIN(input_count, ii + BUF_SIZE); i++) {
      input[i - ii] = input_buf[i];
    }
    uint32_t output_idx = 0;
    for (uint32_t i = 0; i < MIN(input_count - ii, BUF_SIZE); i++) {
      #define ADD_TO_OUTPUT(w0, b0) do {\
                uint32_t word = w0;\
                uint8_t num_bits = b0;\
                if (num_bits + out_buf_bits >= 32) {\
                  output[output_idx++] = ((word & ((1L << (32 - out_buf_bits)) - 1)) << out_buf_bits) | out_buf;\
                  out_buf = word >> (32 - out_buf_bits);\
                  out_buf_bits = num_bits + out_buf_bits - 32;\
                } else {\
                  out_buf = (word << out_buf_bits) | out_buf;\
                  out_buf_bits += num_bits;\
                }\
              } while (0)
      // could replace this with the direct computatation instead of table lookup
      #define VARINT_LEN(b0) bits_to_varint_len[b0]

      uint8_t cur_bit_length = bit_length(input[i]);
      for (int j = 0; j < NUM_WIDTHS; j++) {
        if (cur_bit_length <= bit_widths[j]) {
          bit_counts[j][0] += 1;
        } else {
          if (cur_bit_length > bit_counts[j][1]) {
            bit_counts[j][1] = cur_bit_length;
          }
          bit_counts[j][2] += VARINT_LEN(cur_bit_length);
        }
      }
      buffer[words_consumed++] = input[i];
      if (words_consumed == BATCH_SIZE) {
        uint8_t min_width_idx = 0;
        cost_info min_cost = {0, 255}; // assumes that 255 is an unreachable cost value
        for (int j = 0; j < NUM_WIDTHS; j++) {
          cost_info cur_cost = compute_cost(bit_widths[j], bit_counts[j]);
          if (cur_cost.cost < min_cost.cost) {
            min_width_idx = j;
            min_cost = cur_cost;
          }
        }
        ADD_TO_OUTPUT(min_width_idx, MAX(LG_NUM_WIDTHS, 1));
        ADD_TO_OUTPUT(BATCH_SIZE - bit_counts[min_width_idx][0], LG_BATCH_SIZE_PLUS_ONE);
        for (int j = 0; j < BATCH_SIZE; j++) {
          if (bit_length(buffer[j]) <= bit_widths[min_width_idx]) {
            ADD_TO_OUTPUT(buffer[j], bit_widths[min_width_idx]);
          }
        }
        if (bit_counts[min_width_idx][0] < BATCH_SIZE) {
          if (min_cost.fixed_cheaper) {
            // fixed exceptions
            ADD_TO_OUTPUT(0, 1);
            ADD_TO_OUTPUT(bit_counts[min_width_idx][1] - 1, LG_WORD_SIZE);
          } else {
            // varint exceptions
            ADD_TO_OUTPUT(1, 1);
          }
          for (int j = 0; j < BATCH_SIZE; j++) {
            uint8_t word_bit_length = bit_length(buffer[j]);
            if (word_bit_length > bit_widths[min_width_idx]) {
              ADD_TO_OUTPUT(j, MAX(LG_BATCH_SIZE, 1));
              if (min_cost.fixed_cheaper) { // fixed
                ADD_TO_OUTPUT(buffer[j], bit_counts[min_width_idx][1]);
              } else {
                for (int k = 0; k < word_bit_length; k += 7) {
                  if (k + 7 < word_bit_length) {
                    ADD_TO_OUTPUT(bit_select(buffer[j], k + 6, k) | (1 << 7), 8);
                  } else {
                    ADD_TO_OUTPUT(bit_select(buffer[j], word_bit_length - 1, k), 8);
                  }
                }
              }
            }
          }
        }
        for (int j = 0; j < NUM_WIDTHS; j++) {
          bit_counts[j][0] = 0;
          bit_counts[j][1] = 0;
          bit_counts[j][2] = 0;
        }
        words_consumed = 0;
      }
    }
    for (uint32_t i = 0; i < output_idx; i++) {
      output_buf[output_buf_idx + i] = output[i];
    }
    output_buf_idx += output_idx;
  }
  if (out_buf_bits > 0) {
    output_buf[output_buf_idx++] = out_buf;
  }
  *(output_count + index) = output_buf_idx;
}

int main(int argc, char **argv) {
  uint32_t GLOBAL_CHARS = 10000000;
  uint32_t CHARS = atoi(argv[1]);

  // extracts "ad_id" and "ad_type"
  uint8_t seq_confs[] = {1, 34, 2, 97, 3, 100, 4, 95, 5, 105, 6, 100, 200, 34, 8, 121, 9, 112, 10, 101, 200, 34};
  uint8_t split_confs[] = {4, 7, 116};

  ifstream infile("kafka-json.txt");
  string line;

  uint32_t conf_size = sizeof(seq_confs) + sizeof(split_confs);
  uint32_t input_buf_size = conf_size + GLOBAL_CHARS;
  uint8_t *input_buf = new uint8_t[input_buf_size];
  uint32_t global_chars = 0;
  uint64_t chars = 0;
  memcpy(input_buf + global_chars, seq_confs, sizeof(seq_confs));
  global_chars += sizeof(seq_confs);
  memcpy(input_buf + global_chars, split_confs, sizeof(split_confs));
  global_chars += sizeof(split_confs);
  while (getline(infile, line)) {
    if (chars == 0 && global_chars + line.length() > conf_size + CHARS) {
      chars = global_chars;
    }
    if (global_chars + line.length() > input_buf_size) {
      break;
    }
    memcpy(input_buf + global_chars, line.c_str(), line.length());
    global_chars += line.length();
  }

  chars = chars / 4 * 4;
  uint8_t *combined_input = new uint8_t[chars * NUM_THREADS];
  for (uint64_t i = 0; i < NUM_THREADS; i++) {
    memcpy(combined_input + i * chars, input_buf + i * 10, chars);
    uint32_t mask = (1L << ((i + 32) % 33)) - 1; // start with full mask at i = 0 to match CPU version
    uint32_t *slice = (uint32_t *)(combined_input + i * chars);
    for (uint32_t j = 0; j < chars / 4; j++) {
      slice[j] = slice[j] & mask;
    }
  }

  uint8_t *output_buf = new uint8_t[chars * 2];
  uint32_t output_count;

  uint8_t *output_dev, *input_dev;
  uint32_t *output_count_dev;
  cudaSetDevice(0);
  assert(cudaMalloc((void **) &output_dev, chars * 2 * NUM_THREADS) == cudaSuccess);
  assert(cudaMalloc((void **) &input_dev, chars * NUM_THREADS) == cudaSuccess);
  assert(cudaMalloc((void **) &output_count_dev, sizeof(uint32_t) * NUM_THREADS) == cudaSuccess);

  cudaMemcpy(input_dev, combined_input, chars * NUM_THREADS, cudaMemcpyHostToDevice);

  struct timeval start, end, diff;
  gettimeofday(&start, 0);
  run<<<NUM_BLOCKS, BLOCK_SIZE>>>((uint32_t *)input_dev, chars / 4, (uint32_t *)output_dev, output_count_dev);
  cudaThreadSynchronize();
  gettimeofday(&end, 0);
  timersub(&end, &start, &diff);
  cudaMemcpy(&output_count, output_count_dev, sizeof(uint32_t), cudaMemcpyDeviceToHost);
  cudaMemcpy(output_buf, output_dev, output_count / 4, cudaMemcpyDeviceToHost);
  double secs = diff.tv_sec + diff.tv_usec / 1000000.0;
  printf("%.2f MB/s, %d input tokens, %d output tokens, random output byte: %d\n",
    (chars * NUM_THREADS) / 1000000.0 / secs, (int) chars / 4, output_count,
    output_count == 0 ? 0 : output_buf[rand() % (output_count * 4)]);
  return 0;
}
