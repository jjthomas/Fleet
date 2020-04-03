#include <stdint.h>
#include <sys/time.h>
#include <fstream>
#include <stdlib.h>
#include <string.h>

using namespace std;

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

static inline uint8_t bit_length(uint32_t word) {
  return word == 0 ? 1 : WORD_SIZE - __builtin_clz(word);
}

static inline uint32_t bit_select(uint32_t word, uint32_t upper, uint32_t lower) {
  uint32_t num_bits = upper - lower + 1;
  return (word >> lower) & ((1L << num_bits) - 1);
}

static inline cost_info compute_cost(uint8_t width, uint8_t bit_count[3]) {
  uint8_t num_exceptions = BATCH_SIZE - bit_count[0];
  uint8_t fixed_cost = LG_WORD_SIZE + num_exceptions * bit_count[1];
  uint8_t varint_cost = bit_count[2]; // will be 0 if there are no exceptions
  uint8_t common_exception_cost = (num_exceptions > 0 ? 1 : 0) + num_exceptions * MAX(LG_BATCH_SIZE, 1);
  return (cost_info) {fixed_cost <= varint_cost, width * bit_count[0] +
    common_exception_cost + (fixed_cost <= varint_cost ? fixed_cost : varint_cost)};
}

void run(uint32_t *input, uint32_t input_count, uint32_t *output, uint32_t *output_count) {
  uint32_t input_idx = 0;
  uint32_t output_idx = 0;
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

  for (uint32_t i = input_idx; i < input_count; i++) {
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
  if (out_buf_bits > 0) {
    output[output_idx++] = out_buf;
  }
  *output_count = output_idx;
}

int main(int argc, char **argv) {
  uint32_t CHARS = atoi(argv[1]);
  uint32_t NUM_THREADS = atoi(argv[2]);
  uint32_t BITS = atoi(argv[3]);

  // extracts "ad_id" and "ad_type"
  uint8_t seq_confs[] = {1, 34, 2, 97, 3, 100, 4, 95, 5, 105, 6, 100, 200, 34, 8, 121, 9, 112, 10, 101, 200, 34};
  uint8_t split_confs[] = {4, 7, 116};

  ifstream infile("kafka-json.txt");
  string line;

  uint32_t input_buf_size = (sizeof(seq_confs) + sizeof(split_confs) + CHARS + 4 - 1) / 4 * 4;
  uint8_t *input_buf = new uint8_t[input_buf_size * NUM_THREADS];
  uint32_t chars = 0;
  memcpy(input_buf + chars, seq_confs, sizeof(seq_confs));
  chars += sizeof(seq_confs);
  memcpy(input_buf + chars, split_confs, sizeof(split_confs));
  chars += sizeof(split_confs);
  while (getline(infile, line)) {
    if (chars + line.length() > input_buf_size) {
      break;
    }
    memcpy(input_buf + chars, line.c_str(), line.length());
    chars += line.length();
  }
  chars = chars / 4 * 4;

  uint32_t *input_buf32 = (uint32_t *)input_buf;
  for (uint32_t i = 0; i < chars / 4; i++) {
    input_buf32[i] = rand() & ((1 << BITS) - 1);
  }
  for (uint32_t i = 1; i < NUM_THREADS; i++) {
    memcpy(input_buf + i * chars, input_buf, chars);
  }
  uint8_t *output_buf = new uint8_t[chars * 2 * NUM_THREADS];
  uint32_t *output_count = new uint32_t[NUM_THREADS];

  struct timeval start, end, diff;
  gettimeofday(&start, 0);
  #pragma omp parallel for
  for (uint32_t i = 0; i < NUM_THREADS; i++) {
    run((uint32_t *)(input_buf + chars * i), chars / 4,
      (uint32_t *)(output_buf + chars * 2 * i), output_count + i);
  }
  gettimeofday(&end, 0);
  timersub(&end, &start, &diff);
  double secs = diff.tv_sec + diff.tv_usec / 1000000.0;
  printf("%.2f MB/s, %d input tokens, %d output tokens, random output byte: %d\n",
    (chars * NUM_THREADS) / 1000000.0 / secs, chars / 4,
    output_count[0], output_count[0] == 0 ? 0 : output_buf[rand() % (output_count[0] * 4)]);
  return 0;
}
