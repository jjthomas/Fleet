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

#define NUM_HASHES 8
#define NUM_ITEMS 3072
#define NUM_BLOOM_BYTES 4096
#define ITEM_BYTES 100

typedef uint8_t uint1_t;

__global__ void run(uint8_t *input_full, uint32_t input_count, uint8_t *output_full, uint32_t *output_count) {
  uint64_t index = blockIdx.x * blockDim.x + threadIdx.x;
  uint8_t *input_buf = input_full + index * input_count;
  uint8_t *output_buf = output_full + index * input_count;

  uint32_t input_idx = 0;
  uint32_t output_buf_idx = 0;

  uint32_t hash_seeds[NUM_HASHES] = {0, 1, 2, 3, 4, 5, 6, 7};
  uint32_t hashes[NUM_HASHES] = {0, 1, 2, 3, 4, 5, 6, 7};
  uint8_t bloom[NUM_BLOOM_BYTES] = {0};
  uint32_t byte_counter = 0;
  uint32_t item_counter = 0;

  #define BUF_SIZE 256
  uint8_t input[BUF_SIZE];
  for (uint32_t ii = input_idx; ii < input_count; ii += BUF_SIZE) {
    for (uint32_t i = ii; i < MIN(input_count, ii + BUF_SIZE); i++) {
      input[i - ii] = input_buf[i];
    }
    for (uint32_t i = 0; i < MIN(input_count - ii, BUF_SIZE); i++) {
      for (uint32_t j = 0; j < NUM_HASHES; j++) {
        hashes[j] += input[i];
        hashes[j] += hashes[j] << 10;
        hashes[j] ^= hashes[j] >> 6;
      }
      byte_counter++;
      if (byte_counter == ITEM_BYTES) {
        for (uint32_t j = 0; j < NUM_HASHES; j++) {
          hashes[j] += hashes[j] << 3;
          hashes[j] ^= hashes[j] >> 11;
          hashes[j] += hashes[j] << 15;
          uint32_t cell = (hashes[j] >> 3) & (NUM_BLOOM_BYTES - 1);
          uint32_t bit = hashes[j] & 7;
          bloom[cell] |= 1 << bit;
          hashes[j] = hash_seeds[j];
        }
        byte_counter = 0;
        item_counter++;
      }
      if (item_counter == NUM_ITEMS) {
        for (uint32_t j = 0; j < NUM_BLOOM_BYTES; j++) {
          output_buf[output_buf_idx++] = bloom[j];
          bloom[j] = 0;
        }
        item_counter = 0;
      }
    }
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
  }

  uint8_t *output_buf = new uint8_t[chars];
  uint32_t output_count;

  uint8_t *input_dev;
  uint8_t *output_dev;
  uint32_t *output_count_dev;
  cudaSetDevice(0);
  assert(cudaMalloc((void **) &output_dev, chars * NUM_THREADS) == cudaSuccess);
  assert(cudaMalloc((void **) &input_dev, chars * NUM_THREADS) == cudaSuccess);
  assert(cudaMalloc((void **) &output_count_dev, sizeof(uint32_t) * NUM_THREADS) == cudaSuccess);

  cudaMemcpy(input_dev, combined_input, chars * NUM_THREADS, cudaMemcpyHostToDevice);

  struct timeval start, end, diff;
  gettimeofday(&start, 0);
  run<<<NUM_BLOCKS, BLOCK_SIZE>>>(input_dev, chars, output_dev, output_count_dev);
  cudaThreadSynchronize();
  gettimeofday(&end, 0);
  timersub(&end, &start, &diff);
  cudaMemcpy(&output_count, output_count_dev, sizeof(uint32_t), cudaMemcpyDeviceToHost);
  cudaMemcpy(output_buf, output_dev, output_count, cudaMemcpyDeviceToHost);
  double secs = diff.tv_sec + diff.tv_usec / 1000000.0;
  printf("%.2f MB/s, %d input tokens, %d output tokens, random output byte: %d\n",
    (chars * NUM_THREADS) / 1000000.0 / secs, (int) chars, output_count,
    output_count == 0 ? 0 : output_buf[rand() % output_count]);
  return 0;
}
