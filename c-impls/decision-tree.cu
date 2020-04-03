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

#define BATCH_SIZE 10
#define TREE_DEPTH 4
#define TREE_SIZE ((1 << TREE_DEPTH) - 1)
#define NUM_TREES 60

typedef uint8_t uint1_t;

typedef struct {
  uint16_t field;
  uint16_t split;
} node;

__global__ void run(uint32_t *input_full, uint32_t input_count, uint32_t *output_full, uint32_t *output_count) {
  uint64_t index = blockIdx.x * blockDim.x + threadIdx.x;
  uint32_t *input_buf = input_full + index * input_count;
  uint32_t *output_buf = output_full + index * input_count;

  uint32_t input_idx = 0;
  uint32_t output_buf_idx = 0;
  uint16_t words_consumed = 0;
  uint16_t buffer[BATCH_SIZE];
  __shared__ node tree[TREE_SIZE * NUM_TREES];

  if (threadIdx.x == 0) {
    for (int i = 0; i < TREE_SIZE * NUM_TREES; i++) {
      tree[i].field = i % BATCH_SIZE;
      tree[i].split = 1 << 15; // midpoint of 16-bit values
    }
  }
  __syncthreads();


  #define BUF_SIZE 256
  uint32_t input[BUF_SIZE];
  uint32_t output[(BUF_SIZE + BATCH_SIZE - 1) / BATCH_SIZE]; // max outputs for BUF_SIZE inputs
  for (uint32_t ii = input_idx; ii < input_count; ii += BUF_SIZE) {
    for (uint32_t i = ii; i < MIN(input_count, ii + BUF_SIZE); i++) {
      input[i - ii] = input_buf[i];
    }
    uint32_t output_idx = 0;
    for (uint32_t i = 0; i < MIN(input_count - ii, BUF_SIZE); i++) {
      buffer[words_consumed++] = input[i];
      if (words_consumed == BATCH_SIZE) {
        uint32_t sum = 0;
        for (uint32_t tree_base = 0; tree_base < TREE_SIZE * NUM_TREES; tree_base += TREE_SIZE) {
          int tree_idx = 0;
          while (tree_idx < TREE_SIZE / 2) {
            node cur = tree[tree_base + tree_idx];
            tree_idx = buffer[cur.field] < cur.split ? tree_idx * 2 + 1 : tree_idx * 2 + 2;
          }
          sum += tree[tree_base + tree_idx].split;
        }
        output[output_idx++] = sum;
        words_consumed = 0;
      }
    }
    for (uint32_t i = 0; i < output_idx; i++) {
      output_buf[output_buf_idx + i] = output[i];
    }
    output_buf_idx += output_idx;
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
    uint32_t *slice = (uint32_t *)(combined_input + i * chars);
    for (uint32_t j = 0; j < chars / 4; j++) {
      slice[j] = slice[j] & ((1 << 16) - 1);
    }
  }

  uint8_t *output_buf = new uint8_t[chars];
  uint32_t output_count;

  uint8_t *output_dev, *input_dev;
  uint32_t *output_count_dev;
  cudaSetDevice(0);
  assert(cudaMalloc((void **) &output_dev, chars * NUM_THREADS) == cudaSuccess);
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
  printf("%.2f Mops/s\n", ((chars * NUM_THREADS) + (chars * NUM_THREADS) / BATCH_SIZE * TREE_DEPTH * NUM_TREES) / 1000000.0 / secs);
  printf("%.2f MB/s, %d input tokens, %d output tokens, random output byte: %d\n",
    (chars * NUM_THREADS) / 1000000.0 / secs, (int) chars / 4, output_count,
    output_count == 0 ? 0 : output_buf[rand() % (output_count * 4)]);
  return 0;
}
