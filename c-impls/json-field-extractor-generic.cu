#include <stdint.h>
#include <sys/time.h>
#include <fstream>
#include <stdlib.h>
#include <string.h>
#include <cuda.h>
#include <assert.h>

using namespace std;

#define MAX_DEPTH 5
#define MAX_FIELD_CHARS 200
#define MAX_FIELDS 10

#define NUM_SMS 110
// must be power of two
#define BLOCK_SIZE 256
// must be power of two
#define NUM_THREADS_PER_SM 2048
#define NUM_BLOCKS_PER_SM (NUM_THREADS_PER_SM / BLOCK_SIZE)
#define NUM_BLOCKS (NUM_SMS * NUM_BLOCKS_PER_SM)
#define NUM_THREADS (NUM_THREADS_PER_SM * NUM_SMS)

#define MIN(x, y) (((x) < (y)) ? (x) : (y))

typedef enum {EXP_KEY, IN_KEY, EXP_COL, EXP_VAL, IN_VAL, EXP_COM} state;

typedef uint8_t uint1_t;

typedef struct __attribute__((packed)) {
  uint8_t expected_match_state;
  uint8_t next_match_state;
  uint8_t expected_input;
} split_entry;

typedef struct __attribute__((packed)) {
  uint8_t next_match_state;
  uint8_t expected_input;
} seq_entry;

__global__ void run(uint8_t *input_full, uint8_t num_seq_confs, uint8_t num_split_confs, uint32_t input_count,
  uint8_t *output_full, uint32_t *output_count) {

  uint64_t index = blockIdx.x * blockDim.x + threadIdx.x;
  uint8_t *input_buf = input_full + index * input_count;
  uint8_t *output_buf = output_full + index * input_count;

  uint32_t input_idx = 0;
  uint32_t output_buf_idx = 0;
  state parse_state = EXP_VAL;
  uint8_t match_state = 0; // caps number of chars in fields to match at ~254
  uint1_t in_string_value = 0;
  uint8_t last_char = ' ';
  uint8_t nest_depth = 0;

  uint8_t state_stack[MAX_DEPTH];
  uint8_t stack_ptr = 0;
  __shared__ seq_entry seq_trans[MAX_FIELD_CHARS];
  __shared__ split_entry split_trans[MAX_FIELDS];

  #define IS_WHITESPACE(c) ((c) == ' ' || (c) == '\n' || (c) == '\t')
  #define POP_STATE_STACK do {\
            if (match_state == MAX_FIELD_CHARS) {\
              output[output_idx++] = ',';\
            }\
            match_state = state_stack[--stack_ptr];\
          } while (0)


  if (threadIdx.x == 0) {
    // match_states are exactly 8 bits only when MAX_FIELD_CHARS is >= 127
    for (uint8_t i = 0; i < num_seq_confs; i++) {
      seq_trans[i] = (seq_entry){input_buf[input_idx], input_buf[input_idx + 1]};
      input_idx += 2;
    }
    for (uint8_t i = 0; i < num_split_confs; i++) {
      split_trans[i] = (split_entry){input_buf[input_idx], input_buf[input_idx + 1], input_buf[input_idx + 2]};
      input_idx += 3;
    }
    for (uint8_t i = num_split_confs; i < MAX_FIELDS; i++) {
      split_trans[i] = (split_entry){0, 0, 0}; // need to zero-initialize since all entries checked below
    }
  }
  __syncthreads();

  #define BUF_SIZE 512
  uint8_t input[BUF_SIZE];
  uint8_t output[BUF_SIZE]; // it is possible that output is larger than input, but with fairly large BUF_SIZE
  // on the kafka_json dataset it shouldn't happen
  for (uint32_t ii = input_idx; ii < input_count; ii += BUF_SIZE) {
    for (uint32_t i = ii; i < MIN(input_count, ii + BUF_SIZE); i++) {
      input[i - ii] = input_buf[i];
    }
    uint32_t output_idx = 0;
    for (uint32_t i = 0; i < MIN(input_count - ii, BUF_SIZE); i++) {
      #define EMIT_CUR_TOKEN do {\
                if (match_state == MAX_FIELD_CHARS) {\
                  output[output_idx++] = input[i];\
                }\
              } while (0)

      // only need a next variable for parse_state because the other two state variables used in if conditions
      // (in_string_value and nest_depth) are not at risk of an erroneous read after write
      state next_parse_state = parse_state;
      if (parse_state == EXP_VAL) {
        if (input[i] == '{') {
          next_parse_state = EXP_KEY;
          nest_depth++;
        } else if (nest_depth != 0 && !IS_WHITESPACE(input[i])) {
          // at nest_depth of 0 we only accept new records
          EMIT_CUR_TOKEN;
          next_parse_state = IN_VAL;
          in_string_value = input[i] == '"';
        }
      }
      if (parse_state == IN_VAL) {
        if (in_string_value) {
          EMIT_CUR_TOKEN;
          if (input[i] == '"' && last_char != '\\') {
            in_string_value = 0;
          }
        } else if (input[i] != '}') {
          if (input[i] == ',') {
            next_parse_state = EXP_KEY;
            POP_STATE_STACK;
          } else {
            EMIT_CUR_TOKEN;
          }
        }
      }
      if (input[i] == ',' && parse_state == EXP_COM) {
        next_parse_state = EXP_KEY;
        POP_STATE_STACK;
      }
      if (input[i] == '}' &&
        (parse_state == EXP_KEY || parse_state == EXP_COM || (parse_state == IN_VAL && !in_string_value))) {
        if (parse_state == EXP_COM || parse_state == IN_VAL) {
          POP_STATE_STACK;
        }
        if (nest_depth == 1) {
          output[output_idx++] = '/'; // record separator
          next_parse_state = EXP_VAL;
        } else {
          next_parse_state = EXP_COM;
        }
        nest_depth--;
      }
      if (input[i] == '"' && parse_state == IN_KEY) {
        next_parse_state = EXP_COL;
      }
      if (input[i] == ':' && parse_state == EXP_COL) {
        next_parse_state = EXP_VAL;
      }
      uint1_t entering_key = input[i] == '"' && parse_state == EXP_KEY;
      if (entering_key) {
        next_parse_state = IN_KEY;
        state_stack[stack_ptr++] = match_state;
      }
      if ((parse_state == IN_KEY || entering_key) && match_state != MAX_FIELD_CHARS &&
        (match_state != 0 || nest_depth == 1)) {
        // only allow match to start at top level
        if (input[i] == seq_trans[match_state].expected_input) {
          if (seq_trans[match_state].next_match_state == MAX_FIELD_CHARS) {
            output[output_idx++] = match_state;
          }
          match_state = seq_trans[match_state].next_match_state;
        } else {
          uint8_t next_match_state = 0;
          for (uint8_t j = 0; j < MAX_FIELDS; j++) {
            if (match_state == split_trans[j].expected_match_state && input[i] == split_trans[j].expected_input) {
              if (split_trans[j].next_match_state == MAX_FIELD_CHARS) {
                output[output_idx++] = match_state;
              }
              next_match_state = split_trans[j].next_match_state;
            }
          }
          match_state = next_match_state;
        }
      }

      last_char = input[i];
      parse_state = next_parse_state;
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

  uint8_t *combined_input = new uint8_t[chars * NUM_THREADS];
  for (uint64_t i = 0; i < NUM_THREADS; i++) {
    memcpy(combined_input + i * chars, input_buf, conf_size);
    memcpy(combined_input + i * chars + conf_size, input_buf + conf_size + i * 10, chars - conf_size);
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
  run<<<NUM_BLOCKS, BLOCK_SIZE>>>(input_dev, sizeof(seq_confs) / 2, sizeof(split_confs) / 3, chars, output_dev,
    output_count_dev);
  cudaThreadSynchronize();
  gettimeofday(&end, 0);
  timersub(&end, &start, &diff);
  cudaMemcpy(&output_count, output_count_dev, sizeof(uint32_t), cudaMemcpyDeviceToHost);
  cudaMemcpy(output_buf, output_dev, output_count, cudaMemcpyDeviceToHost);
  double secs = diff.tv_sec + diff.tv_usec / 1000000.0;
  printf("%.2f MB/s, %d output tokens, random output byte: %d\n", (chars * NUM_THREADS) / 1000000.0 / secs, output_count,
    output_count == 0 ? 0 : output_buf[rand() % output_count]);
  return 0;
}
