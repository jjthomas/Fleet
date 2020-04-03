#include <stdint.h>
#include <sys/time.h>
#include <fstream>
#include <stdlib.h>
#include <string.h>

using namespace std;

#define MAX_DEPTH 5
#define MAX_FIELD_CHARS 200
#define MAX_FIELDS 10

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

void run(uint8_t *input, uint8_t num_seq_confs, uint8_t num_split_confs, uint32_t input_count, uint8_t *output,
  uint32_t *output_count) {

  uint32_t input_idx = 0;
  uint32_t output_idx = 0;
  state parse_state = EXP_VAL;
  uint8_t match_state = 0; // caps number of chars in fields to match at ~254
  uint1_t in_string_value = 0;
  uint8_t last_char = ' ';
  uint8_t nest_depth = 0;

  uint8_t state_stack[MAX_DEPTH];
  uint8_t stack_ptr = 0;
  seq_entry seq_trans[MAX_FIELD_CHARS];
  split_entry split_trans[MAX_FIELDS];

  #define IS_WHITESPACE(c) ((c) == ' ' || (c) == '\n' || (c) == '\t')
  #define POP_STATE_STACK do {\
            if (match_state == MAX_FIELD_CHARS) {\
              output[output_idx++] = ',';\
            }\
            match_state = state_stack[--stack_ptr];\
          } while (0)


  // match_states are exactly 8 bits only when MAX_FIELD_CHARS is >= 127
  for (uint8_t i = 0; i < num_seq_confs; i++) {
    seq_trans[i] = (seq_entry){input[input_idx], input[input_idx + 1]};
    input_idx += 2;
  }
  for (uint8_t i = 0; i < num_split_confs; i++) {
    split_trans[i] = (split_entry){input[input_idx], input[input_idx + 1], input[input_idx + 2]};
    input_idx += 3;
  }
  for (uint8_t i = num_split_confs; i < MAX_FIELDS; i++) {
    split_trans[i] = (split_entry){0, 0, 0}; // need to zero-initialize since all entries checked below
  }
  for (uint32_t i = input_idx; i < input_count; i++) {
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
  *output_count = output_idx;
}

int main(int argc, char **argv) {
  uint32_t CHARS = atoi(argv[1]);
  uint32_t NUM_THREADS = atoi(argv[2]);

  // extracts "ad_id" and "ad_type"
  uint8_t seq_confs[] = {1, 34, 2, 97, 3, 100, 4, 95, 5, 105, 6, 100, 200, 34, 8, 121, 9, 112, 10, 101, 200, 34};
  uint8_t split_confs[] = {4, 7, 116};

  ifstream infile("kafka-json.txt");
  string line;

  uint32_t input_buf_size = sizeof(seq_confs) + sizeof(split_confs) + CHARS;
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
  for (uint32_t i = 1; i < NUM_THREADS; i++) {
    memcpy(input_buf + i * input_buf_size, input_buf, input_buf_size);
  }
  uint8_t *output_buf = new uint8_t[input_buf_size * NUM_THREADS];
  uint32_t *output_count = new uint32_t[NUM_THREADS];

  struct timeval start, end, diff;
  gettimeofday(&start, 0);
  #pragma omp parallel for
  for (uint32_t i = 0; i < NUM_THREADS; i++) {
    run(input_buf + input_buf_size * i, sizeof(seq_confs) / 2, sizeof(split_confs) / 3, chars,
      output_buf + input_buf_size * i, output_count + i);
  }
  gettimeofday(&end, 0);
  timersub(&end, &start, &diff);
  double secs = diff.tv_sec + diff.tv_usec / 1000000.0;
  printf("%.2f MB/s, %d output tokens, random output byte: %d\n", (chars * NUM_THREADS) / 1000000.0 / secs,
    output_count[0], output_count[0] == 0 ? 0 : output_buf[rand() % output_count[0]]);
  return 0;
}
