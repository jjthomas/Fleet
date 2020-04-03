#include <stdint.h>
#include <sys/time.h>
#include <fstream>
#include <stdlib.h>
#include <string.h>

using namespace std;

#define MIN(x, y) (((x) < (y)) ? (x) : (y))
#define MAX(x, y) (((x) > (y)) ? (x) : (y))

typedef uint8_t uint1_t;

void run(uint8_t *input, uint32_t input_count, uint32_t *output, uint32_t *output_count) {
  uint32_t input_idx = 0;
  uint32_t output_idx = 0;

  uint1_t s12 = 0;
  uint1_t s13 = 0;
  uint1_t s10 = 0;
  uint1_t s16 = 0;
  uint1_t s17 = 0;
  uint1_t s11 = 0;
  uint1_t s20 = 0;
  uint1_t s21 = 0;

  for (uint32_t i = input_idx; i < input_count; i++) {
    uint1_t o12 = (0 || (input[i] >= '0' && input[i] <= '9') || (input[i] >= 'a' && input[i] <= 'z') || input[i] == '-' || input[i] == '+') && s12;
    s12 = 1;
    uint1_t o13 = (0 || (input[i] >= '0' && input[i] <= '9') || (input[i] >= 'a' && input[i] <= 'z') || input[i] == '-' || input[i] == '+') && s13;
    uint1_t o14 = o13 || o12;
    s13 = o14;
    uint1_t o15 = o14;
    uint1_t o10 = (0 || input[i] == '@') && s10;
    s10 = o15;
    uint1_t o16 = (0 || (input[i] >= '0' && input[i] <= '9') || (input[i] >= 'a' && input[i] <= 'z') || input[i] == '-') && s16;
    s16 = o10;
    uint1_t o17 = (0 || (input[i] >= '0' && input[i] <= '9') || (input[i] >= 'a' && input[i] <= 'z') || input[i] == '-') && s17;
    uint1_t o18 = o17 || o16;
    s17 = o18;
    uint1_t o19 = o18;
    uint1_t o11 = (0 || input[i] == '.') && s11;
    s11 = o19;
    uint1_t o20 = (0 || (input[i] >= '0' && input[i] <= '9') || (input[i] >= 'a' && input[i] <= 'z') || input[i] == '-') && s20;
    s20 = o11;
    uint1_t o21 = (0 || (input[i] >= '0' && input[i] <= '9') || (input[i] >= 'a' && input[i] <= 'z') || input[i] == '-') && s21;
    uint1_t o22 = o21 || o20;
    s21 = o22;
    uint1_t o23 = o22;
    uint1_t o24 = o23;
    uint1_t o25 = o24;
    uint1_t o26 = o25;
    uint1_t o27 = o26;
    if (o27) {
      output[output_idx++] = i;
    }
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
  for (uint32_t i = 1; i < NUM_THREADS; i++) {
    memcpy(input_buf + i * chars, input_buf, chars);
  }
  uint32_t *output_buf = new uint32_t[chars * NUM_THREADS];
  uint32_t *output_count = new uint32_t[NUM_THREADS];

  struct timeval start, end, diff;
  gettimeofday(&start, 0);
  #pragma omp parallel for
  for (uint32_t i = 0; i < NUM_THREADS; i++) {
    run(input_buf + chars * i, chars, output_buf + chars * i, output_count + i);
  }
  gettimeofday(&end, 0);
  timersub(&end, &start, &diff);
  double secs = diff.tv_sec + diff.tv_usec / 1000000.0;
  printf("%.2f MB/s, %d input tokens, %d output tokens, random output word: %d\n",
    (chars * NUM_THREADS) / 1000000.0 / secs, chars,
    output_count[0], output_count[0] == 0 ? 0 : output_buf[rand() % output_count[0]]);
  return 0;
}
