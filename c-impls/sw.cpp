#include <stdint.h>
#include <sys/time.h>
#include <fstream>
#include <stdlib.h>
#include <string.h>

using namespace std;

#define MIN(x, y) (((x) < (y)) ? (x) : (y))
#define MAX(x, y) (((x) > (y)) ? (x) : (y))

#define NEEDLE "sponsored-search"
#define NEEDLE_SIZE (sizeof(NEEDLE) - 1)
#define THRESHOLD 4

typedef uint8_t uint1_t;

void run(uint8_t *input, uint32_t input_count, uint32_t *output, uint32_t *output_count) {
  uint32_t input_idx = 0;
  uint32_t output_idx = 0;
  uint8_t table_row[NEEDLE_SIZE] = {0};

  for (uint32_t i = input_idx; i < input_count; i++) {
    uint1_t at_threshold = 0;
    uint8_t running_row_max = 0;
    for (uint32_t j = 0; j < NEEDLE_SIZE; j++) {
      int8_t match_best = running_row_max + ((input[i] == NEEDLE[j]) ? 1 : -1);
      running_row_max = MAX(running_row_max - 1, table_row[j]);
      table_row[j] = MAX(0, MAX(match_best, table_row[j] - 1)); // best of (i, j) match or gap
      if (table_row[j] >= THRESHOLD) {
        at_threshold = 1;
      }
    }
    if (at_threshold) {
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
