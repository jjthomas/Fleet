#include <stdint.h>
#include <sys/time.h>
#include <fstream>
#include <stdlib.h>
#include <string.h>
#include <immintrin.h>

using namespace std;

#define MIN(x, y) (((x) < (y)) ? (x) : (y))
#define MAX(x, y) (((x) > (y)) ? (x) : (y))

#define NUM_HASHES 8
#define NUM_ITEMS 3072
#define NUM_BLOOM_BYTES 4096
#define ITEM_BYTES 100

typedef uint8_t uint1_t;

void run(uint8_t *input, uint32_t input_count, uint8_t *output, uint32_t *output_count) {
  uint32_t input_idx = 0;
  uint32_t output_idx = 0;

  __m256i hash_seeds = _mm256_set_epi32(0, 1, 2, 3, 4, 5, 6, 7);
  __m256i hashes = hash_seeds;
  uint8_t bloom[NUM_BLOOM_BYTES] = {0};
  uint32_t byte_counter = 0;
  uint32_t item_counter = 0;

  for (uint32_t i = input_idx; i < input_count; i++) {
    __m256i input_broadcast = _mm256_set1_epi32(input[i]);
    hashes = _mm256_add_epi32(hashes, input_broadcast);
    hashes = _mm256_add_epi32(hashes, _mm256_sll_epi32(hashes, _mm_cvtsi32_si128(10)));
    hashes = _mm256_xor_si256(hashes, _mm256_srl_epi32(hashes, _mm_cvtsi32_si128(6)));
    byte_counter++;
    if (byte_counter == ITEM_BYTES) {
      hashes = _mm256_add_epi32(hashes, _mm256_sll_epi32(hashes, _mm_cvtsi32_si128(3)));
      hashes = _mm256_xor_si256(hashes, _mm256_srl_epi32(hashes, _mm_cvtsi32_si128(11)));
      hashes = _mm256_add_epi32(hashes, _mm256_sll_epi32(hashes, _mm_cvtsi32_si128(15)));
      __m256i cells = _mm256_and_si256(_mm256_srl_epi32(hashes, _mm_cvtsi32_si128(3)),
        _mm256_set1_epi32(NUM_BLOOM_BYTES - 1));
      __m256i vals = _mm256_sllv_epi32(_mm256_set1_epi32(1), _mm256_and_si256(hashes, _mm256_set1_epi32(7)));
      for (uint32_t j = 0; j < NUM_HASHES; j++) {
        bloom[_mm256_extract_epi32(cells, j)] |= _mm256_extract_epi32(vals, j);
      }
      hashes = hash_seeds;
      byte_counter = 0;
      item_counter++;
    }
    if (item_counter == NUM_ITEMS) {
      for (uint32_t j = 0; j < NUM_BLOOM_BYTES; j++) {
        output[output_idx++] = bloom[j];
        bloom[j] = 0;
      }
      item_counter = 0;
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
  uint8_t *output_buf = new uint8_t[chars * NUM_THREADS];
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
