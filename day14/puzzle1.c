#include <stdio.h>
#include <string.h>

#include "utils.h"

#define MEM_BUFFER_SIZE 500

bigInt apply_mask(bigInt input, struct mask *bit_mask) {
  // a 0 or 1 overwrites the corresponding bit in the value
  // while an X leaves the bit in the value unchanged

  bigInt val = input;
  val |= bit_mask->ones;     // force 1s
  val &= (~bit_mask->zeros); // force 0s
  // nothing with wildcards

  val %= (bigInt)1 << 36; // since we ~ zeros, we end up with junk above 36th bit

  return val;
}

int main() {

  char filename[] = "program.txt";
  FILE *fp;
  char buf[MAX_LINE_LENGTH];

  struct mask bit_mask;
  struct mem_addr memory[MEM_BUFFER_SIZE] = {{0}};
  bigInt curr_addr = 0;
  bigInt curr_value = 0;

  if ((fp = fopen(filename, "r")) != NULL) {
    while (read_line_to_buffer(buf, MAX_LINE_LENGTH, fp)) {
      if (starts_with("mask", buf)) {
        bit_mask = parse_mask_instruction(buf);
      } else if (starts_with("mem", buf)) {
        parse_mem_instruction(buf, &curr_addr, &curr_value);

        // apply bit mask and store in memory
        bigInt masked_value = apply_mask(curr_value, &bit_mask);
        store_value(&memory[0], curr_addr, masked_value);
      }
    }
  } else {
    printf("cannot open file %s\n",  filename);
  }

  fclose(fp);

  // inspect_memory(&memory[0]);

  bigInt value_sum = memory_total(&memory[0]);

  printf("memory bank total: %llu\n", value_sum);

  return 0;
}
