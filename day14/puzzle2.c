#include <stdio.h>
#include <string.h>

#include "utils.h"

#define MEM_BUFFER_SIZE 100000

bigInt apply_mask(bigInt input, struct mask *bit_mask) {
  // if the bitmask bit is 0, the memory address bit is unchanged
  // if the bitmask bit is 1, the memory address bit is overwritten with 1
  // if the bitmask bit is X, the memory address bit is floating

  bigInt val = input;
  val |= bit_mask->ones; // force 1s
  // nothing for 0s
  // wildcards handled separately

  val %= (bigInt)1 << 36; // since we ~ zeros, we end up with junk above 36th bit

  return val;
}

void store_at_multi_locations(struct mem_addr *mem_start, bigInt *val, bigInt addr_val, bigInt addr_mask) {
  addr_val %= (bigInt)1 << 36;  // since we ~ zeros, we end up with junk above 36th bit
  addr_mask %= (bigInt)1 << 36; // since we ~ zeros, we end up with junk above 36th bit

  if (addr_mask == 0) {
    store_value(mem_start, addr_val, *val);
  } else {
    bigInt bit = 1;
    while ((addr_mask & bit) == 0) {
      bit <<= 1;
    }

    struct mem_addr *m1 = mem_start;
    struct mem_addr *m2 = mem_start;

    store_at_multi_locations(m1, val, addr_val & (~bit), addr_mask & (~bit));
    store_at_multi_locations(m2, val, addr_val | bit, addr_mask & (~bit));
  }
}

int main() {

  char filename[] = "program.txt";
  FILE *fp;
  char buf[MAX_LINE_LENGTH];

  struct mask bit_mask;
  struct mem_addr memory[MEM_BUFFER_SIZE] = {{0}};
  bigInt curr_addr = 0;
  bigInt curr_value = 0;

  char buf2[36];

  if ((fp = fopen(filename, "r")) != NULL) {
    while (read_line_to_buffer(buf, MAX_LINE_LENGTH, fp)) {
      if (starts_with("mask", buf)) {
        bit_mask = parse_mask_instruction(buf);
      } else if (starts_with("mem", buf)) {
        parse_mem_instruction(buf, &curr_addr, &curr_value);

        // split masked address on wildcards and store value
        store_at_multi_locations(&memory[0],
          &curr_value,
          apply_mask(curr_addr, &bit_mask),
          bit_mask.wildcards);
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
