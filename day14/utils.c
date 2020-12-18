#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"

// memory bank access

void store_value(struct mem_addr *mem_start, bigInt addr, bigInt value) {
  while (mem_start != NULL && mem_start->address != 0) {
    if (mem_start->address == addr) break;
    ++mem_start;
  }
  mem_start->address = addr;
  mem_start->value = value;
}

bigInt memory_total(struct mem_addr *mem_start) {
  bigInt value_sum = 0;

  while (mem_start != NULL && mem_start->address != 0) {
    value_sum += mem_start->value;
    ++mem_start;
  }

  return value_sum;
}

// parsing

int read_line_to_buffer(char *buf, int max_line, FILE* stream) {
  if (fgets(buf, max_line, stream) != NULL) {
    char *end;
    if ((end = strrchr(buf, '\n')) != NULL) {
      *end = '\0';
    }
    return 1;
  }

  return 0;
}

int starts_with(const char *pre, const char *str)
{
  return strncmp(pre, str, strlen(pre)) == 0;
}

struct mask parse_mask_instruction(char *s) {
  struct mask m;

  while (*s != '\0') {
    if (*s == '1' || *s == '0' || *s == 'X') { // this way we can parse whole line
      m.ones <<= 1;
      m.zeros <<= 1;
      m.wildcards <<= 1;
      if (*s == '1') ++m.ones;
      if (*s == '0') ++m.zeros;
      if (*s == 'X') ++m.wildcards;
    }
    ++s;
  }

  return m;
}

void parse_mem_instruction(char *s, bigInt *addr, bigInt *value) {
  strtok(s, "["); // discard "mem["
  *addr = strtoull(strtok(NULL, "]"), NULL, 10); // read until "]""
  strtok(NULL, "="); // dicard "="
  *value = strtoull(strtok(NULL, "\0"), NULL, 10); // read until end (space gets ignored)
}

// inspection

void write_binary_string(bigInt input, char *s) {
  int len = 36;
  s += len;
  *s = '\0';
  bigInt curr = input;
  for (int i = 0; i < len; ++i) {
    --s;
    *s = curr % 2 == 1 ? '1' : '0';
    curr >>= 1;
  }
}

void inspect_mask(struct mask *bit_mask) {
  printf("INSPECTING MASK:\n");

  char buf[36];

  write_binary_string(bit_mask->ones, &buf[0]);
  printf("       ones: %s\n", buf);

  write_binary_string(bit_mask->zeros, &buf[0]);
  printf("      zeros: %s\n", buf);

  write_binary_string(bit_mask->wildcards, &buf[0]);
  printf("  wildcards: %s\n", buf);
}

void inspect_memory(struct mem_addr *mem_start) {
  printf("INSPECTING MEMORY:\n");

  int len = 0;
  char buf[36];

  while (mem_start != NULL && mem_start->address != 0) {
    write_binary_string(mem_start->value, &buf[0]);
    printf("[%llu]%s\n", mem_start->address, buf);
    ++mem_start;
    ++len;
  }

  printf("COUNT: %d\n", len);
}
