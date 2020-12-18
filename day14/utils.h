#ifndef DAY_14_UTILS
#define DAY_14_UTILS

#define MAX_LINE_LENGTH 200

typedef unsigned long long bigInt;

struct mask {
  bigInt ones;
  bigInt zeros;
  bigInt wildcards;
};

struct mem_addr {
  bigInt address;
  bigInt value;
};

void store_value(struct mem_addr *, bigInt, bigInt);
bigInt memory_total(struct mem_addr *);

int read_line_to_buffer(char *, int, FILE*);
int starts_with(const char *, const char *);
struct mask parse_mask_instruction(char *);
void parse_mem_instruction(char *, bigInt *, bigInt *);

void write_binary_string(bigInt, char *);
void inspect_mask(struct mask *);
void inspect_memory(struct mem_addr *);

#endif
