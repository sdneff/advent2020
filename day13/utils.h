#ifndef DAY_13_UTILS
#define DAY_13_UTILS

#define MAX_LINE_LENGTH 200
#define MAX_BUS_COUNT 100

struct travel {
  int time;
  int schedule[MAX_BUS_COUNT];
};

void load_travel_info(struct travel *info, char *filename);

#endif
