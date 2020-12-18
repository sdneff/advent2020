#include <stdio.h>

#include "utils.h"

int get_wait(struct travel* travel_info, int bus_index) {
  int time = travel_info->time;
  int bus = travel_info->schedule[bus_index];
  return bus - (time % bus);
}

int main() {
  struct travel travel_info;

  load_travel_info(&travel_info, "schedule.txt");

  int next_bus = -1;
  int next_bus_wait = -1;

  for (int i = 0; i < MAX_BUS_COUNT; ++i) {
    int bus_id = travel_info.schedule[i];
    if (bus_id > 0) { // ignore "0" bus ids
      int wait = get_wait(&travel_info, i);
      if (next_bus < 0 || wait < next_bus_wait) {
        next_bus = bus_id;
        next_bus_wait = wait;
      }
    }
  }

  printf("next available bus: %d; wait: %d; bus*wait: %d\n",
    next_bus, next_bus_wait, next_bus * next_bus_wait);

  return 0;
}
