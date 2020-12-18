#include <stdio.h>

#include "utils.h"

typedef unsigned long long bigInt;

// solve for smallest positive x in:
// ax ≡ c mod b
bigInt euclid(bigInt a, bigInt b, bigInt c) {
  c %= b;

  // since we encounter some big numbers, we have 2 versions of the algorithm,
  // depending on which is bigger (a or b), we'll use that one to increment
  if (a > b) {
    // direct method:
    // try a * 1, a * 2, ... until a * x % b == c
    bigInt x = 1;

    bigInt curr = a;
    while (curr % b != c) {
      ++x;
      curr += a;
      curr %= b;
    }

    return x;
  } else {
    // indirect method:
    // try (b * 0,) b * 1, b * 2, ... until (b * m + c) % a == 0

    bigInt m = c == 0 ? 1 : 0;

    while ((m * b + c) % a != 0) {
      ++m;
    }

    return (m * b + c) / a; // solve for x
  }
}

// least common multiple
bigInt lcm(bigInt a, bigInt b) {
  return euclid(a, b, 0) * a;
}

int main() {
  struct travel travel_info;

  load_travel_info(&travel_info, "schedule.txt");

  bigInt time = 0; // current timestamp of first bus
  bigInt period = 0; // current repetition period

  for (bigInt i = 0; i < MAX_BUS_COUNT; ++i) {
    bigInt bus_id = travel_info.schedule[i];
    if (bus_id > 0) { // ignore "0" bus ids
      if (time == 0) {
        period = bus_id;
        time = bus_id + i;
      } else {
        bigInt mul = euclid(bus_id, period, time + i); // mul=# of cycles current bus needs
        time = mul * bus_id - i; // time stays relative to first bus
        period = lcm(bus_id, period);
      }
    }
  }

  printf("time: %llu\n", time);

  return 0;
}
