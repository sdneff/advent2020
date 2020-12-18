#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "utils.h"

int isnumeric(char *string) {
  size_t length = strlen(string);
  if (length == 0)
    return 0;

  size_t i = 0;
  for (; i < length; i++) {
      if (!isdigit(string[i])) {
        return 0;
      }
  }

  return 1;
}

int read_line_as_number(int max_line, FILE* stream) {
  char line[max_line];

  if (fgets(line, max_line, stream) == NULL) {
    return 0;
  } else {
    char *end;
    if ((end = strrchr(line, '\n')) != NULL) {
      *end = '\0';
    }
    return atoi(line);
  }
}

void split_line_as_numbers(int *ptr, int max_numbers, int max_line, FILE* stream) {
  char line[max_line];
  char *end;

  if (fgets(line, max_line, stream) != NULL) {
    if ((end = strrchr(line, '\n')) != NULL) {
      *end = '\0';
    }

    for (int i = 0; i < max_numbers; ++i) {
      char *val = strtok(i > 0 ? NULL : line, ",");

      if (val != NULL) {
        *ptr = isnumeric(val) ? atoi(val) : 0;
      } else {
        *ptr = 0;
      }

      ++ptr;
    }
  }
}

void load_travel_info(struct travel *info, char *filename) {
  FILE *fp;

  if ((fp = fopen(filename, "r")) != NULL) {
    info->time = read_line_as_number(MAX_LINE_LENGTH, fp);
    split_line_as_numbers(&info->schedule[0], MAX_BUS_COUNT, MAX_LINE_LENGTH, fp);
  } else {
    printf("cannot open file %s\n",  filename);
  }

  fclose(fp);
}
