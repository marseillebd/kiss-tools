// # This is a test file
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

// ## Properties
//
// Let's define the properties that can be specified in a `.x` file.
// This also includes how to parse and validate them.

enum key {
  BAD_KEY = 0,
  ENDIAN = 1,
  DIGIT = 2,
  WORD_SIZE = 4,
};
struct keyassoc {
  enum key key;
  char keyName[12];
};
struct keyassoc keynames[] = {
  {.key = ENDIAN, .keyName = "endian:"},
  {.key = DIGIT, .keyName = "digit:"},
  {.key = WORD_SIZE, .keyName = "wordSize:"},
};

static
enum key parseKey(char *s) {
  for (int i = 0; i < sizeof(keynames)/sizeof(struct keyassoc); ++i) {
    if (strncmp(s, keynames[i].keyName, 11) == 0) {
      return keynames[i].key;
    }
  }
  return BAD_KEY;
}

typedef struct properties {
  bool bigendian; // TODO use an enum
  int digit; // TODO use an enum
  int wordSize;
} properties;

// # Main

int main() {
  properties props = {
    .bigendian = true,
    .digit = 0,
    .wordSize = 0
  };

  char *line[2] = {
    " digit: octal\n",
    "endian: little   \n"
  };
  for (int i = 0; i < 2; ++i) {
    char keystr[12];
    char value[100];
    int matches = sscanf(line[i], "%11s%99s\n", keystr, value);
    enum key key = parseKey(keystr);
    printf("matches: %d\nkey: '%s' -> %d\nval: '%s'\n", matches, keystr, key, value);
  }

  return 0;
}

// ta-dah!
