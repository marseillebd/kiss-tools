#include <stdio.h>
#include <stdlib.h>

int main() {
  printf("%s\n", __FILE__);
  /* FILE* fp = stdin; */
  /* FILE* fp = fopen(__FILE__, "rb"); */
  FILE* fp = fopen("test.fifo", "rb");
  fseek(fp, 0, SEEK_END);
  for (int err = ferror(fp); err != 0; ) { exit(err); }
  int sz = ftell(fp);
  fseek(fp, 0, SEEK_SET);
  for (int err = ferror(fp); err != 0; ) { exit(err); }
  printf("%d\n", sz);
}
