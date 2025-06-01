#include "kiss-alloc.h"
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

void main() {
  printf("sizeof page header: 0x%lx\n", sizeof(kissPageHeader));
  const size_t n = 14;
  void *ps[n];
  for (int i = 0; i < n; ++i) {
    ps[i] = kissAlloc(512, 0);
    printf("%p\n", ps[i]);
  }
  for (int i = 0; i < 8; ++i) {
    kissFree(ps[i]);
  }
  printf("======\n");
  void* p = kissAlloc(512, 0);
  printf("%p\n", p);
  printf("%lx\n", (uintptr_t)p ^ (uintptr_t)ps[0]);

  kissPageHeader* header = (void*)((uintptr_t)ps[0] & ~(4096-1));
  header->prev->next = NULL;
  while (header != NULL) {
    void* foo = header->next;
    free(header);
    header = foo;
  }
}
