#include <stddef.h>
#include <stdlib.h>
#include "kiss-alloc.h"

const size_t log2_pageSize = 12;

// DO NOT edit these values, they are all derived from `log2_pageSize`.
const size_t pageSize = 1 << log2_pageSize;
const size_t low_mask = pageSize - 1;
const size_t high_mask = ~low_mask;
const size_t largeObjThreshold = pageSize / 2;
// END

void* kissAllocPages(size_t nPages) {
  return aligned_alloc(pageSize, nPages * pageSize); // TODO use checked arithmetic
}
