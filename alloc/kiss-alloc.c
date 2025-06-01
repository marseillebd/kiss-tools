#include "kiss-alloc.h"

#include <assert.h>
#include <stdint.h>

// BEGIN platform-specific values
extern const size_t log2_pageSize;
extern const size_t pageSize;
extern const size_t low_mask;
extern const size_t high_mask;
extern const size_t largeObjThreshold;

extern void* kissAllocPages(size_t nPages);
// END platform-specific values

kissAllocator globalAlloc = (struct kissAllocator){ .rootPage = NULL, .freeList = NULL };

kissPageHeader* newPage() {
  // try to get a page from the freelist first
  if (globalAlloc.freeList != NULL) {
    kissPageHeader* page = globalAlloc.freeList;
    globalAlloc.freeList = page->freeList;
    page->freeList = NULL;
    return page;
  }
  // when there isn't one, allocate and initialize a new page
  else {
    kissPageHeader* page = kissAllocPages(1);
    if (page == NULL) { return NULL; }
    // insert the new page into the circular page list
    page->prev = globalAlloc.rootPage == NULL ? page : globalAlloc.rootPage;
    page->next = globalAlloc.rootPage == NULL ? page : globalAlloc.rootPage->next;
    page->prev->next = page->next->prev = page;
    // basic initializations
    page->freeList = NULL;
    page->nObjects = 0;
    page->topOffset = sizeof(kissPageHeader);
    page->largeObjPages = 0;
    // the new page is now the root page
    globalAlloc.rootPage = page;
    return page;
  }
}

void* bump(kissPageHeader* page, size_t nbytes, size_t align) {
  size_t top = page->topOffset;
  // BEGIN TODO use checked arithmetic
  // align the top offset
  size_t misaligned = (align - 1) & top;
  if (misaligned > 0) {
    top += align - misaligned;
  }
  // snag the current top, which is where out pointer will be
  size_t base = top;
  // reserve the requested number of bytes
  top += nbytes;
  // END TODO use checked arithmetic
  if (top >= pageSize) { return NULL; }
  page->topOffset = top;
  void* out = (char*)page + base;
  return out;
}

void* kissAlloc(size_t nbytes, size_t log2_align) {
  if (log2_align < 3) { log2_align = 3; } // minimum alignment is 8 = 2^3 bytes
  size_t align = 1 << log2_align;

  // large object allocation
  if (nbytes >= largeObjThreshold || align >= largeObjThreshold) {
    size_t npages = nbytes >> log2_pageSize + (low_mask & nbytes == 0 ? 0 : 1);
    return NULL; //TODO large object allocation
  }

  // small object allocation
  kissPageHeader *availPage = globalAlloc.rootPage;
  if (availPage == NULL) {
    availPage = newPage();
    if (availPage == NULL) { return NULL; }
  }
  void* out = bump(availPage, nbytes, align);
  if (out == NULL) {
    availPage = newPage();
    if (availPage == NULL) { return NULL; }
    out = bump(availPage, nbytes, align);
  }
  assert(out != NULL);
  availPage->nObjects += 1;
  return out;
}

void kissFree(void* ptr) {
  if (low_mask & ptr == 0) {
    return; //TODO large object free
  }

  // small object deallocation
  kissPageHeader* page = (void*)(high_mask & (uintptr_t)ptr);
  page->nObjects -= 1;
  if (page->nObjects == 0) { // emptied pages go on the free list
    page->freeList = globalAlloc.freeList;
    page->topOffset = sizeof(kissPageHeader);
    globalAlloc.freeList = page;
  }
}
