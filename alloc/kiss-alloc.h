#ifndef KISS_ALLOC_H
#define KISS_ALLOC_H

#include <stddef.h>

typedef struct kissPageHeader kissPageHeader;
struct kissPageHeader {
  // The prev and next pointers are there to form a circular doubly-linked list.
  // I'm not assuming the system's page allocator will always give pages contiguously,
  // so this list is given so that an allocator can reach any page it controls
  // as long as it has a reference to one of them.
  kissPageHeader *prev, *next;
  // When a page has all its objects freed, the allocator can re-use it for more allocations.
  // This pointer allows a stack, called the free list, to be stored within the pages themselves,
  // rather than create a new stack type and somehow allocate its memory elsewhere.
  kissPageHeader *freeList;
  // Counts the number of currentlyl valid objects allocated in this page (but not the large object).
  size_t nObjects;
  // NOTE topOffset _could_ be a smaller type, but this might imply a max page size.
  // Perhaps a max page size could be lived with, but what would be worse is trying to select
  // an appropriately-sized type based on the page size.
  // Hacking is more resilient if we use `size_t`.
  size_t topOffset;
  // The number of pages immediately following this one which are used to store a large object.
  // (A large object is one that is at least as many bytes as the page size or aligned to at least
  // the page boundary).
  size_t largeObjPages;
  // TODO I may want to allow extra data in here, such as gc information
  // OTOH, if a page is used for a gc, I might just allocate the gc header within the block.
};

typedef struct kissAllocator {
  kissPageHeader *rootPage;
  kissPageHeader *freeList;
} kissAllocator;

void* kissAlloc(size_t nbytes, size_t log2_align);
void kissFree(void* ptr);

#endif
