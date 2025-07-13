#include "gc.h"
#include <stdint.h>

#define PACKED __attribute__((packed))

////////////////////
////// Limits //////
////////////////////

#define GC_BLK_SIZE 4096

#define GC_MAX_ROOTS 100

#define GC_PTR_ALIGN 16

///////////////////
////// Types //////
///////////////////

////// Roots //////

struct gc_root {
  gc_obj* it; // nullable, representing an unused root
};

////// Objects //////

struct gc_obj {
  void* it;
};

typedef struct PACKED gc_objheader {
  uint32_t nobjs; // FIXME I need to derive/check some limits based on this representation
  uint32_t nbytes; // FIXME I need to derive/check some limits based on this representation
} gc_objheader;

////// Blocks //////

typedef struct gc_blk gc_blk;

typedef struct gc_blkheader {
  gc_blk* next; // nullable, representing end of block list
  size_t freeoff; // offset within the block of the next free memory
  int side; // either 0 or 1, representing whether this block is being collected from or allocated into.
            // The meanings of 0 and 1 are determined by the state of the engine.
  // TODO later version: generation
} gc_blkheader;

struct gc_blk {
  gc_blkheader header;
  char arena[GC_BLK_SIZE - sizeof(gc_blkheader)];
};

////// Engine //////

typedef struct gc_engine {
  gc_blk* alloc_blks;
  size_t nallocblks;
  gc_blk* collect_blks;
  size_t ncollectblks;
  gc_root roots[GC_MAX_ROOTS];
  int curside; // see gc_blkheader.side
} gc_engine;

////////////////////////////
////// Implementation //////
////////////////////////////

static
size_t align(size_t v, size_t log2_align) {
  size_t mask = (~0) << log2_align;
  size_t strayBits = v & mask;
  if (strayBits != 0) {
    v += mask & ~strayBits;
  }
  return v;
}

#include <stdlib.h> // TODO remove cstdlib dependence

gc_engine engine = {
  .alloc_blks = NULL,
  .nallocblks = 0,
  .collect_blks = NULL,
  .ncollectblks = 0,
  .roots = {},
  .curside = 0
};

// FIXME I suppose if I'm going to all enough collect blocks to cover the alloc blocks,
// I may as well create both at the same time.

static gc_blk* newblk() {
  // TODO ensure we don't create more blocks than allowed
  // FIXME don't assign it to either of the lists yet, or at least parameterize
  gc_blk* it = aligned_alloc(GC_BLK_SIZE, GC_BLK_SIZE);
  // initialize block header
  it->header.next = engine.alloc_blks;
  it->header.freeoff = align(sizeof(gc_blkheader), GC_PTR_ALIGN);
  it->header.side = engine.curside;
  // update the engine state
  engine.alloc_blks = it;
  engine.nallocblks += 1;
  // done
  return it;
}

// NOTE there's no delblk, since we don't collect the blocks, at least in this version

// TODO reset block
