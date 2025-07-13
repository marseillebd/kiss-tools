#ifndef KISS_GC
#define KISS_GC

#include <stddef.h>

//////////////////////////
////// General Info //////
//////////////////////////

// This is an interface for a garbage collector where you manually manage the collection roots.

// Allocate to your heart's content (well, up to some maximum number of bytes).
// Should memory become scarce, a garbage collection will be triggered.
// Collection begins at the "roots", and maintains all and only the memory reachable from those roots.

// You should not assume anything about the algorithm.
// It might be a moving collector, it might be generational, it might be on-line.
// The C stack will not be traced.
// So, except for the smallest blocks of code, ensure your access to gc objects goes through roots.
// That way, those objects will be traced, and they won't be spontaneously moved.

// Well, you can assume one thing: these functions are not thread-safe.
// If you use the same gc engine across multipe threads, problems may arise,
// WARNING and right now, I've only implemented a single, global engine.

////////////////////////
////// GC Objects //////
////////////////////////

typedef struct gc_obj gc_obj;

struct gc_objinfo {
  size_t nrefs; // The number of pointers to other gcobjs held in the gcobj.
  size_t nbytes; // The number of bytes of non-gcobj data held in the gcobj.
};

// Return a new managed object with the given layout.
//
// NOTE If insufficient memory is available for allocation, this procedure panics.
// The idea is to treat this allocator more like a stack than a heap.
// Generally, you don't try to recover from a stack overflow, and the same applies here.
//
// WARNING Calls to this function may trigger a garbage collection.
// Collection may move managed data, so any plain `gc_obj` pointers become invalid after calling this function.
gc_obj* gc_alloc(struct gc_objinfo);

// Get a referenced object.
//
// WARNING If a collection happens while this pointer is the only live reference, it will dangle.
// Thus, the return value should be considered only to live up until the next call to `gc_alloc`.
gc_obj* gc_getobj( gc_obj* // the start object
                 , size_t i // the index of the referenced object
                 );

// Set an object to be referenced.
void gc_setobj( gc_obj* // the referencer
              , size_t i // the index where the reference should be stored
              , gc_obj* v // the new referenced object
              );

// Copy unboxed data from the input object into a destination buffer.
void gc_getmem( void* dst // buffer to write the data to
              , gc_obj* obj // the object to read from
              , size_t byteoff // the offset in `obj`s unboxed data from which to read
              , size_t nbytes // the number of bytes to read
              );

// Set unboxed data in a object.
void gc_setmem( gc_obj* obj // the object to mutate
              , void* src // the buffer to copy data from
              , size_t byteoff // the offset in `obj`s unboxed data where the data will be written, in bytes
              , size_t nbytes // the number of bytes to write
              );

// Retrieve information about the object: its layout in particular.
struct gc_objinfo gc_objinfo(gc_obj*);

//////////////////////
////// GC Roots //////
//////////////////////

// TODO documentation
typedef struct gc_root gc_root;
gc_root* gc_mkroot();
gc_obj* gc_getroot(gc_root*);
void gc_setroot(gc_root*, gc_obj*);
void gc_rmroot(gc_root*);

/////////////////////////
////// GC Engines //////
/////////////////////////

// TODO currently, there is a single, global engine

#endif
