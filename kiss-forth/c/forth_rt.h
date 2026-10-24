#define f__INTERNAL
#include "forth.h"

////// C Dialect //////

#include <assert.h>
#include <stdlib.h>

#ifdef f__RT_INLINE
  #define INLINE inline
#else
  #define INLINE extern inline
#endif

#if defined(__STDC_DEFER_TS25755__) || __has_include(<stddefer.h>)
  #include <stddefer.h>
#elif __GNUC__ > 8
  #define defer f__defer
  #define f__defer f__deferShim(__COUNTER__)
  #define f__deferShim(N)                                                \
    auto void f_PASTE(f__deferF, N)(int*);                               \
    __attribute__(( __cleanup__(f_PASTE(f__deferF, N)),                  \
                    __deprecated__, __unused__                           \
                 ))                                                      \
    int f_PASTE(f__deferV, N);                                           \
    __attribute__((__always_inline__, __deprecated__, __unused__))       \
    inline auto void                                                     \
    f_PASTE(f__deferF, N)(int*)
#else
  #error "The defer feature seems not available"
#endif


#if __has_c_attribute(clang::musttail)
  #define tailcall \
    [[clang::musttail]] return
#elif __has_attribute(musttail)
  #define tailcall \
    __attribute__((musttail)) return
#elif __has_c_attribute(gnu::musttail)
  #define tailcall \
    [[gnu::musttail]] return
#else
  #error "no known musttail attribute supported"
#endif

////// Runtime Support //////

/// Stacks

typedef struct f__Stack {
  int cap;
  int cur;
  f_Value* stack;
} f__Stack;

INLINE
f_Value POP(f__Stack* self) {
  assert(self->cur >= 0);
  return self->stack[self->cur--];
}

INLINE
void PUSH(f__Stack* self, f_Value val) {
  assert(self->cur < self->cap);
  self->cur++;
  self->stack[self->cur] = val;
}

/// The Forth Machine

struct ST {
  const f_Value* ip; // instruction pointer
  f__Stack d; // data stack
  f__Stack r ; // return stack
};

/// Concepts

// interpreter functions:
// These are C function pointers that can be tail-called by the runtime.
// Each call to a Forth primitive or user-defined function must point to one of these.
typedef void (*Interp)(ST*);

typedef struct Thread {
  Interp interp;
  f_Value body[];
} Thread;

////// Marshalling //////

INLINE
Thread* V2THD(f_Value v) {
  return (Thread*)v;
}

INLINE
f_Value VP2V(const void* addr) {
  return (f_Value)addr;
}
INLINE
void* V2VP(f_Value addr) {
  return (void*)addr;
}

////// Interpreter //////

void f__step(ST* st);
void f__imm(ST* st);
void f__skip(ST* st);
void f__ret(ST* st);
void f__stop(ST* st);
