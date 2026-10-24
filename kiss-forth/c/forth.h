#ifndef FORTH_RT_H
#define FORTH_RT_H

////// C Dialect //////

#include <stdint.h>

#define f_PASTE(a,b) a##b

////// Forth Types //////

typedef uintptr_t f_Value;
typedef uint8_t f_Byte;

////// Forth Entry Point //////

f_Value f_main(void);

////// Forth Codewords //////

#ifdef f__INTERNAL
typedef struct ST ST;
void f__step(ST*);
#else
void f__step(void*);
#endif

extern const f_Value f_imm[];
extern const f_Value f_ret[];
extern const f_Value f_halt[];

////// Primitives //////

#define f_FOREACH_BUILTIN(X) \
  X(jump)                    \
  X(call)                    \
  X(ifnz)                    \
  X(ZA)

#define f_DECL_BUILTIN(name) \
  extern const f_Value f_PASTE(F,name)[];

f_FOREACH_BUILTIN(f_DECL_BUILTIN)

////// Hand Compiling Helpers //////

#ifndef F_HAND_MACROS

#define DEFUN(name, ...)        \
  f_Value f_PASTE(F,name)[] = { \
    (f_Value)&f__step,          \
    __VA_ARGS__                 \
    (f_Value)&f_ret[0],         \
  };

#define DO(name)               \
  (f_Value)&f_PASTE(F,name)[0]

#define IMM(n)                 \
  (f_Value)&f_imm[0],          \
  (f_Value){ (n) }

#define TICK(name) \
  IMM((f_Value)&f_PASTE(F,name)[0])

#endif

#endif // include once
