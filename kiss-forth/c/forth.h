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
extern const f_Value f_skip[];
extern const f_Value f_ret[];

////// Primitives //////

#define f_FOREACH_BUILTIN(X) \
  X(jump)                    \
  X(call)                    \
  X(ifnz)                    \
                             \
  X(ndrop)                   \
  X(pick)                    \
  X(plop)                    \
  X(roll)                    \
  X(ZSroll)                  \
                             \
  X(rZ_ndrop)                \
  X(rZ_save)                 \
  X(rZ_load)                 \
  X(rZ_pick)                 \
  X(rZ_plop)                 \
  X(ZGr)                     \
  X(rZG)                     \
                             \
  X(ZA)                      \
  X(ZS)                      \
  X(ZM)                      \
  X(ZDZR)                    \

#define f_FOREACH_STDLIB(X)  \
  X(drop)                    \
  X(nip)                     \
  X(dup)                     \
  X(over)                    \
  X(swap)                    \
  X(rot)                     \
  X(ZSrot)                   \
                             \
  X(ZD)                      \
  X(ZR)                      \

#define f_DECL_STDLIB(name) \
  extern const f_Value f_PASTE(F,name)[];
f_FOREACH_BUILTIN(f_DECL_STDLIB)
f_FOREACH_STDLIB(f_DECL_STDLIB)


////// Hand Compiling Helpers //////

#ifndef F_HAND_MACROS

#define DEFUN(name, ...)              \
  const f_Value f_PASTE(F,name)[] = { \
    (f_Value)&f__step,                \
    __VA_ARGS__                       \
    (f_Value)&f_ret[0],               \
  };

#define DO(name)               \
  (f_Value)&f_PASTE(F,name)[0]

#define IMM(n)                 \
  (f_Value)&f_imm[0],          \
  (f_Value)(n)

#define TICK(name) \
  IMM((f_Value)&f_PASTE(F,name)[0])

// you have to skip over the amount, then the interior will probably be a function and you'll have to skip over its interpreter and return instruction as well as all the calls it includes
// don't forget that IMM and SKIP take up two words
#define SKIP(nWords) \
  (f_Value)&f_skip[0], \
  (f_Value)(sizeof(f_Value) * nWords)

#endif

#endif // include once
