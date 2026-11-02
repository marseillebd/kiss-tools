#define f__RT_INLINE
#include "forth_rt.h"

////// Tracing //////

#include <stdarg.h>
#include <stdio.h>

#ifdef f_TRACE
  #define TRACE(...) _trace(__VA_ARGS__)
  static
  void _trace(const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    defer { va_end(args); }
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
  }
#else
  #define TRACE(...) NULL
#endif

////// Interpreter //////

void f__step(ST* st) {
  TRACE("------ f__step: ip = %p", st->ip);
  Thread* callee = V2THD(*st->ip);
  PUSH(&st->r, VP2V(st->ip + 1)); // save return address
  st->ip = &callee->body[0]; // jump to the thread body
  TRACE("%p: callee", callee);
  TRACE("%p: new st->ip", st->ip);
  TRACE("%p: new interp", callee->interp);
  tailcall callee->interp(st); // interpret the thread
}

void f__imm(ST* st) {
  TRACE("------ f__imm");
  f_Value* imm_p = V2VP(POP(&st->r));
  PUSH(&st->d, *imm_p);
  TRACE("[%d]: %lu", st->d.cur, *imm_p);
  st->ip = imm_p + 1;
  tailcall f__step(st);
}

void f__skip(ST* st) {
  TRACE("------ f__skip");
  f_Value* off_p = V2VP(POP(&st->r)); // the skip amount (immediately after the skip instruction) has been pushed on the return stack
  st->ip = (f_Value*)((char*)off_p + *off_p); // thus, a zero-length skip must be at least one to jump over the skip amount
                                // Also, to make things easier on the machine code, skip amount is in bytes rather than words
  tailcall f__step(st); // this means that we can only skip within a user-defined function
}

void f__ret(ST* st) {
  TRACE("------ f__ret");
  POP(&st->r); // top of return stack points after the f_ret instruction; it's trash
  st->ip = V2VP(POP(&st->r)); // the true return address is NoS when this primitive is executed
  tailcall f__step(st);
}

void f__stop(ST* st) {
  TRACE("------ f__stop");
  return;
}

////// Execute //////

#define MAX_STACKS 8192
#define MAX_RETURNS 1024
#define MAX_DATA (MAX_STACKS - MAX_RETURNS)

extern const f_Value Fmain[];
extern const f_Value F_start[];

static inline
void showCore();

f_Value f_main(void) {
  static f_Value dataStack[MAX_DATA];
  static f_Value returnStack[MAX_RETURNS];
  ST st = (ST){
    .ip = &F_start[1],
    .d = (f__Stack){
      .cap = MAX_DATA,
      .cur = -1,
      .stack = dataStack,
    },
    .r = (f__Stack){
      .cap = MAX_RETURNS,
      .cur = -1,
      .stack = returnStack,
    },
  };
  showCore();

  f__step(&st);

  TRACE("============");
  TRACE("st.d.cur: %d", st.d.cur);
  if (st.d.cur < 0) {
    return 0;
  } else {
    return st.d.stack[st.d.cur];
  }
}

////// Primitives //////

const f_Value f_imm[] = { (f_Value)f__imm };
const f_Value f_skip[] = { (f_Value)f__skip };
const f_Value f_ret[] = { (f_Value)f__ret };
const f_Value f_stop[] = { (f_Value)f__stop };

const f_Value F_start[] = {
  (f_Value)f__step,
  (f_Value)&Fmain[0],
  (f_Value)&f_stop[0],
};

static inline
void retPrim(ST* st) {
  st->ip = V2VP(POP(&st->r));
  // ^ This works because when calling a primitive, the ip points after a pointer to the C primitive implementation
  // (i.e. the current ip is trash inside a primitive).
  // Instead, the instruction _after_ the call to the primitive is where we should go next,
  // and that location happens to already have been pushed onto the return stack by f__step.
  // We also know that if was f__step who ultimiately invoked up and also the interpreter we should use next,
  // because the call trace always has f__step at interior nodes and priitives at leaves.
  tailcall f__step(st);
}

#define f__DEF_BUILTIN(name) \
  void f_PASTE(f__,name)(ST*); \
  const f_Value f_PASTE(F,name)[] = { \
    (f_Value)&f_PASTE(f__,name), \
  };
f_FOREACH_BUILTIN(f__DEF_BUILTIN)

////// Control Flow //////

void f__jump(ST* st) {
  POP(&st->r); // top of return stack points after the Fjump call, but we don't want to return there
  Thread* callee = V2THD(POP(&st->d));
  st->ip = &callee->body[0];
  tailcall callee->interp(st);
}

void f__call(ST* st) {
  Thread* callee = V2THD(POP(&st->d));
  st->ip = &callee->body[0];
  tailcall callee->interp(st);
}

void f__ifnz(ST* st) {
  f_Value condition = POP(&st->d);
  f_Value thenClause = POP(&st->d);
  f_Value elseClause = POP(&st->d);
  PUSH(&st->d, condition != 0 ? thenClause : elseClause);
  tailcall retPrim(st);
}

////// Stack Manipulation //////

void f__ndrop(ST* st) {
  f_Value amt = POP(&st->d);
  st->d.cur -= amt;
  if (st->d.cur < 0) { st->d.cur = -1; }
  tailcall retPrim(st);
}

void f__rZ_ndrop(ST* st) {
  st->ip = V2VP(POP(&st->r));
  //^ since we're making changes to the return stack,
  //  we need to fixup the return stack before doing the real operation
  f_Value amt = POP(&st->d);
  st->r.cur -= amt;
  if (st->r.cur < 0) { st->r.cur = -1; }
  tailcall f__step(st);
}

DEFUN(drop,
  IMM(1), DO(ndrop),
)

DEFUN(nip,
  DO(swap), DO(drop),
)

void f__pick(ST* st) {
  f_Value depth = POP(&st->d);
  uintptr_t i = st->d.cur - (depth - 1);
  // TODO bounds check
  PUSH(&st->d, st->d.stack[i]);
  tailcall retPrim(st);
}

DEFUN(dup,
  IMM(1), DO(pick),
)

DEFUN(over,
  IMM(2), DO(pick),
)

void f__rZ_pick(ST* st) {
  st->ip = V2VP(POP(&st->r));
  //^ since we're making changes to the return stack,
  //  we need to fixup the return stack before doing the real operation
  f_Value depth = POP(&st->d);
  uintptr_t i = st->r.cur - (depth - 1);
  // TODO bounds check
  PUSH(&st->d, st->r.stack[i]);
  tailcall f__step(st);
}

void f__plop(ST* st) {
  f_Value depth = POP(&st->d);
  f_Value value = POP(&st->d);
  uintptr_t i = st->d.cur - (depth - 1);
  // TODO bounds check
  st->d.stack[i] = value;
  tailcall retPrim(st);
}

void f__rZ_plop(ST* st) {
  st->ip = V2VP(POP(&st->r));
  //^ since we're making changes to the return stack,
  //  we need to fixup the return stack before doing the real operation
  f_Value depth = POP(&st->d);
  f_Value value = POP(&st->d);
  uintptr_t i = st->r.cur - (depth - 1);
  // TODO bounds check
  st->r.stack[i] = value;
  tailcall f__step(st);
}

void f__roll(ST* st) {
  f_Value depth = POP(&st->d);
  uintptr_t i = st->d.cur - (depth - 1);
  // TODO bounds check
  f_Value oldBottom = st->d.stack[i];
  for (; i < st->d.cur; i++) {
    st->d.stack[i] = st->d.stack[i+1];
  }
  st->d.stack[st->d.cur] = oldBottom;
  tailcall retPrim(st);
}

void f__ZSroll(ST* st) {
  f_Value depth = POP(&st->d);
  uintptr_t lowest = st->d.cur - (depth - 1);
  // TODO bounds check
  f_Value oldTop = st->d.stack[st->d.cur];
  for (uintptr_t i = st->d.cur; i > lowest; i--) {
    st->d.stack[i] = st->d.stack[i-1];
  }
  st->d.stack[lowest] = oldTop;
  tailcall retPrim(st);
}

DEFUN(swap,
  IMM(2), DO(roll),
)

DEFUN(rot,
  IMM(3), DO(roll),
)

DEFUN(ZSrot,
  IMM(3), DO(ZSroll),
)

void f__rZ_save(ST* st) {
  st->ip = V2VP(POP(&st->r));
  //^ since we're making changes to the return stack,
  //  we need to fixup the return stack before doing the real operation
  f_Value amt = POP(&st->d);
  // TODO bounds check
  st->d.cur -= amt;
  for (int i = 1; i <= amt; i++) {
    PUSH(&st->r, st->d.stack[st->d.cur + i]);
  }
  tailcall f__step(st);
}

void f__ZGr(ST* st) {
  st->ip = V2VP(POP(&st->r));
  //^ since we're making changes to the return stack,
  //  we need to fixup the return stack before doing the real operation
  PUSH(&st->r, POP(&st->d));
  tailcall f__step(st);
}

void f__rZ_load(ST* st) {
  st->ip = V2VP(POP(&st->r));
  //^ since we're making changes to the return stack,
  //  we need to fixup the return stack before doing the real operation
  f_Value amt = POP(&st->d);
  // TODO bounds check
  st->r.cur -= amt;
  for (int i = 1; i <= amt; i++) {
    PUSH(&st->d, st->r.stack[st->r.cur + i]);
  }
  tailcall f__step(st);
}

void f__rZG(ST* st) {
  st->ip = V2VP(POP(&st->r));
  //^ since we're making changes to the return stack,
  //  we need to fixup the return stack before doing the real operation
  PUSH(&st->d, POP(&st->r));
  tailcall f__step(st);
}

////// ALU //////

#define BINOP(operation) \
  f_Value tos = POP(&st->d); \
  f_Value nos = POP(&st->d); \
  PUSH(&st->d, operation); \
  tailcall retPrim(st);

void f__ZA(ST* st) { BINOP(nos + tos); }
void f__ZS(ST* st) { BINOP(nos - tos); }
void f__ZM(ST* st) { BINOP(nos * tos); }

void f__ZDZR(ST* st) {
  f_Value tos = POP(&st->d);
  f_Value nos = POP(&st->d);
  PUSH(&st->d, nos % tos);
  PUSH(&st->d, nos / tos);
  tailcall retPrim(st);
}

DEFUN(ZD,
  DO(ZDZR), DO(nip),
)
DEFUN(ZR,
  DO(ZDZR), DO(drop),
)

////// Tracing Helpers //////

static inline
void showCore() {
  TRACE("%p: f__step", &f__step);
  TRACE("%p: f__imm", &f__imm);
  TRACE("%p: f__ret", &f__ret);
  TRACE("%p: f__stop", &f__stop);
  TRACE("%p: f_imm", f_imm);
  TRACE("%p: f_ret", f_ret);
  TRACE("%p: f_stop", f_stop);
  TRACE("============");
  TRACE("%p: F_start", &F_start[0]);
  TRACE("%p: Fmain", &Fmain[0]);
  TRACE("============");
}
