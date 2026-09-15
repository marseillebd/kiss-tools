#include <stdint.h>

// this code relies on:
// - a code label `f__step` which is the main loop of the interpreter
// - a label `f__start` that starts the Forth function _start
// it provides:
// - a code label `f__doStop` which is the implementation of the internal primitive `_stop`
// - a label `f__rstack` of empty memory useable for the control stack

extern uintptr_t f__start;

// I think this is effectively a synthesized Forth caller code,
// but we know the first instruction f__start will not return.
// We init the vip to a _pointer_ to this variable,
// since this variable "is" a Forth instruction
// and vip should point to Forth instructions.
uintptr_t* f__start_p;

uintptr_t f__rstack[4096];

int main() {
  // since this is for linux/posix/sysv, I'll just let the compiler grab crt0 for me to handle the interface from OS to forth entry point.

  // TODO: initialize progname, args, envvars
  static uintptr_t* vrp; vrp = &f__rstack[0];
  f__start_p = &f__start;
  static uintptr_t** vip; vip = &f__start_p;
  static int exit_code;
  static uintptr_t save_sp;
  static uintptr_t save_bp;
  // I'm expecting to compile with gcc, and so I'll use its inline assembly syntax
  // I suspect clang will use the same syntax, since it has already cribbed so much of the rest of the interface (and its documentation)
  __asm__ (
      // save sp and bp
      "\n\t" "movq %%rsp, %[sp]"
      "\n\t" "movq %%rbp, %[bp]"

      "\n\t" "mov %[vrp], %%rbp"
      "\n\t" "mov %[vip], %%rsi"
      "\n\t" "jmp f__step"

      // implement the internal stop primitive here,
      // since it depends on the platform
      "\n\t" ".global f__doStop"
      "\n"   "f__doStop:"
      "\n\t" "popq %%rax" // TOS is the exit code
      "\n\t" "movl %%eax, %[ec]"

      // TODO teardown the guest machine
      // restore sp and bp
      "\n\t" "movq %[sp], %%rsp"
      "\n\t" "movq %[bp], %%rbp"
    // output operands
    : [ec] "=m" (exit_code)
    , [sp] "+m" (save_sp)
    , [bp] "+m" (save_bp)
    // input operands
    : [vrp] "m" (vrp)
    , [vip] "m" (vip)
    // clobbers potentially everything
    : "rax", "rcx", "rdx", "rbx", "rsi", "rdi" // NOT these: "rsp", "rbp"
    , "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"
    , "cc", "memory"
    // TODO and eventually also the floating-point registers
  );
  return exit_code;
}
