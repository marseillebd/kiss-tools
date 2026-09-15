#include <stdint.h>

// this code relies on:
// - a uintptr_t-aligned buffer f__rstack for the return stack
// - a code label `f__step` which is the start of the "main loop"
// it provides:
// - a code label `f__doStop` which is the implementation of the internal primitive `_stop`

int main() {
  // since this is for linux/posix/sysv, I'll just let the compiler grab crt0 for me to handle the interface from OS to forth entry point.

  // I'm expecting to compile with gcc, and so I'll use its inline assembly syntax
  // I suspect clang will use the same syntax, since it has already cribbed so much of the rest of the interface (and its documentation)
  static int exit_code;
  static uintptr_t save_sp;
  static uintptr_t save_bp;
  // TODO: initialize progname, args, envvars
  __asm__ (
      // save sp and bp
      "\n\t" "movq %%rsp, %[sp]"
      "\n\t" "movq %%rbp, %[bp]"

      // TODO create and start up the guest machine
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
    : // input operands
    // clobbers potentially everything
    : "rax", "rcx", "rdx", "rbx", "rsi", "rdi" // NOT these: "rsp", "rbp"
    , "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"
    , "cc", "memory"
    // TODO and eventually also the floating-point registers
  );
  return exit_code;
}
