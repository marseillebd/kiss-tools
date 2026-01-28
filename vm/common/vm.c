#include "../kiss-vm.h"
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

// a type for indexes
typedef unsigned int uint;

// a type for VM values, which are always 32 bits, but may have different interpretations
typedef union word {
  uint32_t u;
  int32_t i;
} word;

// Yes, it's a global variable holding the entire VM state.
// The VM executable should do little more than (load and) execute bytecode.
// It doesn't need to be embedded, and it doesn't need multiple threads.

struct vm {
  struct {
    char *buf;
    uint sz;
  } code;
  uint ip; // offset into the code buffer

  struct {
    word *buf;
    uint sz;
  } stack;
  uint sp; // offset into the stack buffer; grows upwards
  uint fp; // offset into the stack buffer; grows upwards

  struct {
    char *buf;
    uint sz;
  } data;

  int exit_code;
} vm;

void vmInit(
  char *code, unsigned int code_sz,
  char *stack, unsigned int stack_sz,
  char *data, unsigned int data_sz
) {
  if (code == NULL) { code_sz = 0; }
  vm.code.buf = code;
  vm.code.sz = code_sz;
  vm.ip = 0;

  if (stack == NULL) { stack_sz = 0; }
  vm.stack.buf = (word*)stack;
  vm.stack.sz = stack_sz / sizeof(word);
  vm.sp = 0;
  vm.fp = stack_sz - 1;

  if (data == NULL) { data_sz = 0; }
  vm.data.buf = data;
  vm.data.sz = data_sz;

  vm.exit_code = 0;
}

int vmExitCode() {
  return vm.exit_code;
}

//////////////////
// The fun part!
//////////////////

// Implements "overridable semicolon" that premeturely exits vm microcode.
// Relies on GCC's "Statement Expressions"
#define DO(f) ({ \
  vmResult _r = VM_OK; \
  word _it = (f)(&_r); \
  if (_r != VM_OK) { return _r; } \
  _it; \
})

// 6-bit zero-extended:  0b00xxxxxx
// 6-bit one-extended:   0b01xxxxxx
// 8-bit:                0b10000001 <1 byte>
// 16-bit little-endian: 0b10000001 <2 bytes>
// 32-bit little-endian: 0b10000010 <4 bytes>
// 64-bit little-endian: 0b10000011 <8 bytes>
// other immediate value byte patterns are reserved
// not all immediate values will be supported byt all implementations
// at least the 6-bit encodings and the word-size encoding should be, though
#define IMM() DO(imm)
static
word imm(vmResult *r) {
  word it; it.u = 0;
  if (vm.code.sz - vm.ip < 4) {
    *r = VM_SEG;
  } else {
    for (int i = 0; i < 4; i++) {
      it.u <<= 8;
      it.u |= vm.code.buf[vm.ip];
      vm.ip++;
    }
  }
  return it;
}

#define POP() DO(pop)
static
word pop(vmResult *r) {
  word it; it.u = 0;
  if (vm.sp == 0) {
    *r = VM_SEG;
  } else {
    vm.sp--;
    it = vm.stack.buf[vm.sp];
  }
  return it;
}

#define PUSH(val) do { \
  vmResult _r = VM_OK; \
  push(&_r, (val)); \
  if (_r != VM_OK) { return _r; } \
} while (false)
static
void push(vmResult *r, word val) {
  if (vm.sp >= vm.fp) {
    *r = VM_SEG;
  } else {
    vm.stack.buf[vm.sp] = val;
    vm.sp++;
  }
}

vmResult vmStep() {
  char opcode; {
    if (vm.ip >= vm.code.sz) { return VM_SEG; }
    opcode = vm.code.buf[vm.ip];
    vm.ip += 1;
  }
  switch (opcode) {
    case 0x00: // illegal opcode
      return VM_ILL;
    case 0x20: // no operation
      return VM_OK;
    case 0x50: // push immediate
      PUSH(IMM());
      return VM_OK;
    case 0x7F: // exit/halt
      vm.exit_code = POP().i;
      return VM_HLT;
    default:
      return VM_ILL;
  }
}
