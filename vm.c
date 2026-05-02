/* compile with:
gcc -std=c11 -pedantic -Wall vm.c

a good way to create an input file (for now) is to use xxd:
edit sample-bytecode.hex, then
xxd -r sample-bytecode.hex | tee sample-bytecode.bin | xxd
inspect the result on stdout, and if ok, then
./a.out sample-bytecode.bin
*/

// Try for as long as possible to just keep everything inside this file.
// DO NOT make a fancy build system.
// DO NOT expose as a library.
// DO NOT make it cross-platform (it's meant to just serve as a bueprint for hand-compilation)
// DO NOT avoid the C stdlib, at least for now

/* This is a C implementation of a simple virtual machine.
 * The goals are:
 * - easy to hand-implement an interpreter (this file)
 * - easy to target from a compiler
 * - easy to compile the vm code to native (so, a small, simple set of operations)
 */

//////////////////////////
////// Dependencies //////
//////////////////////////

#include <limits.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <assert.h>
_Static_assert(CHAR_BIT == 8, "assume an 8-bit byte");

//////////////////////////////////////////
////// Virtual Machine Architecture //////
//////////////////////////////////////////

typedef union {
  uint32_t u;
  int32_t i;
} word_t;

// so, the bytecode is big-endian
word_t be32(uint8_t* bc) {
  uint32_t bits = (uint32_t)bc[0] << 8*3
                | (uint32_t)bc[1] << 8*2
                | (uint32_t)bc[2] << 8*1
                | (uint32_t)bc[3] << 8*0;
  return (word_t){ .u = bits };
}

size_t _codesize; // just for assertions
int ip; // instruction pointer
int lp; // link register (set on unconditional jumps)

// a stack, grows upwards
int sp;
word_t stack[16*1024/4]; // TODO hardcoded to be 16kiB for now

// flags
bool carry = false;

// an unstructured heap
char heap[2*1024*1024]; // TODO hardcoded to be 2MiB for now

// instruction ROM
uint8_t* code;

/////////////////////////////
////// Instruction Set //////
/////////////////////////////

/*
So, I'm still thinking about the bytecode design.

OnRamp vm gets away with just 16 opcodes (one of them is a syscall and can do a lot (incl exit, fopen/tell/seek/read/write/close, and a lot more I think).
I don't think I want to support as many bits of the OnRamp syscalls, but its opcides look very nice.
Every instr is 4 bytes (very nice for alignment/decoding): 1 op + 3 args
- opcodes are in the range 70-7F
  - add, sub, mul, div (signed or unsigned?), and, or, shl, shru, ldw, stw, ims (16bits of immediate), ldb, jz, ltu
- arguments mostly are split into three groups:
  - non-neg immediates are 00-7f
  - registers (16 of them) are 80-8f
  - negative immediates are 90-ff
- load immediate takes a register as arg1, and args2+3 are 16 bits; arg1 is shifted up and the 16bit imm is put in the low bits
- there's only one control flow: jz relative addr
- CON: there's no stack
- CON: loads of wasted space in the opcodes; lacks computed jumps, carry arithmetic is missing, conditional moves might be nice, no call/ret

The 6502 has 56 instructions, though many are different in that they target different registers:
- ld, st, mov
- push/pop the stack to/from regs
- and, xor, or, bit(test)
- add/sub(with carry only), cmp
- inc/dec (which I don't need)
- asl, lsr, ror, rol
- jmp, jsr, rts (ret)
- branch on {carry, zero, negative, overflow} is {set, clear}
- clear/set some status flags
- nop and brk/rti for interrupts
The constant use of the carry flagg+ops makes sense with an 8-bit machine trying to do 16/32-bit arith. Unneeded for me.

Takeaways so far are: I _can_ get away with a lot less that I'd expect, if I have some form of addressing mode rather than just pure stack operations.
That said, I'm still favoring having a stack addressing mode for implementing expression evaluation and argument passing.
Also, an auto-increment (prolly not auto-decrement) mode for memory addresses would be nice.

RiscV:
- add, and, or, xor
- shl, shrl, shra (shifts)
- stli{,u} (a bit oddball), lui, auipc
- slt{,u} (set when less than)
- jal (jump and link), jalr
- b{eq,ne,lt,gt,ltu,gtu,ge,le,geu,leu}
- {l,s}{w,h,hu,b,bu}
- fence (for synchronization barriers)
- ecall, ebreak (for system stuff)
- mul, div, rem with the M extension
There are a bunch of pseudo instructions:
  - nop is just add zero with zero and then place in the zero register (which I don't have)
  - mov is just add with zero
  - not is just xor with -1
  - j is just jal storing the link in the zero register (which I don't have, again)

So, here's my idea:
1-byte opcode + 3x 1-byte arguments, the arguments are given by addressing modes:
- 00-7f are 7 bits of zero-extended immediate mode
- 70-8f are register mode `register[i]`
- 90-9f are register auto-increment mode `register[i]++`
- a0-af are special modes
  - a0: zero register `0` and ignore writes
  - a1: push stack `stack[sp++]`
  - a2-ae: UNASSIGNED, perhaps just auto-decrement, or `stack[sp-i]`
  - ae: peek stack `stack[sp-1]`
  - af: pop stack `stack[--sp]`
- c0-ff are 6 bits of sign-extended immmediate mode
So, we've got space for 256 opcodes:
- add, sub, mul, divu
- adc, sbb, mulc, modu (the mulc and modu just load from the carry register set by add/sub/mul/div, so I'd have to think about it)
- and, or, xor
- shl, shrl, shra
- bit(test), mask (zero, except set bits from a2 to a3 inclusive)
- ldw, stw, ldb, stb
- jal, jalc
  - jal dst, imm16: takes a 16-bit signed immediate that relative to the ip, saves ip into dst
  - jalc dst, src, ign: saves ip into dst and jumps to src
  - ret = jalc zeroReg, savedIp
  - ldpc dst = jal dst, 0
- branch, where the low 3 bits specify a gt/eq/lt mask, so mask 111 would be unconditional, mask 010 is on zero, mask 110 would be gte
- cmpi, cmpu: sets up for a branch
- imm (the src args combine to just be a 16-bit immediate)
- brk/hlt, swi
And this still leaves plenty of room for more advanced opcodes:
- dup, swap, drop stack
- popcount, count {lead,tral}ing {zero,one}s, bit-implies, max/min/clamp, lea, conditional moves, bit and byte swaps, fused multiply-add, perhaps even floating point
- to/from utf8

*/

/*

I actually _am_ going with a pure stack-based architecture.
There are four special-purpose registers: instruction pointer (ip), link pointer (lp), stack pointer (sp), and flags (f).
Nearly every instruction is one byte, with the exception of imm32, which is five bytes to accomodate a 32-bit value.
There are roughly three instruction formats:
- 0x00-7F (0b0xxxxxxx): push zero-extended 7-bit immediate
- 0xC0-FF (0b11xxxxxx): push one-extended 6-bit immediate
- 0x08-B7: operation codes. There are some sub-types:
  - 0x80-8F (0b1000xxxx): read (duplicate/copy to top of stack) at depth 0bxxxx within the stack (`sp-1` -- `sp-16`)
  - 0x90-9F (0b1001xxxx): save at depth 0bxxxx on the stack (`sp-1` -- `sp-16`)
  - 0xA0-A3: arithmetic operations
  - 0xA4-A7: load/store
  - 0xA8-AE: mostly bitwise and shift operations
  - 0xAF imm32be: push 32-bit immediate to stack
  - 0xB0-B7: jumps
  - 0xB8-BF: miscellaneous
*/

#define FOR_OP(X) \
  X(add, 0xA0, NULL) \
  X(sub, 0xA1, NULL) \
  X(mul, 0xA2, NULL) \
  X(div, 0xA3, NULL) \
  X(mod, 0xAB, NULL) \
  X(or,  0xA8, NULL) \
  X(and, 0xA9, NULL) \
  X(xor, 0xAA, NULL) \
  X(shr, 0xAC, NULL) \
  X(sar, 0xAD, NULL) \
  X(shl, 0xAE, NULL) \
  X(imm, 0xAF, NULL) \
  X(jnc, 0xB0, NULL) \
  X(jlt, 0xB1, NULL) \
  X(jeq, 0xB2, NULL) \
  X(jle, 0xB3, NULL) \
  X(jgt, 0xB4, NULL) \
  X(jne, 0xB5, NULL) \
  X(jge, 0xB6, NULL) \
  X(j,   0xB7, NULL) \
  X(ret, 0xB8, NULL) \
  X(slp, 0xB9, NULL) \
  X(pop, 0xBA, NULL) \
  X(xch, 0xBB, NULL) \
  X(ld,  0xA4, NULL) \
  X(ldb, 0xA5, NULL) \
  X(st,  0xA6, NULL) \
  X(stb, 0xA7, NULL) \
  X(in,  0xBC, NULL) \
  X(out, 0xBD, NULL) \
  X(swi, 0xBE, NULL) \
  X(ext, 0xBF, NULL)

#define MK_ENUM(name, code, ...) \
  OP_##name = code,

enum Op {
  FOR_OP(MK_ENUM)
};

word_t imm32() {
  // TODO check that we aren't out of bounds for the code
  word_t out = be32(&code[ip]);
  ip += 4;
  return out;
}

word_t pop() {
  assert(sp >= 1); // check for stack underflow
  return stack[--sp];
}

word_t peekAt(uint8_t depth) {
  assert(sp >= depth); // check for stack underflow
  return stack[sp - depth];
}
word_t peek() { return peekAt(1); }

void saveAt(word_t val, uint8_t depth) {
  assert(sp >= depth); // check for stack underflow
  assert(depth >= 1); // use `push` to actually extend the stack
  stack[sp - depth] = val;
}

void push(word_t val) {
  assert(sp < sizeof(stack)/sizeof(word_t)); // check for stack overflow
  stack[sp++] = val;
}

/////////////////////////
////// Interpreter //////
/////////////////////////

uint8_t _exitCode = 0;
bool cycle() {
  // fetch
  if (!(ip < _codesize)) { fprintf(stderr, "ERROR ip=%xh\n", ip); }
  assert(ip < _codesize);
  uint8_t opcode = code[ip++];
  fprintf(stderr, "ip=%xh, op=%xh\n", ip, opcode); // DEBUG
  // decode TODO
  // execute
  if (opcode < 0x80) { // push unsigned immediate
    push((word_t){ .u = opcode });
  } else if (opcode >= 0xC0) { // push signed immediate
    push((word_t){ .i = (int8_t)opcode });
  } else if (opcode < 0x90) { // peek
    uint8_t depth = (opcode & 0xF) + 1;
    word_t val = peekAt(depth);
    push(val);
  } else if (opcode < 0xA0) { // save
    uint8_t depth = (opcode & 0xF) + 1;
    word_t val = pop();
    saveAt(val, depth);
  } else switch (opcode) {
    // arithmetic //
    case OP_add: {
      uint64_t b = pop().u;
      uint64_t a = pop().u;
      uint64_t r = a + b;
      carry = r > UINT32_MAX;
      push((word_t){ .u = r });
    } break;
    case OP_sub: {
      uint64_t b = pop().u;
      uint64_t a = pop().u;
      uint64_t r = a - b;
      carry = r > UINT32_MAX;
      push((word_t){ .u = r });
    } break;
    case OP_mul: {
      uint64_t b = pop().u;
      uint64_t a = pop().u;
      uint64_t r = a * b;
      carry = r > UINT32_MAX;
      push((word_t){ .u = r });
    } break;
    case OP_div: {
      uint64_t b = pop().u;
      uint64_t a = pop().u;
      uint64_t r = a / b;
      push((word_t){ .u = r });
    } break;
    case OP_mod: {
      uint32_t b = pop().u;
      uint32_t a = pop().u;
      uint32_t r = a % b;
      push((word_t){ .u = r });
    } break;
    case OP_or: {
      uint32_t b = pop().u;
      uint32_t a = pop().u;
      uint32_t r = a | b;
      push((word_t){ .u = r });
    } break;
    case OP_and: {
      uint32_t b = pop().u;
      uint32_t a = pop().u;
      uint32_t r = a & b;
      push((word_t){ .u = r });
    } break;
    case OP_xor: {
      uint32_t b = pop().u;
      uint32_t a = pop().u;
      uint32_t r = a ^ b;
      push((word_t){ .u = r });
    } break;
    case OP_shr: {
      uint32_t b = pop().u & 0x1F;
      uint64_t a = pop().u;
      uint64_t r = a >> b;
      push((word_t){ .u = r });
    } break;
    case OP_sar: {
      uint32_t b = pop().u & 0x1F;
      int32_t a = pop().i;
      int32_t r = a >> b;
      push((word_t){ .i = r });
    } break;
    case OP_shl: {
      uint32_t b = pop().u & 0x1F;
      uint64_t a = pop().u;
      uint32_t r = a % b;
      push((word_t){ .u = r });
    } break;
    case OP_imm: {
      assert(ip + 4 < _codesize);
      push(imm32());
    } break;
    // Jumps //
    case OP_j: {
      int32_t off = pop().i;
      lp = ip;
      ip += off;
    } break;
    case OP_ret: {
      int target = lp;
      lp = ip;
      ip = target;
    } break;
    case OP_jlt: {
      int32_t off = pop().i;
      word_t val = pop();
      if (val.i < 0) { ip += off; }
    } break;
    case OP_jle: {
      int32_t off = pop().i;
      word_t val = pop();
      if ((val.i < 0) | (val.i == 0)) { ip += off; }
    } break;
    case OP_jeq: {
      int32_t off = pop().i;
      word_t val = pop();
      if (val.u == 0) { ip += off; }
    } break;
    case OP_jne: {
      int32_t off = pop().i;
      word_t val = pop();
      if (val.u != 0) { ip += off; }
    } break;
    case OP_jge: {
      int32_t off = pop().i;
      word_t val = pop();
      if ((val.i > 0) | (val.i == 0)) { ip += off; }
    } break;
    case OP_jgt: {
      int32_t off = pop().i;
      word_t val = pop();
      if (val.i > 0) { ip += off; }
    } break;
    case OP_jnc: {
      int32_t off = pop().i;
      if (!carry) { ip += off; }
    } break;
    // TODO check carry flag
    // Memory //
    case OP_ld: {
      // addr --> addr+4 val
      uint32_t addr = pop().u;
      assert(4 <= addr && addr <= sizeof(heap) - 4);
      word_t val = *(word_t*)(&heap[addr]);
      push((word_t){ .u = addr + 4 });
      push(val);
    } break;
    case OP_ldb: {
      // addr --> addr+1 val
      uint32_t addr = pop().u;
      assert(4 <= addr && addr <= sizeof(heap) - 1);
      word_t val = (word_t){ .u = (uint8_t)heap[addr] };
      push((word_t){ .u = addr + 1 });
      push(val);
    } break;
    case OP_st: {
      // addr val --> addr+4
      uint32_t addr = pop().u;
      word_t val = pop();
      assert(4 <= addr && addr <= sizeof(heap) - 4);
      *(word_t*)(&heap[addr]) = val;
      push((word_t){ .u = addr + 4 });
    } break;
    case OP_stb: {
      // val addr --> addr+1
      uint8_t val = pop().u;
      uint32_t addr = pop().u;
      assert(4 <= addr && addr <= sizeof(heap) - 1);
      heap[addr] = val;
      push((word_t){ .u = addr + 1 });
    } break;
    // Special Transfers //
    case OP_slp: { // Swap Link Pointer
      int oldLink = lp;
      int newLink = pop().i;
      push((word_t){ .i = oldLink });
      lp = newLink;
    } break;
    case OP_pop: {
      pop();
    } break;
    case OP_xch: {
      word_t b = pop();
      word_t a = pop();
      push(a);
      push(b);
    } break;
    // Input/Output //
    case OP_in: {
      int byte = fgetc(stdin);
      if (byte == EOF) { byte = -1; } // yes, I am intentionally silently treating all errors as EOF, at least for now (not sure how the vm could possibly recover)
      push((word_t){ .i = byte });
    } break;
    case OP_out: {
      uint8_t byte = pop().u & 0xFF;
      fputc(byte, stdout);
    } break;
    case OP_swi: {
      uint32_t syscode = pop().u;
      // TODO send out to a syscall function
      assert(syscode == 0);
      _exitCode = pop().u;
      return true;
    } break;
    case OP_ext: {
      fprintf(stderr, "unimplemented: ext opcode\n");
      assert(false); // TODO
    } break;
  }
  return false;
}

_Noreturn
void cycles() {
  bool shouldStop;
  do {
    shouldStop = cycle();
  } while (!shouldStop);
  exit(_exitCode);
}

////////////////////
////// Loader //////
////////////////////

struct VmHeader {
  int version[3];
  int bytesize; // in bits
  int wordsize; // in bits
  size_t datasize; // in bytes
  size_t codesize; // in bytes
  // TODO might add heapsize, stacksize
};
bool parseHeader(FILE* fp, struct VmHeader /*out*/*hdr) {
  int nscanned;

  // 8-byte magic string and version info: /ksvm\d{3}\n/
  nscanned = fscanf(fp, "ksvm%1d%1d%1d",
      &hdr->version[0], &hdr->version[1], &hdr->version[2]);
  if (fgetc(fp) == '\n') { nscanned++; }
  if (nscanned < 4) {
    fprintf(stderr, "not a kiss vm bytecode file\n");
    return false;
  }
  // we only support v 1.0 in this interpreter
  if (hdr->version[0] != 1 || hdr->version[1] > 0) {
    fprintf(stderr, "unsupported vm version: %d.%d.%d\n",
        hdr->version[0], hdr->version[1], hdr->version[2]);
    return false;
  }

  // bits per byte and per word
  nscanned = fscanf(fp, "%2dx%4d",
      &hdr->bytesize, &hdr->wordsize);
  if (fgetc(fp) == '\n') { nscanned++; }
  if (nscanned < 3) {
    fprintf(stderr, "missing vm size requirements\n");
    return false;
  }
  // we only support 8-bit byte and 32-bit word
  if (hdr->bytesize != 8 || hdr->wordsize != 32) {
    fprintf(stderr, "unsupported vm size: %dx%d\n",
        hdr->bytesize, hdr->wordsize);
    return false;
  }

  // the rest of the header describes the bytecode payload
  // TODO we need a data section, I think ahead of the code section
  uint8_t sizes[16];
  nscanned = fread(&sizes, 1, 16, fp);
  assert(nscanned == 16);
  // first 2 32bit words are reserved
  // 3rd 32bit word is the size of the data (multiple of 4 bytes)
  hdr->datasize = be32(sizes + 8).u;
  assert(hdr->datasize % 4 == 0);
  // last 32bit word is the size of the bytecode
  hdr->codesize = be32(sizes + 12).u;

  // everything is loaded
  return true;
}

int main(int argc, char* argv[argc]) {
  // parse arguments/aquire system resources
  char* usageStr = "usage: vm <bytecode-file>";
  if (argc < 2) { fprintf(stderr, "%s\n", usageStr); return 1; }
  FILE* fp = fopen(argv[1], "rb");
  if (fp == NULL) { fprintf(stderr, "could not open file: %s\n", argv[1]); return 1; }

  // load header
  struct VmHeader hdr;
  if (!parseHeader(fp, &hdr)) {
    fprintf(stderr, "malformed bytecode file: %s\n", argv[1]);
    return 1;
  }
  // printf("v%i.%i.%i, %ix%i\n%zu\n", //DEBUG
  //     hdr.version[0], hdr.version[1], hdr.version[2],
  //     hdr.bytesize, hdr.wordsize,
  //     hdr.codesize);

  // load data
  (*(word_t*)heap).u = 0;
  fread(heap, 1, hdr.datasize, fp);
  assert((*(word_t*)heap).u == 0); // first four bytes of the data section needs to be zero
  (*(word_t*)heap).u = hdr.datasize; // so we set address zero to the address just after the data section, ie addr 0 hold the heap break

  // load bytecode
  code = malloc(hdr.codesize);
  assert(code);
  size_t nread = fread(code, 1, hdr.codesize, fp);
  assert(nread == hdr.codesize);
  _codesize = hdr.codesize;

  // begin execution
  ip = 0;
  sp = 0;
  cycles();
}


