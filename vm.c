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
  size_t codesize;
} word_t;

// so, the bytecode is big-endian
word_t be32(uint8_t* bc) {
  uint32_t bits = (uint32_t)bc[0] << 8*3
                | (uint32_t)bc[1] << 8*2
                | (uint32_t)bc[2] << 8*1
                | (uint32_t)bc[3] << 8*0;
  return (word_t){ .u = bits };
}

// 16 registers are addressable by 4 bits, so a byte can hold two operands
// if compiling the vm code to native code, it's a small enough section of memory to reserve (16*4 = 64 bytes)
word_t vmregs[16];
// conventions:
// - all registers are caller-save
// - r16 is a link register: it is set on call instructions (and should therefore be saved when it's not a tail-call).
// - function.result possibilities:
//   - slow call: all arguments and results are on the stack
//   - use registers: r1, r2 for results, r3-r15 for arguments, arg/rets larger than 2 words are by-reference, extra ret/args are on the stack

size_t ip; // instruction pointer

// a stack, grows upwards
size_t sp;
word_t stack[16*1024/4]; // TODO hardcoded to be 16kiB for now

// an unstructured heap
char heap[2*1024*1024]; // TODO hardcoded to be 2MiB for now

// instruction ROM
uint8_t* code;

/////////////////////////
////// Interpreter //////
/////////////////////////

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

Instruction Formats:
- all instructions are 32-bit, organized into one byte of opcode and up to three argument bytes
- `src/dst` arguments are one-byte and follow the addressing modes (below)
- `imm16` is a single 16-bit immediate argument
- `r` is like `src/dst`, but must be a register or stack mode

Addressing Modes:

Operation Codes:
- Data transfer instructions are 00-1F.
  - 00 is an invalid instruction, because it's such an easy thing to accidentily run into
  - `imm{,i} dst, imm16` are zero-/sign-extended "load" immediates. They actually shift the dst left 16 bits and place the immediate into the 16 low bits.
  - `{dup,drop,swap}` are just Forth-like stack manipulations. TODO these probably could take an imm8 that specified depth in the stack
  - `{ld,st}{w,b} r, src, src` are load/store word/byte. The memory address is the sum of the two src operands.
  - `mov dst, src` is just `add dst, src, 0`
- Arithmetic and Logical instructions are 20-4F, maybe even 5F.
  - `20-27 dst, src, src` are ordinary arithmetic instructions.
    `cmp dst, a, b` sets `dst[0..2]` to `[a==b,a>b,a<b]`, which is useful in the conditional branch instructions.
    `neg dst, src` === `sub dst, 0, src`
  - `28-2F` are for high-precision arithmetic.
    Whenever an arithmetic instruction (add,sub,mul,div,adc,sbb) happens, a hidden overflow register is set with the overflow/underflow/modulus.
    This register is used for `adc,sbb`, ofc, and it can be transferred directly to a dst with `hml/mod` for "high result of multiply" and "modulus".
  - `30-37 dst, src, src` are for bitwise operations.
    `inv dst, src` === `xor dst, src, -1`.
  - `38-3F` are for bit manipulation functions.
    - `bit dst, src1, src2` sets dst to 1 if src1's src2th bit is set, or zero otherwise
    - `blit dst, src1, src2` mix bits from src1 and src2 if the dst bit is zero or one (respectively)
    - `mask dst, src, src` clears the destination and then sets the bits between (inclusive) the two sources (which should be in low-to high order).
    - logical/arithmetic shift left, shift right, and rotate left/right are exactly what you'd think
  - `40-47` are for "sideways" bit operations
    - `c{t,l}{z,s} dst, src` are count trailing/leading zero/set bits
    - `pcnt dst, src` counts set bits
    - `{bswp,bitr} dst, src` swaps/reverses the order of bytes/bits in a word, respectively
  - `58-5F` are currently unused, but expected to be ALU instructions
- `60-6F` are system operations
  - `{in,out} dst/src` are for character I/O. They create a whole word at a time, so can be used for ASCII or unicode, depending on the platform, and still have room for -1 to indicate end of file.
  - `brk` returns control back to the system/debugger. Without a debugger, it is expected to halt.
  - `swi` is "software interrupt, the details of which are to be worked out later.
- `70-7F` are for control flow
  - `70-77 src, imm16` are jumps.
    Generally, the low three bits of the src and opcode are anded, and if non-zero, the signed imm16 is added to the instruction pointer.
    `77` is an unconditional jump, even if the src is zero. Note that `70` would never jump, so it's a no-op.
  - `jal src, imm16` is a "jump and link` also known as a "call". The current ip (just after the instr) is saved in r16.
  - `jalc dst, src` is a compuled jump and link, saving the current ip in dst and jumping to the absolute address in src.
  - Since the codesize is not expected to be huge, I suspect jal will be able to execute any valid jump, but just in case, there's also the absolute one.
That's 53 opcodes in the low half, leaving 75 unallocated. Architectures like the 8086, 6502, and RiscV-IM are plenty functional with this many instrs or fewer.
I may add bitwise implies, a fused mul-add (just for running totals), perhaps an `lea`-type instruction.
There's even the possibility of having memcpy/move as part of the instruction set, since it's so well-used and it'd be a bit silly to implement it inside the vm.
Similar string operations, like setting to zero or strcmp might come into play as well.
There's an outside possibility that I take the `mux` instruction from MMIX, but that might be a bit heavyweight to implement in the basic set.

Advanced/Optional Operation Codes:
Instructions 80 and above are optional.
I suppose there'd need to be an extensions bitfield in the header for them.
I haven't quite worked out what should go here, on account of not having the basic opcodes implemented.
Nevertheless, they may consist of floating-point, extra comparisons, conditional instructions, utf8, some extra system calls (filling out libc/posix a bit more), and perhaps bcd.

|     | +0        | +1   | +2       | +3     | +4    | +5     | +6       | +7      |
| --- | --------- | ---- | -------- | ------ | ----- | ------ | ------   | ------- |
| 00  | (invalid) |      | imm      | immi   |       |        |          |         |
| 08  | dup       | drop | swap     |        |       |        |          |         |
| 10  | ldw       | ldb  | stw      | stb    |       |        |          |         |
| 18  |           |      |          |        |       |        |          |         |
| 20  | add       | sub  | mul      | muli   | div   | divi   | cmp      | cmpi    |
| 28  | adc       | sbb  |          |        |       |        | hmul/mod |         |
| 30  | xor       | or   | and      |        |       |        |          |         |
| 38  | bit       | blit | mask     | shl    | lshr  | ashr   | rotr     | rotl    |
| 40  | ctz       | cts  | clz      | cls    | pcnt  |        | bswp     | bitr    |
| 48  |           |      |          |        |       |        |          |         |
| 50  |           |      |          |        |       |        |          |         |
| 58  |           |      |          |        |       |        |          |         |
| 60  | in        | out  |          |        |       |        | brk      | swi     |
| 68  |           |      |          |        |       |        |          |         |
| 70  | (nop)     | jgt  | jeq      | jge    | jlt   | jne    | jle      | j       |
| 78  | jal       | jalc |          |        |       |        |          |         |
| 80  |           |      |          |        |       |        |          |         |
| 88  | utf8...   |      |          |        |       |        |          |         |
| 90  |           |      |          |        |       |        |          |         |
| 98  |           |      |          |        |       |        |          |         |
| A0  | floats... |      |          |        |       |        |          |         |
| A8  |           |      |          |        |       |        |          |         |
| B0  |           |      |          |        |       |        |          |         |
| B8  |           |      |          |        |       |        |          |         |
| C0  | bcd...    |      |          |        |       |        |          |         |
| C8  | min       | mini | max      | maxi   | clamp | clampi | bounds   | boundsi |
| D0  |           |      |          |        |       |        |          |         |
| D8  |           |      |          |        |       |        |          |         |
| E0  |           |      |          |        |       |        |          |         |
| E8  |           |      |          |        |       |        |          |         |
| F0  |           |      |          |        |       |        |          |         |
| F8  |           |      |          |        |       |        |          |         |

*/

#define FOR_OP(X) \
  X(hlt, 0, NULL) \
  X(swi, 1, NULL \
  X(imm32, 2, NULL) \
  X(imm8, 3, NULL) \
  X(out, 255, NULL) \
  X(in, 254, NULL) \
  // TODO sext byte to word, or perhaps have some better masking/extending/blitting ops
// TODO: more opcodes

#define MK_ENUM(name, code, ...) \
  OP_##name = code,

enum Op {
  FOR_OP(MK_ENUM)
};

word_t imm8() {
  // TODO check that we aren't out of bounds for the code
  return (word_t){ .u = code[ip++] };
}

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

void push(word_t val) {
  assert(sp < sizeof(stack)/sizeof(word_t)); // check for stack overflow
  stack[sp++] = val;
}

bool cycle() {
  switch (code[ip++]) {
    case OP_hlt: { return true; }
    case OP_imm8: { push(imm8()); } break;
    case OP_imm32: { push(imm32()); } break;
    case OP_out: { fputc(pop().u, stdout); } break;
  }
  return false;
}

_Noreturn
void execute() {
  bool shouldStop;
  do {
    fprintf(stderr, "ip=%zxh, op=%i\n", ip, code[ip]); // DEBUG
    shouldStop = cycle();
  } while (!shouldStop);
  exit(pop().u);
}

////////////////////
////// Loader //////
////////////////////

struct VmHeader {
  int version[3];
  int bytesize; // in bits
  int wordsize; // in bits
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
  uint8_t sizes[16];
  nscanned = fread(&sizes, 1, 16, fp);
  assert(nscanned == 16);
  // first 3 32bit words are reserved
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

  // load bytecode
  code = malloc(hdr.codesize);
  assert(code);
  size_t nread = fread(code, 1, hdr.codesize, fp);
  assert(nread == hdr.codesize);

  // begin execution
  ip = 0;
  sp = 0;
  execute();
}


