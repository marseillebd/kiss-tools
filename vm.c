/* compile with:
gcc -std=c11 -pedantic -Wall vm.c -o vm
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

/////////////////////////
////// Interpreter //////
/////////////////////////

typedef union {
  uint32_t u;
  int32_t i;
} word_t;

word_t imm32(char* bc) {
  uint32_t bits = (uint32_t)bc[0] << 8*3
                | (uint32_t)bc[1] << 8*2
                | (uint32_t)bc[2] << 8*1
                | (uint32_t)bc[3] << 8*0;
  return (word_t){ .u = bits };
}

////////////////////
////// Loader //////
////////////////////

struct VmHeader {
  int version[3];
  int bytesize; // in bits
  int wordsize; // in bits
  // TODO might add heapsize, stacksize, codesize
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
  printf("v%i.%i.%i, %ix%i\n", //DEBUG
      hdr.version[0], hdr.version[1], hdr.version[2],
      hdr.bytesize, hdr.wordsize);

  // load bytecode
  size_t codesize; {
    long cur = ftell(fp); assert(cur >= 0);
    fseek(fp, 0, SEEK_END); // TODO error checking
    long eof = ftell(fp); assert(eof >= 0);
    fseek(fp, cur, SEEK_SET); assert(cur >= 0);
    codesize = eof - cur;
  }
  char* bytecode = malloc(codesize);
  assert(bytecode);
  size_t nread = fread(bytecode, 1, codesize, fp);
  assert(nread == codesize);

  // TODO begin execution
  printf("an integer: 0x%.8x\n", imm32(bytecode).u);
  return 0;
}


