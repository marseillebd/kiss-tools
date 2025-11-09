#include "../kiss-vm.h"
#include <stdbool.h>
#include <stdio.h>

char code[] = {
  0x20, // nop
  0x50, 0x00, 0x00, 0x00, 0x2A, // push 42
  0x7F // hlt
};
char stack[1024];
char data[1024];

int main(int argc, char *argv[]) {
  // TODO load real code
  vmInit( code, sizeof(code)
        , stack, sizeof(stack)
        , data, sizeof(data)
        );

  vmResult res = VM_OK;
  for (int i = 0; res == VM_OK; i++) {
    printf("Step %d\n", i);
    res = vmStep();
  }
  switch (res) {
    case VM_OK:
      printf("internal vm error: early vm loop termination\n");
      return 1;
    case VM_HLT:
      return vmExitCode();
    case VM_ILL:
      printf("illegal vm opcode\n");
      return 1;
    case VM_SEG:
      printf("vm segfault\n");
      return 1;
    default:
      printf("internal vm error: unrecognized vm status %d\n", res);
      return 1;
  }
}
