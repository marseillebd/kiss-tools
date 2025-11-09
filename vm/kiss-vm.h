#ifndef KISS_VM_H
#define KISS_VM_H

// Set up the initial code and data segments of the VM,
// and set the VM's special registers to their initial state.
// All sizes are in number of bytes.
void vmInit(
  char *code, unsigned int code_sz,
  char *stack, unsigned int stack_sz,
  char *data, unsigned int data_sz
);

typedef enum vmResult {
  VM_OK,
  VM_HLT, // halt
  VM_ILL, // illegal opcode
  VM_SEG, // segmentation fault (access to code/stack/data that is out of bounds
} vmResult;

// executes a single VM instruction
vmResult vmStep();

// retrieves the exit code, which is only valid after `VM_HLT` is received
int vmExitCode();

#endif
