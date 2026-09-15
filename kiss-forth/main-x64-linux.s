# This file defines primitive interpreters that interact with the x64 linux environment.

#   .data
#   .align 8
#   .global f__rstack
# f__rstack:
#   .skip 8 * 4096 # reserve a call depth of about four thousand


  .text
  .global f__linux2forth
  .type f__linux2forth, @function
# The calling convention for this is unusual.
# To execute the call, emit a jump.
# It clobbers __everything__, so be sure to save at least rsp, rbp.
# Control will be returned to the f__doStop label when Forth eventually exits.
f__linux2forth:
  push 42
  jmp f__doStop
                            # data stack is already initialized
#  mov f__rstack, %rcx # initialize control stack
#  mov f__start, %rsi  # initialize virtual instruction pointer
#  jmp f__step               # start the Forth interpreter
