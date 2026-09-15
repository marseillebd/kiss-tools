# FIXME this is just for experimentation
  .data
f_main: # just so the linker doesn't die; once we compile forth, we'll be ok
  .quad f__ret

  .text
  .global f__step
  .type f__step, @function
f__step:
  push $42
  jmp f__doStop
f__imm:
f__ret:


# FORTH CALLING CONVENTION
#
# The virtual instruction pointer is %edi
#   (so we can use lodsq for the guest's fetch).
# The virtual data stack pointer is %esp
#   (so we can use the x64's push/pop for Forth's internal push/pop).
# The virtual return pointer is in 

  .bss

  .align 8
f__rstack:
  .skip 8 * 4096 # reserve a call depth of about four thousand

# we put definitions of subroutines in the data section so relocation can happen efficiently
# it does mean that genuine code ends up getting marked as writable though, hehehehehHEHEHEHEHEAAAAAA FIXME
  .data

  .align 8
  .global f__start
f__start:
  .quad f__step
  .quad f_main
  .quad f__imm
  .quad 0
  .quad f__stop

  .align 8
f__stop:
  .quad f__doStop

