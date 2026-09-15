# FIXME this is just for experimentation
  .data
  .align 8
f_main: # just so the linker doesn't die; once we compile forth, we'll be ok
  # .quad f__doRet # making _main a synonym for _ret
  .quad f__step
  .quad f__ret



# FORTH CALLING CONVENTION
#
# The virtual instruction pointer is %rsi
#   (so we can use lodsq for the guest's fetch).
# The virtual data stack pointer is %rsp
#   (so we can use the x64's push/pop for Forth's internal push/pop).
# The virtual return pointer is in, well, I guess I'll use %rbp

###### The Forth Interpreter(s) ######

  .text
  .global f__step
  .type f__step, @function
f__step:
  lodsq                # subrptr(rax) = *vip(rsi)++
  mov %rsi, (%rbp)     # save the now-updated vip as the return address
  lea 8(%rbp), %rbp    # | by pushing onto the vrp(%rbp)
  lea 8(%rax), %rsi    # vip(%rsi) = bodyptr = subrptr(rax) + 1
  jmp *(%rax)          # interpreter(rax) = *subrptr(rax)
                       # | xfer control to the interpreter

  .data
  .align 8
f__imm:
  .quad f__doImm
  .text
  .type f__doImm, @function
f__doImm:
  lea -8(%rbp), %rbp    # the "return address" is currently pointing to the immediate value
  mov (%rbp), %rsi      # | we'll pop it directly into the vip for now
  push (%rsi)           # | because we can put that value directly onto the data stack
  lea 8(%rsi), %rsi     # we can now skip over the non-instruction
  jmp f__step           # | and go directly to the main loop, because the return logic has already happened

### Internal primitives ###

  .data
  .align 8
f__ret:
  .quad f__doRet
  .text
  .type f__doRet, @function
f__doRet:
                         # the top of the vrp is garbage, so skip it
  lea -16(%rbp), %rbp    # | and instead get the return address from vrp's NOS
  mov (%rbp), %rsi       # put that return address  back into the vip
  jmp f__step            # return to the "main loop" now that we've restored the vip

###### Forth Entry and Exit ######

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

