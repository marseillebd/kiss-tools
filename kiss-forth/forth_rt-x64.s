# FIXME this is just for experimentation
  .data
  .align 8
f_main: # just so the linker doesn't die; once we compile forth, we'll be ok
  # .quad f__doRet # making _main a synonym for _ret
  .quad f__step
  .quad f__imm, 70
  .quad f__imm, 1
  .quad f__imm, 0, f__imm, 2, f_plop
  # 0 1
  .quad f__imm, 137
  .quad f__imm, 42
  .quad f__imm, 1
  .quad f__imm, f_ifnz
  .quad f_call
  # 0 1 42||137
  .quad f__imm, 3, f_pick
  .quad f__imm, 10
  .quad f_drop
  .quad f__ret



# FORTH CALLING CONVENTION
#
# The virtual instruction pointer is %rsi
#   (so we can use lodsq for the guest's fetch).
# The virtual data stack pointer is %rsp
#   (so we can use the x64's push/pop for Forth's internal push/pop).
# The virtual return pointer is in, well, I guess I'll use %rbp

# FIXME there's no bounds-checking for the stack or the return stack

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
  .quad f__stop

  .align 8
f__stop:
  .quad f__doStop

###### Primitive Functions ######

# `ifnz` and `jump` together make it Turing-complete.
# May as well throw in `call` here as well.

  .data
  .align 8
  .global f_ifnz
f_ifnz:
  .quad f__doIfnz
  .text
  .type f__doIfnz, @function
f__doIfnz:
  lea -8(%rbp), %rbp        # forth primitive prologue restores a good `vip` and `rsp`
  mov (%rbp), %rsi          # |
  # p, c, a = POP, POP, POP; goto p ? c : a
  pop %rax    # the condition
  pop %rdx    # the consequent
  pop %rbx    # the alternate
  test %rax, %rax
  jnz f__doInfzThen
f__doIfnzElse:
  push %rbx
  jmp f__step               # forth primitive epilogue
f__doInfzThen:
  push %rdx
  jmp f__step               # forth primitive epilogue

# This would be valid code, but jump and call can be optimized for size,
# since their bodies are so similar.
# I'm keeping the un-optimized version for learning reference.
#
#   .data
#   .align 8
#   .global f_jump
# f_jump:
#   .quad f__doJump
#   .text
#   .type f__doJump, @function
# f__doJump:
#   lea -8(%rbp), %rbp        # forth primitive prologue restores a good `vip` and `rsp`
#   # mov (%rbp), %rsi        # | but vip gets overwritten later
#   # target = POP; vip = &target[1]; goto target
#   pop %rax              # get the jump target from the top of the stack
#   lea 8(%rax), %rsi     # set vip to the target's body
#   jmp *(%rax)           # xfer control directly to the target interpreter
#   # jmp f__step               # forth primitive epilogue is dead code
# 
#   .data
#   .align 8
#   .global f_call
# f_call:
#   .quad f__doCall
#   .text
#   .type d__doCall, @function
# f__doCall:
#   # lea -8(%rbp), %rbp        # forth primitive prologue restores a good `vip` and `rsp`
#   # mov (%rbp), %rsi          # | but this primitive sets up its own `vip` and re-uses the existing `rsp`
#   # target = POP; call target
#   pop %rax             # get the target subr from TOS
#   lea 8(%rax), %rsi    # set vip to target's body
#   jmp *(%rax)          # xfer control to the target's body

.data
  .align 8
  .global f_jump
  .global f_call
f_jump: .quad f__doJump
f_call: .quad f__doCall
  .text
  .type f__doJump, @function
  .type f__doCall, @function
f__doJump:
  lea -8(%rbp), %rbp        # forth primitive prologue restores a good `vip` and `rsp`
  # mov (%rbp), %rsi        # | but vip gets overwritten later in both `jump` and `call`
f__doCall:                  # | and `call` would otherwise restore the return stack that we just popped at the start of `f__doJump`
  pop %rax              # get the jump target from the top of the stack
  lea 8(%rax), %rsi     # set vip to the target's body
  jmp *(%rax)           # xfer control directly to the target interpreter
  # jmp f__step               # forth primitive epilogue is dead code

