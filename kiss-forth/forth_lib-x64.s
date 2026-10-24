# This file contains implementations of primitive Forth subroutines
# in x64 assembly. Thus, this is a file that would need to be ported
# from machine to machine. We implement fewer primitives than are
# specified because of this need to port. Additional primitives are
# implemented in `forth_lib.f` which depends on this asm library.
# Thus, many of the specified primitives need not be ported by hand,
# but can be compiled with an existing forth implementation.

  .data
  .align 8
  .global f_drop
f_drop:
  .quad f__doDrop
  .text
  .type f__doDrop, @function
f__doDrop:
  lea -8(%rbp), %rbp        # forth primitive prologue restores a good `vip` and `rsp`
  mov (%rbp), %rsi          # |
  # rsp--
  lea 8(%rsp), %rsp    # b/c the stack in x64 grows down/shrinks up
  jmp f__step               # forth primitive epilogue

  .data
  .align 8
  .global f_pick
f_pick:
  .quad f__doPick
  .text
  .type f__doPick, @function
f__doPick:
  lea -8(%rbp), %rbp        # forth primitive prologue restores a good `vip` and `rsp`
  mov (%rbp), %rsi          # |
  # PUSH vsp[-POP]
  mov (%rsp), %rax            # obtain `n`
  mov (%rsp, %rax,8), %rax    # obtain the value from deep in the stack
  mov %rax, (%rsp)            # | and replace `n` on the stack with it
  jmp f__step               # forth primitive epilogue

  .data
  .align 8
  .global f_plop
f_plop:
  .quad f__doPlop
  .text
  .type f__doPlop, @function
f__doPlop:
  lea -8(%rbp), %rbp        # forth primitive prologue restores a good `vip` and `rsp`
  mov (%rbp), %rsi          # |
  # n = POP; x = POP; vsp[-n+1] = x
  pop %rdx                      # obtain depth
  pop %rax                      # obtain value
  mov %rax, -8(%rsp, %rdx,8)    # write deep into the stack
  jmp f__step               # forth primitive epilogue

  .data
  .align 8
  .global f_roll
f_roll:
  .quad f__doRoll
  .text
  .type f__doRoll, @function
f__doRoll:
  lea -8(%rbp), %rbp        # forth primitive prologue restores a good `vip` and `rsp`
  mov (%rbp), %rsi          # |
  # n = POP; tmp = vsp[-n]
  # for i from n thru 2
  #   vsp[-i] = vsp[-i+1]
  # vsp[-1] = tmp
  pop %rcx                    # obtain depth
  mov (%rsp, %rcx,8), %rax    # save low element of stack
                              # | for later insertion at the top
  # The idea here is to walk from the bottom of the stack to the top,
  # replacing each lower value with the next higher.
  # At the end, our saved tmp will go at the top.
rollLoop:
  mov -8(%rsp, %rcx,8), %rdx  # top-wards values are lower in memory on x86
  mov %rdx, (%rsp, %rcx,8)    # | and here's the bottom-wards location
  dec %rcx                    # decrement
  jrcxz rollLoopEnd            # exit loop when our offset from the top is zero
  jmp rollLoop                # or else just carry on
rollLoopEnd:
  mov %rax, (%rsp)            # and finally put our saved value on top
  jmp f__step               # forth primitive epilogue
