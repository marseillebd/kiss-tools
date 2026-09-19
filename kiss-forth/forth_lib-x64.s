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

