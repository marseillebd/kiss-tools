what am I bootstrapping?
- sexpr parser
- garbage collector (with fixed block of memory granted)
- intermediate representation
- a lisp interpreter

Nononono, what tools do I want?
- a vm that:
  - is easy to hand-implement
  - with a small set of very portable operations, so it can be easily compiled to native
  - is an easy compilation target
- a typed lisp interpreter (tree-walk) for writing compilers, which needs:
  - an sexpr parser
  - a garbage collector
  - lookup in an environment (symtab and linear search envs is probably the easiest?)
- libasm for various architectures (including the vm above), which needs:
  - integer arithmetic
  - bitshift/mask operations, sext/zext
  - big-enough integer library
- a compiler for a systems programming language targeting native code

I _don't_ need it to worry about different bitwidths.
I mean, maybe eventually, but that's more for retrofuturism.
Right now, I choose.......... 8-bit byte, 32-bit word.

I also don't need multi-file sources.
Or at least, I should just be able to `include` files.
Point is, I should just be able to feed a single "card stack" into the compiler.
You assemble the card stack yourself.

I'm _not_ exposing the implementations as libraries.
Not until later, anyway; the implementation is enough without also solving the library interface problem.

Ok, and what's my testing/verification plan?
- I need to actually hand-implement the vm (with no abstraction!) on a real architecture (x64? or virtualize a riscv?)
- but I don't need to implement it in machine code immediately; I can make a reference impl

How am I organizing this all?
- `<name>-<source>-<host>-<target>` for compilers
- `<name>-<host>` for interpreters
