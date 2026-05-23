# What is Done?

```
/-------------------\
| C >-hand-> native |
|___         _______|
    | human |
    |_______|

/----\         /--------\
| vm |  ====>  |   vm   |
| C  |  human  | native |
|____|         |________|

/--------\        /-------------------\
| hexasm |  ===>  | hex >-hexasm-> vm |
|  hex   |  (me)  |______      _______|
|________|   vm          | vm |
                         |____|
```

## Kiss Virtual Machine
A stack-based bytecode.
To be as platform-independent as possible,
  other tools will run on/target this until cross-compilation.

- Written in C
- The expectation is that it is easily hand-compilable for any machine.
- Ofc, compiled with modern compilers right now.
- [ ] would love more debugger support

## Hex Assembler
More of a reverse hexdump, I guess.
It's here to enable text input of binary, along with comments.

At the moment, it is running 4x slower than python when assembling itself.

# What is Next?

I've got some options:
- a calling convention
- hex assembler with address assertions
- opcode to hex translation
- both at the same time: a full assembler
- debugger support in the VM (which obvs would be optional when hand-compiling)

The syntax for immediate values is maybe easy?
Just have `imm` and `db` keep takinng hex bytes (stopping on any other char except underscore).
Emit that many bytes directly.
If it's an `imm`, then we just have to check how may bytes we loaded, and if only one that the value fits, and them emit the imm32 opcode if needed.
Only thing is, I need to consume a negative sign as well, and adjust.
If it's a `db`, it doesn't emit any other opcode.

# What is Later?

An internal linker.
There's no need for an external linker until we start cross-compiling.

A garbage collector in the vm.
Let's say a gcobj is always 2x32bits.
- The first includes a tag
- cons has lowest three bits clear in word 1; car in word 1, cons in word 2
- (byte)strings have lowest bits `100`; length in word1, (non-moving) pointer to bytes in word2
- forwarding pointer has three lowest bits as `110`. Word 1 is the new location, Word2 undefined.
- 32-bit integers have low three bits `010`, payload in word2; I may include carry flags in high bits of word 1
- any other unboxed data has low bits `111`, possibly with additional tags in word1, and payload in word2


A high-power-to-complexity-ratio high-level language, like a lisp.
I'll want both an interpreter as well as a compiler to the vm.
It'll also require a rts, at least for the gc.
I suspect this will be the slowest.

A join-point calculus that compiles to native (asm or bin)
A x86-family assembler, starting with just the basics.
Other families are also options, but I have x86 rn.
I think having a standalone asm as well as a lib would be the most flexible, and I can do that with a lisp.

Hand-compile the vm to x86, as a proof-of-concept for its hand-compile feasibility.
In fact, I'll have to hand-assemble it as well.

System support libraries are,,, interesting.
- support block devices (open, read, write, seek, close), accessible via a filepath
- support stream devices (open, read or write, close), again accessible via filepath
- support for program aruguments, so that my programs can be redirected
- support for inspecting (and manipulating) the memory layout; I think basically just being able to reserve heap space
The vm isn't required to handle them (though there is space in `swi`).
These may not actually start existing until we can compile, which means the first lisp might be doing a _lot_ in memory.
That's not necessarily that good, but unless I require the vm to support some sort of file system, well, that's what I have.
Definitely requires some thoughy, because I might just implement files in the vm.

## What Will Not Be?

While C is a widespread language, it is not particularly simple.
The syntax quirks add complexity,
  the type system is complex to account for different architectures and numerical types,
  but it also has low expressive power,
  there's really no support for modularity,
  and its ability to put side-effects everywhere requires loads of analysis for even basic optimizations.
So, I'd rather design my own portable assembly, much more principled, powerful, and simpler.
That should be easier to implement than standards-compliant C.
A C ompiler can be built later, out of a language more suited to writing compilers.

I don't think I need environment variables.
In fact, environment variabels are a great way for a system to accidentially depend on settings/data that it shouldn't.

I don't need threads, as that's more a performace concern than a functionality concern.

Now, note that an arbitrary machine could have an arbitrary byte size.
For my own sanity, I'm going to assume an 8-bit byte.
If this turns out not to be the case, well: change the source code to use a different size of byte.
The question of transmitting binary data between an n-bit byte machine and an m-bit byte machine is out-of-scope as far as I'm concerned.
What point is there in using an 8-bit machine to generate 9-bit binaries if they can't be transferred to the target?
Even if they could, I suspect the transmitter would work just as well transmitting 9 bits from 2 octets as it would if the bits were all packed.
Finally, I would hope that in the event of a global software erasure, we would just pick up the UTF-8 standard from the ashes and work with that, which means we'd all agree on an 8-bit byte.

# Preliminaries

## T-Diagrams

I'm taking inspiration from tombstone diagrams. I've got some links for further reading:
- [Wiki](https://en.wikipedia.org/wiki/Tombstone_diagram) is likely to stay up, but isn't complete and doesn't show its practical application very well.
- These [lecture notes](https://arielortiz.info/s201013/tc2006/tombstone_diagrams.pdf) and [excercise](https://arielortiz.info/s201013/tc2006/tombstone_exercise.pdf) provide a fairly good practical demonstration, if you are able to fill in gaps the teacher just knows.
- [This person's notes](https://notes.mathiasandresen.dk/4-semester/SPO/02-tombstone-diagrams/) give a concise summary, and a little extra tidbit.

In short, we have four types of puzzle pieces:
1. Machines, which can execute code expressed in that machine's language.
2. Interpreters, written in one langauge, which can execute code expressed in another language.
3. Compilers, written in one language, which translates programs expressed in one language to programs expressed in another.
4. Programs, which are written in one language, and otherwise don't operate on programs.

As a quick example, consider running a Tetris program, written in Java, on an 86 machine.
We'll first need some puzzle pieces (which neatly illustrate all our types, what a coincidence!).
Obviously, we'll need an x86 machine (top-left), and the source code of Tetris (bottom-right).
We'll also need a way to compile and execute it, which is the Java SDK.
I've simplified that here to just the java compiler (bottom-left) and the java runtime environment (top-right).

```
 -----       _______
| x86 |      | JVM |
 \___/       | x86 |
             -------

---------------  ( Tetris )
| Java -> JVM |   | Java |
----| x86 |----   --------
    -------
```

Now, we work to combine them, making sure the peices match top-to-bottom and side-to-side, until we get the tetris program sitting on top a stack that ends with an x86 machine.
We see there are two instances of the tetris application, but expressed in different languages: Java and the JVM (bytecode).
We create the second instance using the compiler (which we run on the x86 machine), and run it on the JVM on the x86 machine.

```
( Tetris )           ( Tetris )
 | Java | Java -> JVM | JVM |
 -----------| x86 |----------
            -------   | JVM |
            | x86 |   | x86 |
             \___/    -------
                      | x86 |
                       \___/
```
