# Preliminaries

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

# Finding our Constraints

Recall that my goal is to:
- design a high-level language (suitable for implementing compilers in)
- implement a (cross-)compiler for it, and
- be able to execute that compiler on an arbitrary machine with relative ease
- without having to use another compiler.

All of the difficult is in those last two steps.
The end product, however, would constitute instructions on how to begin a new chain of bootstrapped development environments.
If, by some dark magic, all software interpreters and compilers were destroyed, this code would be the easiest way to rebuild our ability to compile and run code.
Alternately, it just serves as a way to compile gcc without having to use an older gcc, or hand-compile the first gcc.
Assuming I bother to implement a C compiler.

## Relative Ease

To ensure relative ease, we have to optimize two interacting dimensions: the number of times a human has to hand-translate something, and the difficulty of each translation.

If we optimized only for the number of times you hand-compile, we could just provide the source for a translator as in the figure below.
However, a compiler for a high-level language (including code generation and linking) is quite complex, and would be difficult to hand-compile.
In fact, it would barely be better than hand-compiling gcc itself, which is unacceptable.

```
| HLL -> Native |      | HLL -> Native |
-----| HLL |-----      ----| Native |---
     ------| HLL -> Native |---------
            -----| HLL  |--
                 --------
                 | HLL  |
                 | Hand |
                 --------
                 | Hand |
                  \____/
```

Instead, we need to distribute several source files which build on each other.
The first source files should accomplish two things: be simple (incl simple enough to hand-compile), and make it easier to compile the next set of source files.
These sources will likely build on each other in a chain.

This process has happened before:
    the first assemblers were assembled by hand,
    and then the first systems languages were written in assembly before being bootstrapped,
    after which it was easiest to implement scripting and high-level languages with systems languages.
While this historical process proceeded by targeting specific platforms (eg Automatic Relay Computer aka ARC, or the PDP-11), I do not have the luxury of platform-specificity.
Instead, I wish to remain agnostic to the underlying architecture, at least as much as possible.

Thus, I will write a portable VM, which will execute bytecode.
I will have to write bytecode to implement an assembler and linker, but this is tedious to do by hand.
Linking is the worst part, so I'll make the linker be generic over what it can link, so it can be used to link both VM and native binaries.
After that, an assembler is mostly there to translate memonics and include libraries.
Both of these should be relatively easy to write by hand (but ofc you wouldn't be able to tell if I "cheated" using external tools).

From here, I'm torn:
- a stripped-down systems language with a lisp-y syntax, then a gc, then a better parser and possibly typechecker would create a full systems language
- a gc and untyped lisp, in which the full systems language is relatively easy to build
- I suppose I should choose the next language to make it easy to write a VM -> Native translator in it; a lisp would require a gc, but a vm assembler really wouldn't
So, I've made my choice: a stripped-down systems language



# More Wrinkles

TODO: system interface
TODO: some basic libraries, ie data structures and control structures

No matter how much work we do to supply a human with source code for the toolchain, we can't provide a system library for an arbitrary machine.
Instead, we have to build our software to match a generic system interface, which must be small enough to work on a wide variety of machines, while also being large enough to be reasonably easy to program against.

Of course, we'll need some way to get the code onto the system (even if that means typing or toggling it in by hand).
And you'll need the system to be able to start processes.

So far, I think the system support libraries will have to:
- support block devices (open, read, write, seek, close), accessible via a filepath
- support stream devices (open, read or write, close), again accessible via filepath
- support for program aruguments, so that my programs can be redirected
- support for inspecting (and manipulating) the memory layout; I think basically just being able to reserve heap space

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

