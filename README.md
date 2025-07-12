The KISS acronym has been transcribed a lot of ways,
  but I think the most accurate today it "keep it stupid simple".
How simple should you keep it?
Imagine an average mechanic, under fire, with just a handful of tools, who needs to repair a jet fighter; can they do it?
That's how simple the engineer has to keep it.
The more intelligent and educated an engineer you are, the more difficult it is to meet this standard.

A modern development toolchain is incredibly complex.
If you didn't already have access to a working C compiler, how would you compile gcc?, or clang?
Is it an easy day's work to audit the codebase for any C compiler?
What about more modern, more feature-packed languages?

My target is to make a stupid simple toolchain, but that's not easy.
It's not a goal to be reached as quickly as possible.
It is my zen garden, to be enjoyed as it is worked on, in its forever-incomplete state.

Why?
Because I've long had a fascination with bootstrapping.
There's something wonderful about making advanced tools out of basic tools.

# The Zen of `kiss-tools`

1. Modularity is more important than performance.
2. Naive approaches are fast enough until proven otherwise.
3. Existing standards are too complex.
4. Why should cross-compiling be any more difficult than native?

A set of development tools:
- functional enough to support a self-hosting compiler for a full typed functional language
- simple enough to hack on, and hand-compile where necessary
- performant _enough_, just so you can't grab a cuppa every time you use them

# An Approach

I'm not committing to sticking with this, but here are some vague plans.

1. run time system: mainly a gc, but also a streamlined OS interface
2. functional language interpreter; simply-typed at first, and then dependently; then add template metaprogramming
3. generic linker
4. a portable assembler, probably based on [join points](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/join-points-pldi17.pdf)
5. a high-level systems programming language
6. port 1, 3, and 4 to the systems lang
7. implement a compiler for the functional language, targeting 5 & 6

While C is a widespread language, it is not particularly simple.
The syntax quirks add complexity,
  the type system is complex to account for different architectures and numerical types,
  but it also has low expressive power,
  there's really no support for modularity,
  and its ability to put side-effects everywhere requires loads of analysis for even basic optimizations.
So, I'd rather design my own portable assembly, much more principled, powerful, and simpler.
That should be easier to implement than standards-compliant C.
A C ompiler can be built later, out of a language more suited to writing compilers.

A linker is little more than a glorified calculator:
  find some offsets, do some basic arithmetic on them, and write the result into some bits.
A format for linkable files then, is just some definitions (symbols, offsets, expressions) and the unlinked binary.
It should come nowhere near the complexity of ELF, and should be useful for any platform.
If you want to support a "real" linker, just use this simple one to generate an ELF or whatever.

Different machines have different register sets, so a portable assembler can't give you access to them.
(At least differences in instruction set can mostly be simulated.)
But register allocation is fairly complex, so why not just use the stack for everything?
Yes, that sounds like Forth, and maybe 
