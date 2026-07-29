The KISS acronym has been transcribed a lot of ways,
  but I think the most accurate today it "keep it stupid simple".
How simple should you keep it?
Imagine an average mechanic, taking cover from weapons fire, with just a handful of tools, who needs to repair a jet fighter; can they do it?
That's how simple the engineer has to keep it.
The more clever an engineer you are, the more difficult it is to meet this standard.

A modern development toolchain is written by incredibly clever people, and is accordinly extraordinarily complex.
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

1. Avoid existing precompiled software.[^1]
2. Naive approaches are fast enough until proven otherwise.
3. Existing standards are too complex.
4. Why should cross-compiling be any more difficult than native?

[^1]: Within the limits of the hardware I have. I'm ofc going to use my existing linux setup, but I should in principle be able to to all this by hand with a switchpanel.

# The Target

A set of development tools:
- functional enough to support a self-hosting compiler for a high-level language(s)
- simple enough to hack on as a routine matter
- simple enough to compile by hand
- performant _enough_, just so you can't grab a cuppa every time you use them
- enough to do systems programming (writing a gc), scripting, and applications development (typed, garbage collected)

The end product would constitute physical instructions on how to cold-start a new chain of bootstrapped development environments.
This process has implicitly happened before:
    the first assemblers were assembled by hand,
    and then the first systems languages were written in assembly before being bootstrapped,
    after which it was easiest to implement scripting and high-level languages with systems languages.
While this historical process proceeded by targeting specific platforms (eg Automatic Relay Computer aka ARC, or the PDP-11), I do not have the luxury of platform-specificity.
Instead, I wish to remain agnostic to the underlying architecture, at least as much as possible.
