# Kiss Forth Spec

Kiss Forth is a variant of Forth meant to:
- modernize the language, which means adjusting its ergonomics for broader appeal among early 21st-century programmers
- simplify defining control structures using functional programming techniques, as an experiment
- separate out the interpreter from the compiler, to make ahead-of-time compiled implementations easier to bootstrap

## The Execution Environment

Execution begins by calling `main` with an empty stack.
Returning from `main` with an integer will 

## Built-in Words

First, let's clarify some terminology:
- "built-in" means that it is provided by the Forth implementation
- "primitive" means that it is implemented in the host platform's language (assembly, C, &c)
Ultimately, the Forth programmer should not know or care whether a built-in is primitive or not, at least as far as functional equivalence is concerned.

### Control Flow

To create a Turning-coplete language, only `ifnz` and one of `jump, call` need be implemented.
(`jump` and `call` can implement each other using return stack manipulation functions.)
However, it should be possible on most architectures to share part of the implementation for `jump` and `call`.

Of course, these are not the most convenient, high-level control operators to work with.
TODO.

TODO: `while`, `doWhile`.
TODO: `reduce, map`.

#### `ifnz`

`(zero: a, nonzero: a, cond: int) -> a`

Pops condition, consequent, and alternate off the data stack.
If the condition is non-zero, the _consequent_ is pushed back onto the data stack.
Otherwise, the condition is zero,  and the _alternate_ is pushed back onto the data stack.

Note that, unlike other languages if-statements, the consequent and alternate are already reduced to values.
To branch between two execution paths, the consequent and alternate must be function pointers, and the result must then be called.
For example: `' onElse ' onThen someCondition ifnz call`.

#### `call`

`(...xs, f: xs -> ys) -> ys`

Pops TOS as a function to call, then calls it.
This implements what in other languages is called an unknown call.

#### `jump`

`(..., f: xs -> ys){ret: _ -> _} -> (...ys){}`

Like `call`, but implements a tail-call.
Thus, control does not return to after this function, but instead returns to the callee of the function the `jump` is executed from.

This function, in conjunction with `ifnz` is key to Turing-completeness.
Loops (iteration) is a subset of recursion, specifically tail-recursion,
  but without the guarantee of a tail call, recursion risks overflowing the (return) stack.
Since `jump` guarantees tail-call optimization, it is safe to implement iteration with it.

### Stack Manipulation

Most Forth tutorials focus on `dup, pop, drop, swap, rot`.
While they are common, and likely deserve to be built-in, they unfortunately do not capture all possible stack manipulations.
Combining `here` with `@, !` and loops _is_ enough to duplicate, remove and rotate at arbitrary depths in the stack.
This is very low-level pointer arithmetic, however, and potentially requires an interpreted loop.
Another primitive basis involves `drop, pick, roll` the obvious-but-not-standard `-roll`, and the non-standard `plop` which is something like the inverse of `pick`.

Something that this Forth does differently from other languages is one-indexing.

#### `drop`

`(x) -> ()`

TODO

#### `pick`

`(x_n, ..., x_2, x_1, n: int) -> (x_n, ..., x_1, x_n)`

Copies a value n-deep in the stack up to the top.

Note that
- `dup` is equivalent to `1 pick`
- `over` is equivalent to `2 pick`

#### `plop`

`(x_n, ..., x_2, x_1, y, n: int) -> (y, x_{n-1}, ..., x_1)`

Moves a value from the stack to overwrite whatever was n-deep in the stack.
Thus, this is the inverse of `pick` in the sense that `n pick n plop` is a no-op.

I would recommend `n pick f n plop` over `n roll f n -roll` because `pick/plop` are likely to
  have much more efficient implementations than `roll, -roll`.

#### `roll`

`(x_n, x_{n-1}, ..., x_2, x_1, n: int) -> (x_{n-1}, ..., x_1, x_n)`

TODO

Equivalencies:
- `swap === 1 roll`
- `rot === 2 roll`
- `nip === 1 roll drop`

#### `-roll`

`(x_{n-1}, x_1, x_n, n: int) -> (x_n, x_{n-1}, ... x_1)`

TODO

Equivalencies:
- `-rot === 2 -roll`
- `1 roll === 1 -roll`
- `swap === 1 -roll`
- `tuck === 1 pick 2 -roll`
- `tuck === swap 2 pick`

### Return Stack Manipulation

TODO: `R>, R<, R.dup, R.drop`
because I suspect it's less likely that you'll need to adjust the return stack as much

### Integer Arithmetic

TODO: `add, sub, umul, imul, udivmod, idivmod`.
TODO: `addc, subb`, umulw, imulw, udiv, idiv, umod, imod`.
TODO: `2*, 4*, 8*, 16*, ..., 2/, 4/, 8/, ...`.

### Logic

TODO: and, or, xor, zshr, sshr, shl, rot, rotc

### Bitwise

TODO: test/set/clear/toggle bit.
TODO: popcount, parity.

### Memory

TODO: `@, !`.
TODO: `here, R.here`, `base, R.base`?
TODO: memcpy/memmove.
TODO: dynamic memory allocation/free.

### Floating-Point Arithmetic (optional)

### Environment

TODO: bits/byte, bytes/word.
TODO: read/write stdin/stdout/stderr.
TODO: get progname, args, env.
TODO: open/read/write/seek/close files/tapes.
TODO: open/read/write/close block/stream devices.
