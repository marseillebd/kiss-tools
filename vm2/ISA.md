# Virtual Hardware Components

## Memory

The VM has a flat memory model, accessible by byte (octet) or my 32-bit word.
The memory is organized into static data including program text, heap, and a register stack.
It is illegal to dereference (read or write) to address zero.
Otherwise, there need not be any further memory protection.[^no-mem-protec]
The layout is as follows:
- Static data and program text is loaded into the lowest memory addresses. The size of this is determined during program loading.
  There is no requirement to separate data from code.
- Above the program text is the heap, which grows upwards. The size of this is determined by the special register `hp`, and begines at zero.
- The register stack starts at the highest address and grows downwards. The size of this is determined by the special register `fp` and begins at 15 words.

[^no-mem-protec]: The Kiss VM is not meant to execute untrusted software, nor is it meant for particularly large programs. Thus, there is less need for memory protection than on a general-purpose machine.

## Register Stack

The register stack identifies a range of 15 words.
These words are identified as registers 1-15.
The base of the register stack is identified by the `fp` (frame poitner) register.
Register 1 is stored at the address held in `fp`, and each next-higher-numbered register is stored in the next-lower-word address.
Some instructions adjust frame poitner, and thus how registers address memory.

## Special Registers

The `ip` (instruction pointer) register holds the address of the next instruction to execute.

The `fp` (frame pointer), as mentioned before, determines the base of the register stack.

The `zr` (staging register) holds partially-loaded immediate values.
It's reasonable to refer to it as register zero, but:
- writes targeting `zr`/`r0` via ordinary operands are ignored
- most instructions reset `zr`/`r0` to zero
- certain instructions shift bits into `zr`/`r0` from the "left" (least-significant) bit
Thus, `zr`/r0` functions either as a zero register or to access otherwise-loaded immediate operands.

The `cr` (carry register) holds the results of arithmetic overflow.
Notably, it can store `1` (for carry), `-1` (for borrow), the high 32-bits of a multiply operation, or the 32-bit modulus after signed integer division.

The `lr` (link register) holds the address after the previously-executed call instruction.

The `hp` (heap pointer) register determines the maximum address of the heap.
The user must manually manage this register as their heap grows.
The machine will halt if the `hp` is ever greater than the `fp - 15`, which represents an overlap of the heap and stack.

# Instructions

## Instruction Encoding Cheatsheet

| Op | Mnem | Fmt            | _0   | _1   | _2   | _3   | _4   | _5   | _6   | _7   | _8   | _9   | _A   | _B   | _C   | _D   | _E   | _F   |
| 0_ | and  | src  src dst   |
| 1_ | or   | src  src dst   |
| 2_ | xor  | src  src dst   |
| 3_ | div  | src  src dst   |
| 4_ | add  | src  src dst   |
| 5_ | adc  | src  src dst   |
| 6_ | sub  | src  src dst   |
| 7_ | mul  | src  src dst   |
| 8_ |  *   | func src dst   |      | xch  | inv  | neg  | shl  | rol  | shr  | asr  | ldb  | stb  | ld_r | st_r | ldw  | stw  |      |      |
| 9_ |
| A_ |  *   | func arg arg   | hlt  | sys  | in   | out  | jal  | ret  | push | pop  |      |      |      |      |      |      |      | ext  |
| B_ |
| C_ |  *   | func src imm24 |      | jeqz | jltz | jlez | jgtz | jgez | jnez | j    |      | ceq  | clt  | cle  | cgt  | cge  | cne  |      |
| D_ |  *   | func src imm24 | jbc  | jbs  |      | jc   |      |      |      |      | cbc  | cbs  |      | cc   |      |      |      |      |
| E_ | li8  | dst imm8       |
| F_ | li12 | imm12

Instructions 9 and D are reserved, as are functions 0,E,F in instruction 8, functions 0,8,F in instruction A, functions 2,4--7,A,C--F in instruction B, and functions 6--E in instruction C.
System-specific extensions must be encoded using the `ext` instruction.

## Encoding Details

### Register Encoding

A register operand (marked `src` or `dst`) is a 4-bit number, $n$.

If $n \neq 0$, loading from the resiger results in the value stored in the word $[sp] + n - 1$.
Likewise, when a value is stored via such a register operand, that value is written to the word in $[sp] + n - 1$.

If $n = 0$, loading from the register results in the value stored in `sr`, after which `sr` is cleared before the instruction finishes execution.
When a value is written via such an operand, no memory or registers are altered.

## Instruction Details

All instructions are 16-bit (2-byte) aligned.

Most instructions are "true" instructions.
Non-true instructions are called "formal" instructions.

True instructions clear the `zr` after completion, but formal instructions may alter or preserve `zr`.

In the following instruction descriptions, instructions are true unless explicitly noted as formal.

The reason for this distinction is that all instructions can, strictly-speaking, be executed independently of any other.
However, formal instructions have the sole purpose of modifying the execution of true instructions.

The `li12` instruction, is able to load a small immediate into the `zr` prior to executing a true instruction,
  so the two in combination effectively encode an instruction with a non-zero immediate operand.
Two instances of `li12` allow 24 bits to be loaded into `zr`, so effectively 16-bit immediates.
Two `li12` instructions followed by a `li8` (the last is a true instruction) allow loading a full 32-bit value into a register, possibly `zr`.

The `c`-family instructions perform a test to determine if the next (true) instruction should be executed or skipped over.
They otherwise have no observable action, and so are useless without a true instruction afterwards (and possibly some `li12` instructions in-between).

### Binary ALU Instructions

Encoding:
```
| 16..12 | 11..8 | 7..4 | 3..0 |
| func   | src1  | src2 | dst  |
```

The `func` field encodes various binary operations as follows:

| encoding | mnemonic | operation                               |
| -------- | -------- | --------------------------------------- |
| 0        | `and`    | bitwise AND                             |
| 1        | `or`     | bitwise OR (inclusive)                  |
| 2        | `xor`    | bitwise eXclusive OR                    |
| 3        | `div`    | signed integer DIVision (rounding down) |
| 4        | `add`    | signed integer ADD                      |
| 5        | `adc`    | signed integer ADd with Carry           |
| 6        | `sub`    | signed integer SUBtract                 |
| 7        | `mul`    | signed integer MULtiply                 |

Operation:
- let $a = \texttt{src1}$, $b = \texttt{src2}$, and
  $c = a \oplus b$
- $\texttt{dst} \leftarrow c[31..0]$
- Set `cr`:
  - if $\oplus$ was `add` or `adc` and $c[63..32] \neq 0$,
    $\texttt{cr} \leftarrow c[63..32]$ (but if so, $c[63..32] = 1$),
  - if $\oplus$ was sub and $c[63..32] \neq 0$,
    $\texttt{cr} \leftarrow c[63..32]$ (but if so, $c[63..32] = -1$),
  - if $\oplus$ was `div`, then $\texttt{cr} \leftarrow a \mod b$, but
  - otherwise $\texttt{cr} \leftarrow 0$.

### Unary ALU Instructions

Encoding:
```
| 15..12 | 11 | 10..8 | 7..4 | 3..0 |
| 8      | 0  | func  | src  | dst  |
```

The `func` field encodes various unary operations as follows:
| encoding | mnemonic | operation               |
| -------- | -------- | ----------------------- |
| 1        | `xch`    | eXCHange registers      |
| 2        | `inv`    | bitwise INVerse         |
| 3        | `neg`    | signed integer NEGation |
| 4        | `shl`    | SHift Left              |
| 5        | `rol`    | ROtate Left             |
| 6        | `shr`    | unsigned SHift Right    |
| 7        | `asr`    | Arithmetic Sift Right   |

Function 0 is reserved.

### Memory Instructions

Encoding:
```
| 15..12 | 11 | 10..9 | 8   | 7..4 | 3..0 |
| 8      | 1  | func  | st? | arg1 | arg2 |
```

The `func` field encodes various unary operations as follows:
| encoding | load mnemonic | store mnemonic  | operation       |
| -------- | ------------- | --------------- | --------------- |
| 0        | `ldb`         | `stb`           | LoaD/STore Byte |
| 2        | `ldw`         | `stw`           | LoaD/STore Word |

TODO: If `st?` is set, we store the value from arg into srg. If clear, we load the value in sreg into arg.

Function 1 is used for special register instructions.
Function 3 is reserved.

TODO: arg1 holds the address, while arg2 holds the register src/dst.

### Special Register Instructions

```
| 15..12 | 11..9 | 8   | 7..4 | 3..0 |
| 8      | 0b101 | st? | arg  | sreg |
```

TODO: If `st?` is set, we store the value from arg into srg. If clear, we load the value in sreg into arg.

The `sreg` identifies various special-purpose registers as follows:

| `sreg` | special register | load mnemonic | store mnemonic | operation                      |
| ------ | ---------------- | ------------- | -------------- | ------------------------------ |
| 0      | `zr`             | `ldzr`        | `stzr`         | LoaD/STore Zero Register       |
| 1      | `cr`             | `ldcr`        | `stcr`         | LoaD/STore Carry Register      |
| 2      | `ip`             | `ldip`        | `stip`         | LoaD/STore Instruction Pointer |
| 3      | `lp`             | `ldlp`        | `stlp`         | LoaD/STore Link Pointer        |
| 4      | `fp`             | `ldfp`        | `stfp`         | LoaD/STore Frame Pointer       |

The `sreg` encodings 5--7 are reserved.
TODO: If, somehow, we end up with a stack pointer, I want that encoded as 5.
Special registers added by an extended implementation must be accessed via the `ext` instruction.

### Special Instructions

- HaLT: `0xA0x_` - exit with error code `x`
- SYStem: `0xA1xy` - call system handler x, passing arguments starting at register y
- read from standard INput device: `0xA2?d`: read a byte from stdin into register `d`, or -1 if at end of input
- write to standard OUTput devide: `0xA3s?`: write a byte to stdout from register `s`
- Jump And Link: `0xA4xy_yyyy` - decrement (grow) `fp` by `x`, store `ip` into `lr`, and add `y` (sign-extended) to `ip`
- RETurn: `0xA5xy` - increment (shrink) `fp` by `x`, and jump to address in `lr` if `y` is zero, or to the contents of `y`
- PUSH: `0xA6x?` - decrement (grow) `fp` by `x`. `?` is padding, but should be zero
- POP: `0xA7x?` - decrement (grow) `fp` by `x`. `?` is padding, but should be zero
- special instructions `0xA8--AE` are reserved
- EXTended instruction prefix `0xAFxx`:
  alters the instruction decoding, allowing access to additional instructions that are not specified in this document.
  Exactly what instructions are implementation-specified, or may be defined by future Kiss-VM specifications.

TODO: I've gotta figure out what system instructions there could be

### Jump Instructions

Encoding
```
| 31..29 | 28    | 27 | 26..24 | 23..20 | 19..0  |
| 0b110  | cond1 | 0  | cond2  | src    | offset |
```

TODO: when the condition applied to src is true, add 2 times signed offet to the instruction pointer.
  That allows jumping up to 2MiB away, which should be large enough for expected program texts.

The `cond` field, once reconstructed as `cond1:cond2`, identifies a condition as below:

| cond | mnemonic | condition      | operation                             |
| ---- | -------- | -------------- | ------------------------------------- |
| 1    | `jeqz`   | $x = 0$        | Jump if EQual to Zero                 |
| 2    | `jltz`   | $x < 0$        | Jump if Less Than Zero                |
| 3    | `jlez`   | $x \leq 0$     | Jump if Less than or Equal to Zero    |
| 4    | `jgtz`   | $x > 0$        | Jump if Greater Than Zero             |
| 5    | `jgez`   | $x \geq 0$     | Jump if Greater than or Equal to Zero |
| 6    | `jnez`   | $x \neq 0$     | Jump if Not Equal to Zero             |
| 7    | `j`      | true           | unconditional Jump                    |
| 0o10 | `jbc`    | $x[zr] = 0$    | Jump if Bit Clear                     |
| 0o11 | `jbs`    | $x[zr] \neq 0$ | Jump if Bit Set                       |
| 0o13 | `jc`     | $cr \neq 0$    | Jump if Carry, borrow, or modulus     |

Conditions 0, 0o12, and 0o14--0o17 are reserved.

### Condition Next Instruction

Encoding:
```
| 15..13 | 12    | 11 | 10..8 | 7..4 | 3..0 |
| 0b110  | cond1 | 1  | cond2 | src1 | src2 |
```

These instructions are formal instructions.

TODO: when the condition is true, continue execution as normal. Otherwise, advance the `ip` to the end of the next true instruction.

The `cond` field, once reconstructed as `cond1:cond2`, identifies a condition much like the jump instructions.
The details are repeated with appropriate alterations below:

| cond | mnemonic | condition      | operation                              |
| ---- | -------- | -------------- | -------------------------------------- |
| 1    | `ceq`    | $a = b$        | Condition on EQual                     |
| 2    | `clt`    | $a < b$        | Condition on Less Than                 |
| 3    | `cle`    | $a \leq b$     | Condition on Less than or Equal        |
| 4    | `cgt`    | $a > b$        | Condition on Greater Than              |
| 5    | `cge`    | $a \geq b$     | Condition on Greater than or Equal     |
| 6    | `cne`    | $a \neq b$     | Condition on Not Equal                 |
| 0o10 | `cbc`    | $x[zr] = 0$    | Condition on Bit Clear                 |
| 0o11 | `cbs`    | $x[zr] \neq 0$ | Condition on Bit Set                   |
| 0o13 | `cc`     | $cr \neq 0$    | Condition on Carry, borrow, or modulus |

Conditions 0, 7, 0o12, and 0o14--0o17 are reserved.

### True Load Immediate (`li8`)
```
| 15..12 | 11..8 | 7..0 |
| 0xE    | dst   | imm8 |
```

TODO:
  Shift `imm8` into the low bits of the `zr`, sign-extending if `zr` has not been loaded since the last true instruction.
  Then, move the contents of `zr` into `dst` and clear `zr`.

### Formal Load Immediate (`li12`)

Encoding:
```
| 15..12 | 11..8 | 7..0 |
| 0xF    | imm12        |
```

This is a formal isntruction.

TODO: Shift `imm12` into the low bits of the `zr`, sign-extending if `zr` has not been loaded since the last true instruction.


## Notable Reserved Instructions

Some gaps were left in the instruction encoding with some intention.
While the behavoir described in this section cannot structly be relied on, it is reasonable to assume some behavoir for these reserved instructions,
  _if_ an implementation actually treats them as I personally expect.

Jump with a zero function would be a "never-branch".
Likewise, condition with a zero function would never execute the next instruction,
  and with function 7 would always would.
Jump and condition with function `1|2` would test if the carry register was zero.
Special register with sreg 5 would target the stack pointer, if the architecture somehow gains one.

If multiple conditional (formal) instructions appear in sequence, if any is false, skip the next true instruction.
If enough `li12` instructions are issued, bits just fall off the high end of the zero register.


