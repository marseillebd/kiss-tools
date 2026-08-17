I've been making all sorts of ISAs, and spending a lot of time developing them.
Well, I think I have some real ideas about what is good or bad for implementation now.

# Goals

- **PRIMARY FUNCTION**: support operations needed for parsing source code, analyzing and manipulating ASTs, and emitting machine code.
- **CORE VALUE**: easy to write a virtual machine implementation
  (ie: decoding is simple and fast and opcodes can share code paths).
  This is the most core because implementations are expected to be hand-written native machine code.
- **CORE VALUE**: easy for a human to write and link both assembly and machine code by hand
  (ie: easy to pull instruction fields out of hex/octal, easy to calculate offsets in assembly language).
- _BONUS_: easy to implement in hardware
  (ie: relatively small register file, memory, few arithmetic operations)
- _BONUS_: easy to translate to native
  (ie: no fancy virtual hardware or instructions)
- **NON-GOAL**: encoding efficiency

# Inspirations

## Onramp VM

Uses 32-bit word and 32-bit instructions.
Addressing modes are 3-argument.
However, it only has 15 valid opcodes,
  and 16 registers, 
It also has no flags register.

What's good is that it allows easy encoding of registers, immediates, and register/immediate operands.
What might not be so good is that many common operations require long and/or unintuitive pseudoinstructions.
For example, while to negate an integer `neg dst x`, you would subtract from zero `sub dst 0 x`,
  but for a bitwise inversion `inv dst x`, you would need to subtract from -1 to counteract the increment from two's complement.
However, while the tiny number of registers might make programming it by hand tedious,
  it _does_ make it easy to implement the machine.
I think a table of pseudoinstructions would go a long way towards mitigating the issue,
  not to mention allowing multiple instructions per line of assembly so psuedoinstructions are more easily spotted.

What I don't like is the massive number of system calls available.
It's clearly a nod to the C library or POSIX, but there's no guarantee that there will be, say,
  program arguments available on some systems, especially raw hardware.
Indeed, I also don't like that there's an interrupt vector table; I think having syscalls as instructions is simpler to implement.
There's also a lot of metadata available that, frankly, should not be needed.
I'm not planning on writing alternate VM inplementations or programs that make use of extended capabilities.

What the OnrampVM shows is that it is fully possible to implement a bootstrap in only 16 instructions and 10 general-purpose registers.
It also shows a strong committment to making is easy to write hex: every interesting field is aligned to a 4-bit boundary.

### Some Details

Operands are either:
- register, encoded 0x8X with X identifying the register
- immediate, a single sign-extended byte
- mixed, which is like register in the range 0x80--8F, and like an immediate otherwise.
  (112 non-negative immediate, 128 negative immediates)
So-called special-purpose registers are `ra, rb, rsp, rfp, rpp, rip`, but they are just `r10--r15`.
Of these, I recognize `rsp, rfp, rip`; `ra, rb` are scratch space for the assembler to emit pseudoinstructions;
  `rpp` is the "program pointer" which points to the start of the program text.
I'm not really sure why `rpp` gets a dedicated register, except that you would need to read from it for certain position-independent loads.

Onramp has a single conditional instruction `jz`, which looks at a register and jumps if it is zero.
I am wondering if it woulf have been better to have a `c CC A B` instruction, where
  `CC` carries a condition mask,
  `A` and `B` are subtracted,
  and the jump occurs if the flags from the subtraction and'ed with the mask is non-zero.
Still no flags register required, and no scratch register needed (though the implementation is more intricate).
Under the condintioning scheme, an unconditional jump could be `add rip OFF rip`,
  which is the same as in the true OnrampVm,
  but the true OnrampVM also allows `jz 0 OFF`, where the gain is merely being able to jump longer distances without a scratch register.

## Lua

The Lua bytecode differs significantly from version to version.
I'm using documentation for Lua 5.3.

It has a 32-bit instruction, but I'm not sure about the size of the data path (it may be large enough for a double-precision floating point).
The opcode and operands are always at the same location in the word,
  with larger operands simply being a catenation of two or three positions.

Destination operands are encoded with one fewer bit than source operands because
  while source operands could be immediate, destinations cannot be.

It's instruction set is a fairly full collection of ordinary ALU and MMU stuff,
  control flow instructions,
  and some array- and object-oriented instructions.
What I see that's very interesting is the `eq,lt,le,test,testset` instructions,
  all of which conditionally skip the next instruction by simply incrementing the instruction pointer.
With an ordinary conditional jump, you need two operands to compare, then another (large immediate) operand for the target address.
Hardware often gets around this by using a flags register:
  one `cmp` instruction takes the two src operands,
  then the conditional jumps can dedicate operand space to the large immediate target/offset.
Lua flips this on its head:
  one instruction takes two operands and jumps _a known amount_,
  which is enough to skip an unconditional jump (with a large immediate),
  or indeed _any_ instruction.
This completely negates the need for instructions like x86's `cmov`,
  and makes conditioned instructions fully orthogonal to non-conditioned instructions.
I love it!

What I don't like is that it relies on an out-of-band linked list for the call stack,
  which isn't exactly 

## Synthesis

If we have "special-purpose" registers available just like general-purpose registers,
  then we only need a tiny number of opcodes: 16 will do.
On top of that, if we have a stack, we only need a tiny number of registers: 16 will do _again_.
Thus, every field can be a single nybble, and machine code writers need not do any hexadecimal arithmetic.

Fixed-width instructions or bust.
No immediates stored as a word after the instruction, no conditional prefixes count as part of an instruction.
This allows conditioning operations to simply jump `ip` a fixed distance.

Multiply and divide are included in these instruction sets, even though they could be implemented with shifts and compares.
Those units are more annoying to build hardware for, but they'd be much faster than simulating add/mul in VM space.
Signed instructions are not necessary, and can often be simluated very easily.

### My Own Ideas

Several systems considered are three-operand formats.
I think this _can_ be convenient for non-destructive updates,
  a non-destructive update can be as easy as a move followed by a two-operand instruction.

A system needs some input/output, if only a punch card reader + typewriter.
In fact, I think that's enough to perform compilation,
  even if having intermediate files might be handy for systems without the memory to keep temp files in memory.
Beyond that, it needs a way to halt, probably with an indication of success/fail (which can just be a light bulb).
The "operating system" during bootstrap can just be a human.
Even the program itself can be provided over stdin before the real stdin kicks in;
  or the hardware might have different levers for "load program" and "begin input".

I think a 16-bit, two-operand format with nybbles `opcode,addrmode,src,dst` might be a good one.
It will save on typing/reading, notably because you can probably keep four hex digits in your head at once.
- 16 opcodes, with no space left over
- addrmode would select between reg-reg, reg-imm
- in fact, addrmode could be one bit while src is 7, so it's not so bad to load larger amounts of memory
- loading a 32-bit immediate could be rough at 3 instructions (8+12+12), but how often do we need those?

### Pseudoinstruction Possibilities

Represented as two-operand, since the destination is usually not important.
First operand is the destination.

- `nop` - `add rN 0`
- `mov rA rB` - `mov rA 0; add rA rB`
- `neg rA` - `sub rA 0`
- `inv rA` - `sub rA -1`
- `xor rA B` - `mov ra rA; or rA B; and ra B; inv ra; and rA ra` (6 instructions)
- `push X` - `st [rsp] X; add rsp 1`
- `pop rA` - `sub rsp 1; ld [rsp]`
- `j X` - `add rip X` (so relative addressing only)

# Sketches

## Nybble Format

```
      1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
      5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
ds   | op    | dst   |r| imm/src     |
     | op    | dst   |0| imm         | ? only for mov?
     | op    | dst   |1| off | src   | ? so that I can read/write at sp +-n
scs  | op    | src   |r| cc  | i/src |
di8  | op    | dst   | imm8          |
sys  | op    | dst   |r| num | i/src |
```

Operations:
- 0-3 (sd): add, sub, mul, div (unsigned, no flags/carry register)
- 4-7 (sd): and, (n)or, shl, shr (unsigned shifts)
- 8-B (sd): ld, st, ldb, stb
- C-F: cCC (css), ldi (imm), mov (sd), sys (sys)
- as for the spare instruction goes, I'm really tempted by `mov`
R-bit:
- 1 to use a register, 0 to use an immediate
Offset?:
- a 3-bit signed immediate that is multiplied by four and added to the contents of the src register
- this let's me, say, `st sp [ip + 4]; j @func; nop; ret_to_instr;`
- I might decide to only mul by 2: I'm facing a choice of values in 6 to -8 or 12 to -16.
- Another option: multiply but use a bias, say, -12 to 16 `sext(off)+1*4`, but a bias means that the pure register isn't just the `1` nybble
- I see it esp used on ip, sp, fp, and sp, but only ip/sp would use negatives
CC:
- invalid (never), eq, lt, le, gt, ge, ne, invalid (always)
Sources:
- if `r` is set, then low nybble names a register
- if `r` is clear, then the field is sign-extended to a word and used directly as an operand.
Destinations:
- always one nybble, names a register
System Call:
- `num` names a system call number as an immediate:
  - 0: read (arg2 is register to store a byte or -1)
  - 1: write (arg1 is register to read a byte from)
  - 7: exit (arg1 is exit code from register)
- other system calls are technically unneeded, unless I perhaps allow one or more "tape" devices, or a linux "syscall" but maybe closer to posix
- I've left syscalls 8-F blank in case I decide to admit an r-bit.
Registers:
- r0-r9: general-purpose registers
- rA-rB: general-purpose, but reserved by the assembler, clobbered
- rC = fp (frame pointer), callee-save
- rD = sp (stack pointer), callee-save
- rE = ep (environment pointer, ie this/closure/"thread"), caller-save
- rF = ip (instruction pointer), caller-save into `sp`

FIXME: ugh! I'm constantly trying to test for ltz.
Perhaps `Cx00` can be for `x < 0`?

For short jumps, we can add or subtract an immediate,
  which means we don't need to sign-extend the imm src.
That'll give jumps of +-64 instructions.
Also, small signed "add"s of +-128.

I'm not super happy with how the `r`-bit conditions the second operand somewhat differently.
Nevertheless, the only thing where it's different is `cCC`, so it might not matter.

It's a bit gross that `ldi` can load 8 bits at a time,
  considering ordinary `mov` can load 7 bits!
Well, `ldi` also does a shift.
Loading a 4-byte word with `ldi` would take 8 bytes.
However, doing an ip-relative load with a jump would require 12 bytes.
(A 3-byte imm could still take 8 bytes to zero the target,
 but a 2-byte imm would only take 6 bytes.)

I might be able to get away with a single shift with signed amount.
However, I think that'd be more difficult to implement in hardware.

