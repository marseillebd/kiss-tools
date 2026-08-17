- Virtual Machine
  - `doc/`
    - [ ] ISA Specification
    - [ ] Executable Format Specification
    - `?` Posix Supplement (so that I could perhaps implement some coreutils in the vm?)
  - `bin/`
    - [x] Prototype Implementation (Haskell)
  - `src/`
    - [ ] Hosted Implementations (C or C3, possibly no stdlib)
    - [ ] Standalone Implementations (ie no OS)
  - [ ] Hardware Implementation
- Hex Tools
  - `doc/`
    - [ ] [`hexasm`](#hexasm) Specification
    - [ ] manpage
  - `bin/`
    - [x] prototype: [`hexasm.sh`](#hexasm.sh)
    - [x] prototype: hexdump (is `xxd`)
    - [x] `hexasm.bin`
  - `src/`
    - [x] `hexasm.hex`
- FRONTIER
  - `?` a symbol dump from hex format
  - `?` an instruction encoder (assembler w/o linking)
  - `?` somewhere I need to evaluate arithmetic expressions
  - `?` a linker, probably as part of an assembler
- LONG-TERM
  - a simple lisp interpreter (perhaps based off kernel/vau-calculus)
  - VM assembler as EDSL
  - native assemblers as EDSL, or perhaps just QBE
  - a lisp-to-native compiler, or perhaps just a C-like EDSL in the lisp
  - perhaps I should give types to my lisp? at least at some point

# Virtual Machine
# Hex Tools

## `hexasm`

Filter that converts an ascii hexadecimal format to binary.
A byte is generated for every two consecutive (case-insensitive) hexdigits.

Spaces, newlines, and underscores are skipped.
Strips comments that start with `#` up to end-of-line
Strips address assertions that start with `@` up to end-of-line.

Optionally, on encountering an address assertion,
  checks that the current offset is equal to the given address.
The address in an address assertion consists of 0-8 hexdigits
  immediately following the first `=` after the `@`.

Other characters, including unpaired hexdigits, result in unspecified behavior
  (probably skipping or erroring).

### `hexasm.sh`

Implemented in POSIX shell with `sed,tr,awk,xxd`.
It does not check address assertions.

### `hexasm.hex`

Implemented in the Kiss VM Executable, encoded in Kiss Hex Format.
It does check address assertions.
It does not use any side-channel for describing errors.
TODO: It should report different exit codes for different types of errors.
