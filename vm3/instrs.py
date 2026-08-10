from typing import List

instrs: List[tuple[int, str, str]] = [
# opcode | mnemonic | flags
  (0x00, "add src src dst", "hw"),
  (0x02, "sub src src dst", "hw"),
  (0x04, "mul src src dst", "hw"),
  (0x06, "div src src dst", "hw"),
  (0x08, "or  src src dst", "hw"),
  (0x09, "xor src src dst", "hw"),
  (0x0A, "and src src dst", "hw"),
  (0x0C, "mov src dst"    , "hw"),
  (0x0E, "inv src dst"    , "hw"),
  (0x0F, "neg src dst"    , "hw"),
  (0x10, "shl src src dst", "hw"),
  (0x11, "asl src src dst", "hw"), # arithmetic shift left
  (0x12, "shr src src dst", "hw"),
  (0x20, "ldb src dst"    , "hw"),
  (0x21, "stb src dst"    , "hw"),
  (0x24, "ldw src dst"    , "hw"),
  (0x25, "stw src dst"    , "hw"),
  (0x30, "mov src dst"    , "hw"),
  (0x36, "ldr src dst"    , "hw"), # load special register
  (0x37, "str dst src"    , "hw"), # store special register
  (0x39, "lih dst +imm"   , "hw,+2"), # load immediate halfword
  (0x3A, "liw dst +imm"   , "hw,+4"), # load immediate word
  (0x50, "hlt src"        , "hw"),
  (0x52, "in  dst"        , "hw"),
  (0x53, "out src"        , "hw"),
  (0x58, "jal dst src"    , "hw"),
  (0x59, "ret src"        , "hw"),
  (0x5A, "pop src"        , "hw"),
  (0x5B, "psh src"        , "hw"),
  (0x61, "ceq src src"    , "formal hw"),
  (0x62, "cb  src src"    , "formal hw"),
  (0x63, "cbe src src"    , "formal hw"),
  (0x64, "ca  src src"    , "formal hw"),
  (0x65, "cae src src"    , "formal hw"),
  (0x66, "cne src src"    , "formal hw"),
  (0x6A, "clt src src"    , "formal hw"),
  (0x6B, "cle src src"    , "formal hw"),
  (0x6C, "cgt src src"    , "formal hw"),
  (0x6D, "cge src src"    , "formal hw"),
  (0x7A, "law +imm"       , "formal hw +4"), # load word into argument

  (0x01, "adc  src src dst", ""),
  (0x03, "sbb  src src dst", ""),
  (0x05, "imul src src dst", ""),
  (0x07, "udiv src src dst", ""),
  (0x13, "ror  src src dst", ""), # rotate right
  (0x22, "ldh  src dst"    , ""),
  (0x23, "sth  src dst"    , ""),
  (0x26, "ldd  src dst"    , ""),
  (0x27, "std  src dst"    , ""),
  (0x31, "xch  dst dst"    , ""),
  (0x38, "lib  dst +imm"   , "+1"),
  (0x3B, "lid  dst +imm"   , "+8"), # load immediate doubleword
  (0x51, "sys  imm src"    , ""),
  (0x57, "ext +extinstr"   , "prefix"),
  (0x70, "cbc src src"     , "formal"), # bit-clear
  (0x71, "cbs src src"     , "formal"), # bit-set
  (0x73, "cc  src"         , "formal"), # carry set
  (0x78, "lab +imm"        , "formal +1"), # load byte into argument
  (0x79, "lah +imm"        , "formal +1"), # load halfword into argument
  (0x7B, "lad +imm"        , "formal +1"), # load doubleword into argument

  (0x18, "seb  src dst"    , "later"), # sign extend byte
  (0x19, "seh  src dst"    , "later"), # sign extend halfword
  (0x72, "cnc src"         , "formal later"), # carry not set
]
