from typing import List

instrs: List[tuple[int, str, str]] = [
# opcode | mnemonic | flags
  (0x00, "mov src dst"    , "hw"),
  (0x01, "inc src dst"    , "later"),
  (0x02, "inv src dst"    , "hw"),
  (0x03, "neg src dst"    , "hw"),
  (0x04, "or  src src dst", "hw"),
  (0x05, "xor src src dst", "hw"),
  (0x06, "and src src dst", "hw"),
  (0x07, "bit src src dst", ""), # extract bit
  (0x08, "add src src dst", "hw"),
  (0x09, "adc src src dst", ""),
  (0x0A, "sub src src dst", "hw"),
  (0x0B, "sbb src src dst", ""),
  (0x0C, "mul src src dst", "hw"),
  (0x0D, "iml src src dst", ""), # integer multiply
  (0x0E, "div src src dst", "hw"),
  (0x0F, "idv src src dst", ""), # integer divide
  (0x14, "shl src src dst", "hw"),
  (0x15, "asl src src dst", "hw"), # arithmetic shift left
  (0x16, "shr src src dst", "hw"),
  (0x30, "ldb src dst"    , "hw"),
  (0x32, "ldw src dst"    , "hw"),
  (0x34, "stb src dst"    , "hw"),
  (0x36, "stw src dst"    , "hw"),
  (0x3E, "ldr src dst"    , "hw"), # load special register
  (0x3F, "str dst src"    , "hw"), # store special register
  (0x39, "lih dst +imm"   , "hw,+2"), # load immediate halfword
  (0x3A, "liw dst +imm"   , "hw,+4"), # load immediate word
  (0x50, "hlt src"        , "hw"),
  (0x52, "in  dst"        , "hw"),
  (0x53, "out src"        , "hw"),
  # TODO there's a chance I allow in/out to be parameterized by a channel id
  (0x58, "jal dst src"    , "hw"),
  (0x59, "ret src"        , "hw"),
  (0x5A, "pop src"        , "hw"),
  (0x5B, "psh src"        , "hw"),
  (0x61, "ceq"            , "formal hw"),
  (0x62, "cb"             , "formal hw"),
  (0x63, "cbe"            , "formal hw"),
  (0x64, "clt"            , "formal hw"),
  (0x65, "cle"            , "formal hw"),
  (0x66, "cne"            , "formal hw"),
  (0x6A, "cae"            , "formal hw"),
  (0x6B, "ca "            , "formal hw"),
  (0x6C, "cge"            , "formal hw"),
  (0x6D, "cgt"            , "formal hw"),
  (0x7A, "aw +imm"        , "formal hw +4"), # load word into argument

  (0x17, "ror src src dst", ""), # rotate right
  (0x31, "ldh src dst"    , ""),
  (0x33, "ldd src dst"    , ""),
  (0x35, "sth src dst"    , ""),
  (0x37, "std src dst"    , ""),
  (0x38, "lib dst +imm"   , "+1"),
  (0x3B, "lid dst +imm"   , "+8"), # load immediate doubleword
  (0x3C, "xch dst dst"    , ""),
  (0x51, "sys imm src"    , ""),
  (0x57, "ext +extinstr"   , "prefix"),
  (0x78, "ab +imm"         , "formal +1"), # load byte into argument
  (0x79, "ah +imm"         , "formal +1"), # load halfword into argument
  (0x7B, "ad +imm"         , "formal +1"), # load doubleword into argument

  (0x18, "bit src src dst", "later"), # set dst to the value of bit src1 from src2
  (0x19, "sxt src src dst", "later"), # sign extend starting from bit named in src1
  (0x1A, "bic src src dst", "later"), # clear bit src1 in src2
  (0x1B, "bis src src dst", "later"), # set bit src1 in src2
  (0x10, "ctz src dst"    , "later"),
  (0x11, "cts src dst"    , "later"), # count trailing set (ones)
  (0x12, "clz src dst"    , "later"),
  (0x13, "cls src dst"    , "later"), # count leading set (ones)
  # TODO pct popcount, par parity
]

# For operations:
# - perform 32-bit operation, with result held in 64 bits
# - if all bits are zero, that's the zero flag ZF
# - if the 31st bit is set, that's the sign flag SF
# - if any high bits are set, that's the carry flag CF
# - if any high bit differs from bit 31, that's overflow OF
# In python, we just do the operation twice (first on the unsigned values, then on sign-extended ones)
# - ZF if the unsigned (signed?) result is zero
# - SF if the signed result is negative
# - CF if the unsigned result is gte 2^32
# - OF if the signed result is gte 2^31 or less than -2^31
# I've reserved four bits for conditions:
# ```
# bits   target   formula         aka     swapped
# ----   ------   -------         ---     -------
# 0001   true     ZF              eq      EQ
# 0010   true     CF              b c     ae nc
# 0011   true     ZF | CF         be      a
# 0100   true     OF != SF (LT)   lt      ge
# 0101   true     ZF | LT         le      gt
# 0110   true     CF | LT         ne      NE
# 0111   true     ZF | CF | LT    always
# 0000   true                     never
# ```
# ```
# bits   target   formula         aka     swapped
# ----   ------   -------         ---     -------
# 0001   true     ZF              eq      EQ
# 0010   true     CF              b c     ae nc
# 0011   true     ZF | CF         be      a
# 0100   true     OF != SF (LT)   lt      ge
# 0101   true     ZF | LT         le      gt
# 0110   true     CF | LT         ne      NE

# 1010   false    CF              AE NC
# 1011   false    ZF | CF         A
# 1100   false    OF != SF (LT)   GE
# 1101   false    ZF | LT         GT
# ```
# Well, two things:
# - I think the condition instruction should not take arguments, just test flags.
#   That way, we can add and then cc=cb j @foo.
# - I'm torn on whether to allow inverted, or just require the previous instr's arguments to flip.
#   I suppose flipping arguments doesn't change CF for add.
#   At the same time, I'd have just 10 opcodes used in a row of 16 unless I wanted to add branches to the decode.
# TODO For now, I've answered c takes no args and encode inverted condiions.

if __name__ == "__main__":
    Hex = "0123456789ABCDEF"
    tab = {}
    for opcode, format, flags in instrs:
        tab[opcode] = (format.split()[0] + "   ")[:3]
    # header
    line = (" _"+Hex[lo] for lo in range(0,16))
    line = " | ".join(line)
    print("|    | "+line+" |")
    print("| -- "+ 16*"| --- " + "|")
    # body
    for hi in range(0, 16):
        line = (tab.get(hi*16+lo, "   ") for lo in range(0,16))
        line = " | ".join(line)
        line = "| "+Hex[hi]+"_ | "+line+" |"
        print(line)
    exit()
