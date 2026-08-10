#!/usr/bin/env python3

from dataclasses import dataclass
from typing import Dict, List
import sys


@dataclass
class Instruction:
    opcode: int
    mnemonic: str
    arguments: List[str]
    size: int
    formal: bool
    level: int

# Initialize byOpcode/byMnemonic from the primitive description in `instrs` module.
# This allows the machine to represent instructions however it wants,
#   while we are able to write a plain database to define those instructions.
def initInstrs():
    byOpcode: Dict[int, Instruction] = {}
    byMnemonic: Dict[str, Instruction] = {}
    import instrs
    for opcode, format, tags in instrs.instrs:
        fmtParts = format.split()
        tags = tags.split()
        extraSize = list(filter(lambda x: x[0] == '+', tags))
        extraSize = int(extraSize[0]) if extraSize else 0
        level = 0 if "hw" in tags else \
                2 if "later" in tags else \
                1
        instr = Instruction(
                opcode,
                fmtParts[0],
                fmtParts[1:],
                1 + extraSize,
                "formal" in tags,
                level,
                )
        byOpcode[instr.opcode] = instr
        byMnemonic[instr.mnemonic] = instr
    return byOpcode, byMnemonic
byOpcode, byMnemonic = initInstrs()

def signExtend(value: int, bits: int):
    signBit = 1 << (bits - 1)
    return (value & (signBit - 1)) - (value & signBit)

def bigEndian(bytes: List[int]) -> int:
    acc = 0
    for b in bytes:
        acc = (acc << 8) + b
    return acc

class Hlt(Exception):
    def __init__(self, exitCode):
        super().__init__(self)
        self.exitCode = exitCode

class VM:
    def __init__(self, text: bytes, *, memsize: int = 32*1024*1024, ip: int = 16, atCycle = None):
        # init i/o
        self.stdin = sys.stdin
        self.stdout = sys.stdout
        # init memory
        self.memory = bytearray(memsize)
        self.memory[16:len(text)] = text
        # init registers
        self.registers = 64*[0]
        self.specialRegs = [
            ip, # instruction pointer
            len(self.memory) - 1, # stack pointer
            0, # do I want a frame pointer? for like, on-stack allocation?? probs not
            0, # flags register
            0, # overflow register
            ]
        # init internal registers
        self.args0 = 3*(('i', 0),)
        self.args: List[tuple[str, int]] = list(self.args0)
        self.argn: int = 0 # which argument will be loaded next?

    @property
    def ip(self) -> int: return self.specialRegs[0]
    @ip.setter
    def ip(self, val: int): self.specialRegs[0] = val
    @property
    def sp(self) -> int: return self.specialRegs[1]

    def loadArg(self, ty, arg: int):
        self.args[self.argn] = (ty, arg)
        self.argn += 1

    def fetchArgs(self, instr: Instruction) -> List[int|None]:
        args: List[int|None] = []
        for i, argfmt in enumerate(instr.arguments):
            match argfmt:
                case "src":
                    ty, val = self.args[i]
                    if ty == 'r': val = self.registers[val]
                    args.append(val)
                case "dst":
                    ty, val = self.args[i]
                    if ty == 'i': val = None
                    args.append(val)
                case "+imm":
                    lo, hi = self.ip+1, self.ip+instr.size
                    val = list(self.memory[lo:hi])
                    val = bigEndian(val)
                    args.append(val)
        self.args = list(self.args0)
        return args

    def cycle(self):
        # debug
        self.atCycle()
        # fetch
        opcode = self.memory[self.ip]
        # prep r/imm6 operand
        if 0x80 <= opcode < 0xC0:  # register index
            self.loadArg('r', opcode - 0x80)
            self.ip += 1
        elif 0xC0 <= opcode: # small immediate
            self.loadArg('i', signExtend(opcode & 0x3F, 6))
            self.ip += 1
        # execute ordinary instruction
        else:
            instr = byOpcode[opcode]
            # ig I'm ignoring the instruction's support level for now
            # decode and fetch instruction operands
            args = self.fetchArgs(instr)
            # execute
            self.ip += instr.size
            getattr(self, instr.mnemonic)(*args)

    # override this method for some debugging
    def atCycle(self):
        if False: pass
        print(self.ip, hex(self.memory[self.ip]))

    def hlt(self, exitCode):
        raise Hlt(exitCode)


def main():
    vm = VM(b"\xCA\x50")
    try:
        while True: vm.cycle()
    except Hlt as hlt:
        exit(hlt.exitCode)

if __name__ == "__main__":
    main()
