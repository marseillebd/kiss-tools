import sys

def main():
    binary = sys.stdin.buffer.read()

    # output metadata
    magic = binary[:16]
    print(f"# kvsm bytecode v{int(magic[4:5].decode())}.{int(magic[5:6].decode())}.{int(magic[6:7].decode())}")
    print(f"# {int(magic[8:10].decode())}-bit byte, {int(magic[12:16].decode())}-bit word")
    binary = binary[16:]

    # decode sizes
    header = binary[:16]
    datasize = imm32(header[8:])
    codesize = imm32(header[12:])
    binary = binary[16:]

    # extract segments
    data, binary = binary[:datasize], binary[datasize:]
    code = binary[:codesize]
    assert(len(data) == datasize)
    assert(len(code) == codesize)

    if datasize:
        print()
        print(".data")
        for line in disdata(data):
            print(line)
    if codesize:
        print()
        for line in disasm(code):
            print(line)

def disdata(data):
    i = 0
    while i < len(data):
        if 0x20 <= data[i] <= 0x7F:
            asciibuf = []
            while i < len(data) and 0x20 <= data[i] < 0x7F:
                asciibuf.append(data[i])
                if data[i] == 0x27: asciibuf.append(data[i]) # escape single-quote
                i += 1
            asciibuf = bytes(asciibuf).decode()
            asciiend = []
            while i < len(data) and (data[i] in [0x0A, 0x00]):
                asciiend.append(data[i])
                i += 1
            asciiend = " " + bytes(asciiend).hex(' ') if asciiend else ""
            yield f".ascii '{asciibuf}'{asciiend}"
        else:
            bytebuf = []
            while i < len(data) and not 0x20 <= data[i] < 0x7F:
                bytebuf.append(data[i])
                i += 1
            while bytebuf:
                yield f".db {bytes(bytebuf[:16]).hex(' ')}"
                bytebuf = bytebuf[16:]






def disasm(code):
    i = 0
    while i < len(code):
        opcode = code[i]
        if opcode < 0x80:
            yield int(opcode)
        elif opcode < 0x90:
            yield f"dup {opcode & 0xF}"
        elif opcode < 0xA0:
            yield f"save {opcode & 0xF}"
        elif opcode == 0xAF:
            val = imm32(code[i+1:i+5])
            yield f"imm {val}"
            i += 5; continue
        elif opcode < 0xBF:
            func = opcode - 0xA0
            yield opnames[func]
        elif opcode == 0xBF:
            raise Exception("extended operations not supported")
        else:
            yield -(0x100-int(opcode))
        i += 1
opnames = [
    "add", "sub", "mul", "div", "ld" , "ldb", "st" , "stb"  ,
    "or" , "and", "xor", "mod", "shr", "sar", "shl", "imm32",
    "jnc", "jlt", "jeq", "jle", "jgt", "jne", "jge", "j"    ,
    "ret", "xlp", "pop", "xch", "in" , "out", "swi", "ext"  ,
]


def imm32(bytestr):
    val = 0
    for b in bytestr[:4]:
        val = val * 16 + b
    return val

if __name__ == '__main__':
    main()
