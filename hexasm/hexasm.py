import sys, re, os

for line in sys.stdin.readlines():
    line = re.sub(r'[#@;].*$', '', line)
    line = re.sub(r'[^0-9a-fA-F]', '', line)
    while line:
        n = int(line[:2], base=16)
        sys.stdout.buffer.write(n.to_bytes(1, 'big'))
        line = line[2:]
