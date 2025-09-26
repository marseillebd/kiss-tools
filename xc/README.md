This will translate files between binary files and human readable representations.

Right now, there's just a program that converts a C source file into markdown.
It's possibly the simplest way to do literate programming in the modern age.

Actually, I started writing it in C, but really it's such a simple program that I'm going to start again in Python.
After all, the real language doesn't matter as long as it's easy to translate into the bootstrapped kiss tools.

# Concept

The idea is to be able to "comfortably" read any bit pattern (not just byte patterns!).
The format will be something like:

```
digit: octal
wordSize: 3
endian: big
lineSize: 8
============
123 456 701
```

- `digit`: octal, hex, and others are allowed values
- `wordSize`: determines where spaces should go (in digits) when converting from binary to text.
- `endian`: what order the digits in a word are given
- `lineSize`: number of words per line

`lineSize` and `wordSize` are only required when converting to text. If they are specified, they could be used for linting.

## Convert to binary

Basically, I'm just going to read in the properties, spot the magic equals signs, and enter a simple state machine.
Read in bits from characters until we have enough to write a word.
Ignore any characters that aren't digits.

I'm genuinely uncertain how to write a nine-bit file out on an 8-bit machine/filesystem in such a way that it can be used without conversion on a 9-bit machine.
I figure I may just zero-pad anything that doesn't fill a full byte, then print info about the number of real vs padding bits written.

## Convert from binary

Same thing, except the properties have to be specified out-of-band.
It can take the same format as the text file.
There might need to be a "padding bits" property in there too, or specified on a command line.

Yeah, read in bits and output them until we don't have enough to form a full character.
When there are too few bits to read a full character, we just drop them (they were probably padding).
We probably want to stderr that there were padding bits, though.
