#!/usr/bin/env python3

# I feel like even TOML has gotten bloated, to say nothing of JSON, and we don't even want to mention XML.
# Saw a Johnathan Blow clip, and while he's out of touch with programming, he's right about computation.
# So here's a human-readable config file format.

# The file is line-oriented, and there are four kinds of lines:
# - blank lines: do nothing
# - comment lines: do nothing
# - section lines: set the current section
# - key-value pairs: add a key-value pair to the current section (empty string can be a section)

# Great, how are they parsed?
# Well, everything is up to end-of-line.
# - blank lines are just `whitespace` (obvs).
# - comments are `whitespace? hash, anything up to end of line`.
# - sections are `open square bracket, one or more identifiers separated by dots, close square brackets, optional whitespace`.
# - key-value pairs are `optional whitespace, one or more identifiers separated by dots, whitespace, value until end of line`.
# And finally, 
# - an identifier matches the reges `[a-zA-Z0-9-]+`.
# That's it.

# What if I need spaces at the start of my value?
# You don't. Just implement a backslash-escaping scheme for values that might need it.
# How do I tell numbers apart from strings in values?
# You don't; if you expect a number use `atoi`, otherwise parse the string as-needed.
# What if I need multi-line strings?
# You haven't considered l10n and i18n. Use keys that index into translation files that hold multiline strings.
# Why dash and not not underscore in key names?
# Underscore needs you to hold shift; dash doesn't.
# How do I do nested data structures?
# Put dots in your section or key names.
# Why no equals sign? Ini files have equals signs!
# Why type a hard-to-reach key when you could type an easy to reach key?
# What if someone makes a key that's a number?
# Then they probably intend some sort of array, sparse or otherwise, and that's fine.
# What if someone needs a value that's an empty string.
# There are so many reasons you don't.

# Enough yapping, let's implement a parser.

import typing
from typing import Generator
import sys, os
from os import path
import re
from tempfile import NamedTemporaryFile


# This is the heart of the algorithm: 9 lines of python.
# Now all we have to do is hook it up to function that read/write files line-by-line.
_idregex = r"[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*"
def parse_line(line: str) -> None | tuple[str, str] | tuple[str] | bool:
    if re.fullmatch(r"\s*(#.*)?", line):
        # comment or blank
        return None
    if m := re.match(r"\[("+_idregex+r")\]\s*", line):
        # section
        return (m[1],)
    if m := re.match(r"("+_idregex+r")\s+(.+)", line):
        # key-value pair
        return m[1], m[2]
    return False

def parsei(filepath: str, logger: typing.Any) -> Generator[tuple[str, str, str]]:
    section = ""
    lineno = 0
    with open(filepath, 'r') as fp:
        for line in fp.readlines():
            line = line.removesuffix("\n")
            lineno += 1
            match parse_line(line):
                case (key, value):
                    yield (section, key, value)
                case (new_section,):
                    section = new_section
                case None:
                    continue
                case False:
                    logger.write(f"config file {repr(filepath)} line {lineno}: ignoring malformed line {repr(line)}\n")

# The entire parser is 23 lines of python.
# It's been a while since I wront Python, and it tool about 30 minutes.
def parse(filepath: str, logger: typing.Any = sys.stderr) -> dict[str, dict[str, str]]:
    acc = dict()
    for section, key, value in parsei(filepath, logger=logger):
        if section not in acc:
            acc[section] = dict()
        if key in acc[section]:
            logger.write(f"config file {repr(filepath)}: duplicate key {key} in section {section}, overwriting\n")
        acc[section][key] = value
    return acc

# And yeah, we're just skipping over syntax errors and moving on.
# Not only do we assume the config file writers are competent, but it was easy produce useful logs, even with dependency injection.
# Oh, I should note that the reason `logger` is `Any` type is because `sys.stderr` is itself a silly union type.

# Okay, but what about updating the config files while preserving comments?
# Well, it's pretty easy to find what needs to change.
def saveChanges(filepath: str, changes: dict[str, dict[str, str]]):
    section, lineno = "", 0
    new = NamedTemporaryFile(mode='w', delete=False, dir=path.dirname(filepath))
    with new:
        with open(filepath, 'r') as old:
            for line in old.readlines():
                lineno += 1
                line = line.removesuffix("\n")
                match parse_line(line):
                    case (key, _):
                        if key in changes.get(section, dict()):
                            m = re.match(r"("+_idregex+r")(\s+)", line)
                            if m is None: raise Exception()
                            key, ws = m[1], m[2]
                            new.write(key+ws+changes[section][key]+"\n")
                            continue
                new.write(line+"\n")
    tmppath = new.name
    os.replace(tmppath, filepath)

if __name__ == '__main__':
    from pprint import pprint
    file = sys.argv[1]
    config = parse(file)
    pprint(config)
    saveChanges(file, {"": {"a": "42"}})
