import re

def main():
    # - [ ] TODO: parse cli arguments
    # For now, we're just going to jump to some experimental code
    file = """
    digit: hex
    wordbits: 8
    defaultformat: le
    ---
    be: 15-0
    le: 7-0 15-8
    octet: 7-0
    it = 3 + 2 * (2+3)
    ---
    {2}
    one: {two - one.start} one.start: 01 02 03 04 05
    two: {end-two.start} two.start: 00 01 01 02 03 05 08 {octet; it}
    end:
    """
    from io import StringIO
    with StringIO(file) as fp:
        asm = parse_file(fp)
    asm.eval()
    asm.patch()
    print(repr(asm))
    asm.print_words()
    return
    # - [ ] TODO: parse and stream a rewrite through the body of the binary

# # File Format

class Asm:
    def __init__(self):
        self.digit = None
        self.wordbits = None
        self.defaultFormat = None
        self.formats = dict()
        self.exprs = dict()
        self.vars = dict()
        self.words = []
        self.patches = []

    def finalize(self):
        if self.digit is None:
            raise Exception()
        if self.wordbits is None:
            raise Exception()
        self.WORD_REGEX = [
            None,
            r'[01]', # binary
            r'[0-3]', # quaternary?
            r'[0-7]', # octal
            r'[0-9A-F]', # hexadecimal
        ][self.digit] + '{' + str(self.wordbits//self.digit) + '}'
        self.WORD_REGEX = re.compile(self.WORD_REGEX)
        if self.wordbits % 4 == 0:
            self.wordstr = hex
        elif self.wordbits % 3 == 0:
            self.wordstr = oct
        else:
            raise Exception()

    def __repr__(self):
        return f"exprs: {self.exprs}\nvars: {self.vars}\nformats: {self.formats}\npatches: {self.patches}"

    def eval(self):
        progress = True
        while progress:
            if not self.exprs: break
            progress = False
            newExprs = dict()
            for name, expr in self.exprs.items():
                val = expr.reduce(self.vars)
                if type(val) is ExprValue:
                    self.vars[name] = val
                    progress = True
                else:
                    newExprs[name] = val
            self.exprs = newExprs
        else:
            raise Exception(f"cannot evaluate some expressions:\n{self.exprs}")

    def patch(self):
        for patch in self.patches:
            fmt = patch['fmt']
            val = patch['expr'].reduce(self.vars)
            if type(val) is ExprValue:
                val = val.val
            else:
                raise Excpetion(f"cannot evaluate patch expression: {val}")
            # load the unpatched bits from the binary
            bits = []
            fmtwords = fmt['nbits']//self.wordbits
            for i in range(0, fmtwords):
                word = self.words[patch['off'] + i]
                for j in range(self.wordbits-1, -1, -1):
                    bits.append((word >> i) & 1)
            # call the bitwise patching function
            bits = list(patch_bits(fmt, bits, val))
            # TODO distribute the bits back
            for i in range(0, fmtwords):
                word = 0
                for j in range(0, self.wordbits):
                    word = (word << 1) + bits[i*self.wordbits + j]
                self.words[patch['off'] + i] = word

    def print_words(self):
        # TODO eval exprs and insert patches
        for word in self.words:
            print(self.wordstr(word))

def parse_file(fp):
    acc = Asm()
    mode = parse_meta
    for line in fp.readlines():
        line = line.rstrip('\n\r')
        line = line.lstrip()
        if not line: continue
        mode = mode(acc, line)
    return acc

def parse_meta(acc, line):
    if m := re.match(r'([a-zA-Z]+):(.*)$', line):
        k, v = m[1].lower(), m[2].strip()
        if k == "digit":
            if v == "hex":
                acc.digit = 4 # four bits per digit
            else:
                raise Exception("unsupported base")
        elif k == "wordbits":
            acc.wordbits = int(v)
        elif k == 'defaultformat':
            acc.defaultformat = v.strip()
        else:
            raise Exception(f"unknown option: {repr(k)}")
        return parse_meta
    elif re.match(r'-{3,}$', line):
        return parse_defs
    else:
        raise Exception(f"syntax error {repr(line)}")

def parse_defs(acc, line):
    if m := re.match(r'([a-zA-Z]+):(.*)$', line):
        name = m[1]
        fmt = parse_format(m[2])
        acc.formats[name] = {
                "nbits": len(fmt),
                "indexes": fmt,
            }
        return parse_defs
    elif m := re.match(r'('+ID_REGEX_STR+r')'+r'\s+=\s+', line):
        name = m[1]
        expr, rest = parse_expr(line[m.end():])
        if rest: raise Exception(f"syntax error: {rest}")
        acc.exprs[name] = expr
        return parse_defs
    elif re.match(r'-{3,}$', line):
        acc.finalize()
        return parse_body
    else:
        raise Exception()

def parse_body(acc, line):
    line = line.lstrip()
    while line:
        if m := re.match(r'('+ID_REGEX_STR+r'):', line):
            acc.vars[m[1]] = len(acc.words)
        elif m := acc.WORD_REGEX.match(line):
            word = int(m[0], base=2**acc.digit)
            acc.words.append(word)
        elif m := re.match(r'\{(.*?)\}', line):
            fmt, words, expr = parse_patch(acc, m[1])
            if fmt is None:
                fmt = acc.formats[acc.defaultformat]
            else:
                fmt = acc.formats[fmt]
            if words is None:
                words = fmt['nbits']//acc.wordbits * [0]
            acc.patches.append({
                'off': len(acc.words),
                'fmt': fmt,
                'expr': expr
            })
            acc.words += words
        # TODO directives (align, pad, section?)
        else:
            raise Exception()
        line = line[m.end():].lstrip()
    return parse_body

def parse_words(asm, line):
    acc = []
    if m := acc.WORD_REGEX.match(line):
        word = int(m[0], base=2**acc.digit)
        acc.words.append(word)

def parse_patch(asm, input):
    parts = input.split(';')
    if len(parts) == 1:
        expr = parse_expr(parts[0].lstrip())
        fmt = None
        words = None
    elif len(parts) == 2:
        expr = parse_expr(parts[1].lstrip())
        fmt = parts[0].strip()
        words = None
    elif len(parts) == 3:
        expr = parse_expr(parts[2].lstrip())
        fmt = parts[0].strip()
        words = []
        words_input = parts[1].lstrip()
        while words_input:
            if m := asm.WORD_REGEX.match(words_input):
                word = int(m[0], base=2**asm.digit)
                words.append(word)
                words_input = words_input[m.end():].lstrip()
            else:
                raise Exception()
    else:
        raise Exception()
    expr, rest = expr
    if rest: raise Exception(f"Syntax error: {rest}")
    return fmt, words, expr

# # Expression Trees and Evaluation

# We use expressions to calculate (non-negative) integers that will be patched into the binary payload.
# To this end, we have a very simple expression language.
# It contains only addition, subtraction, multiplication, and (built-in) function calls.
# Of course, it also allows integer literals and variables.
# Additionally, it respects precedence rules, and allows parentheses to override precedence.
# Expressions are reduced only as far as able, with only fully-evaluated expressions being patched in.
# This allows linking to proceed in stages if necessary, and is little enough work to implement.

# TODO it may be simpler (better) to only allow full reduction.
# Probably this toolchain will be used only with all required sources available at once.

# In the interests of keeping thigs simple, we have neglegted error detection.
# Honestly, if an author can't figure out what's wrong in the expression,
# they had better make a simpler expression.

# ## Parser

# These define the basic tokens.

# 1. Numbers are non-negative integers in base 10.
#    There is no syntax for any other number system, as that would be _fancy_.
# 2. Identifiers are alphanumeric, and also allow dots, dollars, and at-signs.
#    Of course, they must not start with a number.
# 3. The function regex is just the identifier regex, but followed by a parenthesis.
#    Note that function names must immediately preceed their argument list.
NUM_REGEX = re.compile(r'\d+')
ID_CLASS = r'a-zA-Z_.$@'
ID_REGEX_STR = f'[{ID_CLASS}][{ID_CLASS}0-9]*'
ID_REGEX = re.compile(ID_REGEX_STR)
FUNC_REGEX = re.compile(ID_REGEX_STR + r'\(')

# This is a hand-written recursive decent parser.
# Expressions are:
#
# 1. Parenthesized expressions
# 2. Operators
# 3. Function calls
# 4. Values
#
# Values are:
#
# 1. Integer literals
# 2. Variable names

def parse_expr(expr):
    acc = []
    while True:
        if not expr or expr[0] in ",)":
            break
        # Here are parenthesized expressions.
        # `expr ::= '(' <expr> ')'`
        if expr[0] == '(':
            expr = expr[1:].lstrip()
            tree, rest = parse_expr(expr)
            acc.append(tree)
            expr = rest.lstrip()
            if not expr or expr[0] != ')':
                raise Exception()
            expr = expr[1:].lstrip()
        # Here are operators.
        # `expr ::= <expr> ('+' | '-' | '*') <expr>`
        # This parser just expects that the input puts the operators in the right place.
        elif expr[0] in "+-*":
            acc.append(expr[0])
            expr = expr[1:].lstrip()
        # Here are functions.
        # `expr ::= <id>'(' <arg list> ')'
        elif m := FUNC_REGEX.match(expr):
            func_name = m[0][:-1]
            expr = expr[m.end():].lstrip()
            # The argument list is just parsed right here, since it's not used anywhere else.
            # `arg list ::= ( <expr> (',' <expr>)* )?`
            args = []
            while True:
                arg, rest = parse_expr(expr)
                args.append(arg)
                expr = rest.lstrip()
                if not expr: raise Exception()
                if expr[0] == ',':
                    expr = expr[1:].lstrip()
                elif expr[0] == ')':
                    acc.append(ExprFunction(func_name, args))
                    expr = expr[1:].lstrip()
                    break
                else:
                    raise Exception()
        # Anything else will just be values
        # `expr ::= <value>`
        elif it := parse_val(expr):
            tree, rest = it
            acc.append(tree)
            expr = rest.lstrip()
        else:
            raise Exception("syntax error: {repr(expr)}")
    if len(acc) == 0:
        raise Exception()
    elif len(acc) == 1:
        return acc[0], expr
    else:
        return ExprParens(acc), expr

def parse_val(expr):
    # Here are (non-negative, decimal) numbers:
    # `value ::= <number>`
    if m := NUM_REGEX.match(expr):
        return ( ExprValue(int(m[0])), expr[m.end():] )
    # TODO match for function calls before variables
    # Here are identifiers:
    # `value ::= <id>`
    elif m := ID_REGEX.match(expr):
        return ( ExprVariable(m[0]), expr[m.end():] )
    else:
        return None

# ## Built-in Functions
#
# To be honest, I haven't really decided on what functions might be useful for the standard.
# These are just here for my own testing, but a final list will be cdecided later.

FUNC_TAB = {
    'abs': lambda a: abs(a),
    'clamp': lambda lo, a, hi: max(lo, min(a, hi)),
}

# ## Expression Data Types
#
# This is some boring implementation details, and a bunch of almost-plain-old-data.
# A node in the expression tree can be any of these classes, or a string (operator).
# The reason we have classes for all of these is so that they can have methods for reduction and display.
# Reduction is, honestly very straightforward.

class ExprValue:
    def __init__(self, val: int):
        self.val = val
    def __repr__(self):
        return str(self.val)
    def reduce(self, env):
        return self

class ExprVariable:
    def __init__(self, name: str):
        self.name = name
    def __repr__(self):
        return str(self.name)
    def reduce(self, env):
        if self.name in env:
            x = env[self.name]
            if type(x) is int:
                x = ExprValue(x)
            return x
        else:
            return self

class ExprFunction:
    def __init__(self, func_name, args):
        self.name = func_name
        self.args = args
    def __repr__(self):
        return f'{self.name}({", ".join((str(x) for x in self.args))})'

    def reduce(self, env):
        if self.name not in FUNC_TAB:
            raise Exception(f"funknown function {self.name}")
        # TODO check number of arguments using `inspect.signature()`
        self.args = [x.reduce(env) for x in self.args]
        if all((type(x) is ExprValue for x in self.args)):
            return ExprValue(FUNC_TAB[self.name](*(x.val for x in self.args)))
        else:
            return self

class ExprParens:
    def __init__(self, children):
        # The children should be a list of things that would reduce to values,
        # separated by operators.
        self.children = children
    def __repr__(self):
        acc = []
        for x in self.children:
            if type(x) is str:
                acc.append(x)
            else:
                acc.append(str(x))
        return '(' + ' '.join(acc) + ')'

    def reduce(self, env):
        for i in range(0, len(self.children), 2):
            self.children[i] = self.children[i].reduce(env)
        # reduce multiplication
        i = 1
        mult_finished = True
        while i < len(self.children) - 1:
            if self.children[i] == '*':
                a = self.children[i-1]
                b = self.children[i+1]
                if type(a) is ExprValue and type(b) is ExprValue:
                    self.children[i-1:i+2] = [ExprValue(a.val * b.val)]
                else:
                    mult_finished = False
                    i += 2
            else:
                i += 2
        # now it's just addition and subtraction
        if mult_finished and type(self.children[0]) is ExprValue:
            acc = self.children[0].val
            i = 1
            while i < len(self.children) - 1:
                if self.children[i] == '+':
                    b = self.children[i+1]
                    if type(b) is not ExprValue:
                        return self
                    acc += b.val
                elif self.children[i] == '-':
                    b = self.children[i+1]
                    if type(b) is not ExprValue:
                        return self
                    acc -= b.val
                else:
                    raise Exception()
                i += 2
            return ExprValue(acc)
        else:
            return self

# # Formats

# We don't assume any partiular ordering (or number) of bits when we patch a computed value into the binary.
# In fact, for some formats (such as RISC-V S-type instructions), the bits are spread irregularly throughout the binary.
# Formats give us a way to specify where to place each bit of a computed number across some number of bytes.

# A format is stored internally as little more than a list of `None`s and integers.
# An integer identifies the bit (zero-indexed, little-endian, so it agreres with the bit's power of two),
# wheras a `None` specifies a padding bit.
# The patch is performed by masking the non-padding value's bits with the bits present in the binary source.

def patch_bits(format, bits, value):
    bits = list(bits)
    if (nbits := len(bits)) != format['nbits']:
        raise Exception("mismatched number of bits for the format")
    for i in range(0, format['nbits']):
        bitIndex = format['indexes'][i]
        if bitIndex is None:
            yield bits[i]
        else:
            yield (value >> bitIndex) & 1

def parse_format(input):
    input = input.lstrip()
    acc = []
    while input:
        if m := re.match(r'X\*(\d+)', input):
            acc += [None] * int(m[1])
        elif m := re.match(r'X+', input):
            acc += [None] * len(m[0])
        elif m := re.match(r'(\d+)-(\d+)', input):
            a, b = int(m[1]), int(m[2])
            if a < b:
                acc += list(range(a, b+1))
            elif a > b:
                acc += list(range(a, b-1, -1))
            else:
                acc += [a]
        elif m := re.match(r'\d+', input):
            acc += [int(m[0])]
        else:
            raise Exception(f"format syntax error: {input}")
        input = input[m.end():].lstrip()
    return acc

# # Kickoff

if __name__ == "__main__":
    main()
