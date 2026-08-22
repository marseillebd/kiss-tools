# FRAMGMENTS

> [!TIP]
> The addresses in assertions are written in hexadecimal because
>   a) hexadecimal is much easier to parse than decimal,
>   b) they are easier to hand-verify with existing hexdump tools (which use hexadecimal addresses), and
>   c) those assertions are commonly used within the bytes themselves.
>
> Sticking to hexadecimal greatly simplifies the hexasm, which needs only shifting hardware rather than multiply.
> Addresses are likely to appear in the hexadecimal bytes themselves,
>   either directly or as part of a calculation.
>   so it saves a hand-conversion step when writing hex files.
> Arithmetic in hexadecimal is certainly less familiar to you than decimal arithmetic,
>   but it is relatively easy if you have addition/subtraction/multiplication tables next to you.

I've based this on ISO EBNF ([ISO/IEC 14977 : 1996(E)](https://www.cl.cam.ac.uk/~mgk25/iso-14977.pdf)),
  but that grammar has a few limitations, which I've decided to work around:
- No support for bounded repetition like ABNF's `m*n RULE`. I've taken that syntax wholesale, as it does not conflict with ISO EBNF.
- The use of `(* *)` comments is uncommon and awkward to type.
  Semicolons introduce line-comments, in addition to their ordinary use to terminate rules.
- Commas to concatenate just add a lot of line noise and don't help disambiguate anything.
  (The standard uses rule names that include spaces, but I couldn't verify that that was even valid by the same standard!)
- There's no way to quickly describe large ranges of characters, such as printable ascii.
  The ABNF syntax is a bit ugly, but I'll steal it anyway,
    with the caveat that the meaning of the numbers in the range is relative to the character set.
- There's no direct way to specify a delimited list (eg comma-separated or semicolon-separated).
  ABNF doesn't seem to have this either, but [one author](https://www.cs.man.ac.uk/~pjj/bnf/bnf.html) decided to
    use a notation like `( Rule # Sep )+`.
  Since I'm using `{} []` instead of `+ ?`, I'll instead use `{ Rule # Sep }` for zero-or-more.
  I'm not sure what I'd do for one-or-more.
  I'm also not sure what I'd do for if a trailing separator is allowed (specifically with zero items).

```ebnf
file = { directive | ws } ;
directive = byte
          | comment
          | assert ;

;
; Every byte must be two consecutive hexdigits.
; This is simpler to parse than allowing whitespace and/or comments in-between,
;   since no nybble buffer would need to be maintained across calls to parse directives.
;
byte = HEXDIGIT HEXDIGIT ;

;
; Comments begin with a hash and continue until the end of the line.
; (Stricly, they continue _through_ the EOL, but the difference is immaterial, since newlines are ignored anyway.)
;
comment = HASH { ANY - NL } EOL ;

;
; Address assertions begin with an at-sign, and continue to the end-of-line.
; The address itself is indicated by a preceding equals sign so that the at-sign syntax need not differ so much from the Kiss Hex Linker.
; Note that no whitespace is allowed after the equals;
;   otherwise a parser would need to branch to find the address.
;
assert = AT { ANY - EQ } EQ hexaddr [ws] EOL ;
hexaddr = 1*8 HEXDIGIT ;

;
; Hexadecimal digits are as-expected, and not case-sensitive.
;
HEXDIGIT = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
         | "A" | "B" | "C" | "D" | "E" | "F"
         | "a" | "b" | "c" | "d" | "e" | "f" ;

;
; Whitespace includes space as expected, but also:
;   newlines are not meaningful, and
;   underscore is intended as a digit separator between bytes, halfwords, and the like.
;
ws = WS { WS } ;
WS = " " | "_" | NL ;
;
; end-of-line does not requier a newline character
;
EOL = NL | EOF ;

;
; I've given names to specific ascii characters, rather than using literals directly in the rules above.
; This way, all the "special" characters can be identified at a glance by looking here,
;   and the major rules don't contain distracting punctuation.
;
AT   = "@"
HASH = "#"
EQ   = "="
NL   = %x0A
EOF  = ? end of the file/input stream ?

ANY = NL | %x20-7E ; ascii printing characters + newline
```

The above is the minimum language for hexfiles.
The level of requirements can vary, however:
- implementations MAY allow `ANY` to include other control codes, or bytes above 0x7F (ie for unicode).
- implementations SHOULD ignore CR (until such time as windows gets with the plaintext program).
- hex files SHOULD restrict the assertion comment text (between the at-sign and equals)
    to a hexlinker identifier (regex `[0-9A-Za-z:._-]` w/o leading digit) optionally followed by whitespace
- hex files SHOULD NOT put extra characters between the `<hexnumber>` address and the end of line (because a linker would not have generated them)
- hex files SHOULD NOT rely on implementations to distinguish case
Oh, and I need to mention that the input file read as an octet text stream in ascii (or it MAY be in an ascii-compatible encoding.




# The Kiss Hex Assembler Specification
