# brainless

Transpile "python" programs into brainf\*\*\* ascii art

## Installing / Compiling

Note: requires [cabal-install](https://www.haskell.org/cabal/)
1. Clone repo
2. Open your terminal of choice
3. While in repository base (that's the folder with this file in it!), either:
   - Run `cabal install` to install the project to your system
   - Run `cabal run . -- [ARGUMENTS]` to run this project without installing

## Usage

For more information, run `brainless --help`.

## Commands
Only a small subset of python has been implemented:

- `<varname> = <expression>`
- `print(<expression>)`
- `a + b`, `a - b`, `a * b`, `a // b`, `a % b` for basic operations
- `a + b` for string concatenation. Note that creating strings longer than
the specified `--string-length` (default: 64) is undefined behaviour.
- `a >= b`, `a <= b`, `a < b`, `a >= b`, `a == b` for numeric
comparison. Also technically defined for string comparison, but there is an
unresolved bug in this area.
- `a and b`, `a or b`, `not a` for boolean logic
- `input(a)` or `input()` to get a line of user input
- `chr(a)` to convert an integer into the corresponding character.
`ord(a)` to get the character code of the start of a string.
- `while <expression>:` and `if <expression>`. Make sure to indent by
exactly 4 spaces at this time. There is a bug related to nested while/if loops.


## Todo
- [x] Finish by the end of the hackathon
- [ ] Fix bugs to do with nested while loops and/or string comparison
- [ ] Add infix operators
- [ ] Expand available commands
- [ ] Optimise generated code / optimise compile times
- [ ] Optimise memory management
