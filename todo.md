# TODOs

## Engineering

- Features:
  - Imports & Externs
  - Move globals / std into a file using low level ops
  - Type checking
  - Locals (scope management + effects)
  - Main + command function argument parsing
  - Conversion to ssa
  - Conversion to stack
- Testing
  - Tests for parser
  - Tests for interpreter
  - Explicit evaluation of funcs
  - Tests for type checker
  - Tests for compiled programs (not just golden sources, but behaviour)

- Standardize on cli arguments (copying, where possible from go, rustc, cargo) e.g.:
  - tako help
  - tako build
  - tako run
  - tako doc
  - tako clean

## Communication

- Improve on ReadMe
- Language documentation

## Pain points

- No way to run machine or low level instructions
- No way to hide local data from a child function (locals)
- No type checking (requirements and exhuastiveness checking)
- Compiler is far behind interpreter feature set
