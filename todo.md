# TODOs

## Engineering
- Standardize on cli arguments (copying, where possible from go, rustc, cargo)
  - tako help
  - tako build
  - tako run
  - tako doc
  - tako clean
- Features:
  - Locals
  - Main / command function argument parsing
  - Imports
  - More operators
  - Conversion to ssa
  - Conversion to stack
  - Tests for compilation
  - Move globals / std into a file using low level ops
  - Type checking
- Testing
  - Tests for parser
  - Tests for interpreter
  - Explicit evaluation of funcs
  - Type checking

## Communication
- Improve on Read me
- Language documentation

## Pain points
- No way to run machine or low level instructions
- No way to hide local data from a child function (locals)
- No type checking (requirements and exhuastiveness checking)
- Compiler is far behind interpreter feature set
