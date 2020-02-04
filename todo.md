# TODOs

## Engineering
- Build steps in rust (reduce dependencies where possible)
- Standardize on cli arguments (copying, where possible from go, rustc, cargo)
  - tako help
  - tako build
  - tako run
  - tako doc
  - tako clean
- Features:
  - Definitions +/ locals
  - Functions
  - Function arguments
  - Main function argument parsing
  - Imports
  - More operators
  - Conversion to ssa
  - Conversion to stack
  - Tests for compilation
  - Test using work from main.rs
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
- Debug and format for ast, info and datatype types
- No way to run machine or low level instructions
- No way to hide local data from a child function (locals)
- No type checking (requirements and exhuastiveness checking)
- Compiler is far behind interpreter feature set
