# TODOs

## Features

- Imports
- Externs (started)
- Move globals / std into a file using low level ops (started)
- Check for associativity errors (e.g. a&&b||c)
- Convenient Conditionals / Ifs / Matches
  - i.e. Shouldn't have to use operators
  - Should mirror if-then-else, loop, while, for(each)
- Type checking
  - Effects system (e.g. type & Effect)
    - Handlers (i.e. continuations)
  - Mutability (via effects)
  - Containers (Vec, Set, HashMap)
  - Products / Anonymous + Named Tuples / Records
  - Unions / Enums / GADTs
  - Monads?
- Sugar
  - pointer(ty)
  - x?.y (mapped .get and .set)
  - x?:y (sugar for if x then x else y)
  - Functions that take just one argument can skip "()" parens(e.g. func(y) == func y)
- Locals (scope management)
- Main + command function argument parsing
- Conversion to ssa (after parsing)
- Conversion to stack?
- Move source locations out of errors and ast nodes in favour of symbol 'paths'
- Provide a mapping between symbol 'paths' and source locations
- Look up source locations only when displaying info / errors to the user

## Testing

- Tests for code generator
- Testing for error generation (i.e. source locations, file names)
- Tests for type checker
- Tests for compiled programs (not just golden sources, but behaviour)

- Standardize on cli arguments (copying, where possible from go, rustc, cargo) e.g.:
  - tako help
  - tako build
  - tako run
  - tako doc
  - tako clean

## User code optimisations

- [Conversion between Arrays of Structs and Structs of Arrays](https://en.wikipedia.org/wiki/AoS_and_SoA)
- Function fusion / code inlining
- Struct fusion (i.e. inlining / flattening of data)
- Constant propagation
- Rewrite rules +/ symbolic execution
- [Super compilaton](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/supercomp-by-eval.pdf)

### Compiler optimisations

- Store ast as map of ssa nodes by id, map from 'symbol paths' to ids
- Perform passes over the ssa map values without having to perform ast traversal
- Deduplicate ssa nodes (i.e. duplicate nodes are only stored once, start with constants and then bubble it up)
- Intern strings

## Communication

- Improve on ReadMe
- Language documentation

## Pain points

- No way to run machine or low level instructions
- No way to hide local data from a child function (locals)
- No type checking (requirements and exhuastiveness checking)
- Compiler is far behind interpreter feature set
