---
aliases:
  - Tako TODOs
---
# TODOs

## MVP

- [ ] Re association of lambda calculus terms

## Features

- [ ] REPL remembers variables from previous executions
- [ ] REPL completion from std::lib and imported files
- [ ] Load, unload, reload and display loaded modules in REPL
- [ ] Imports
- [ ] Move globals / std into a file using low level ops (started)
- [ ] Check for associativity errors (e.g. `a&&b||c`)
- [ ] Allow operator declaration (with semantics on evaluation order [i.e. which sides are auto converted to lambdas before being sent to the operator])
  -  Op declarations need:
    - [ ] Precedence (maybe a set of allowed inner operators rather than a number)
    - [ ] left or right associativity
    - [ ] a bin/un-op form (possibly both)
	- [ ] Convert bin and un-op to calls to the operator functions
- [ ] Introduce "." notation for identifiers.
- [ ] Convenient Conditionals / Ifs / Matches
  - [ ] i.e. Shouldn't have to use operators
  - [ ] Should mirror if-then-else, loop, while, for(each)
- [ ] Type checking
  - [ ] Effects system (e.g. type & Effect)
    - [ ] Handlers (i.e. continuations)
  - [ ] Mutability (via effects)
  - [ ] Containers (Vec, Set, HashMap)
  - [ ] Products / Anonymous + Named Tuples / Records
  - [ ] Unions / Enums / GADTs
  - [ ] Monads?
- [ ] Sugar
	- [ ] pointer(ty)
	- [ ] `x?.y` (mapped `.get` and `.set`)
	- [ ] `x?:y` (sugar for `if x then x else y`)
- [ ] Locals (scope management)
- [ ] Main + command function argument parsing
- [ ] Conversion to SSA (after parsing)
- [ ] Conversion to stack?
- [ ] Move source locations out of errors and AST nodes in favour of symbol 'paths'
- [ ] Look up source locations only when displaying info / errors to the user
- [ ] Incremental re-parsing

## Testing

- [ ] Unit tests for code generator
- [ ] Unit tests for error generation (i.e. source locations, file names)
- [ ] Tests for compiled programs (not just golden sources, but behaviour)

- [ ] Standardize on cli arguments (copying, where possible from go, rustc, cargo) e.g.:
  - [ ] tako help
  - [ ] tako build
  - [ ] tako run
  - [ ] tako doc
  - [ ] tako clean

## User code optimisations

- [ ] Rewrite rules +/ symbolic execution
  - [ ] [Conversion between Arrays of Structs and Structs of Arrays](https://en.wikipedia.org/wiki/AoS_and_SoA)
  - [ ] Function fusion / code inlining
  - [ ] Struct fusion (i.e. inlining / flattening of data)
  - [ ] Constant propagation
  - [ ] [Super compilation](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/supercomp-by-eval.pdf)

### Compiler optimisations and usability

- [ ] Intern strings
- [ ] Could use a fast string matching algorithm to quickly count newlines when building error messages, rather than counting them during tokenization
  - [ ] See [aho_corasick](https://thedan64.github.io/inkwell/aho_corasick/index.html)
- [ ] For logging and timing purposes, it seems likely that a scope guard pattern would make reporting much more reliable
  - [ ] See [scope_guard](https://crates.io/crates/scopeguard) [Previously](https://thedan64.github.io/inkwell/scopeguard/index.html)
- [ ] Probably should eventually support [Unicode identifiers](http://www.unicode.org/reports/tr31/#Introduction)
  - [ ] See [unicode_xid](https://thedan64.github.io/inkwell/unicode_xid/index.html)
- [x] Should use [smallvec](https://thedan64.github.io/inkwell/smallvec/index.html) for argument lists and other compiler info
- [ ] Should experiment with LLVM
  - [ ] Possible via [inkwell](https://thedan64.github.io/inkwell/inkwell/index.html).
- [ ] Explore using worker threads for the parser
  - [ ] Job per file, contributing to a queue of nodes to store
  - [ ] Bunches of nodes to store would be faster to store as groups and could be async from file access
  - [ ] Parsing the file could be broken up by balanced brackets
  - [ ] This may be premature optimisation the store medium / RAM may be the bottle neck
- [ ] Remove AST and Info types

## Communication

- [ ] Improve on (and updating) Read Me
- [ ] Put the license on the site
- [ ] Improve on the site (add the Read Me)
- [ ] Write a getting started
- [ ] Language documentation

## Pain points (to work on)

- [ ] No way to run machine or low level instructions
- [ ] No type checking (requirements and exhaustiveness checking)
- [ ] Compiler is far behind interpreter feature set

# Benchmarks

- [ ] lexing benchmarks
- [ ] parsing benchmarks
- [ ] type checking benchmarks
- [ ] cache benchmarks

## Resources

- http://craftinginterpreters.com/
- https://rust-hosted-langs.github.io/book/introduction.html
- https://os.phil-opp.com/kernel-heap/#alignment
- http://www.paulgraham.com/rootsoflisp.html

## Vision

- [ ] Main programs have a build function alongside main for config
	- [ ] This means no 2nd language for config, or reduction in expressivity when doing config
	- [ ] Build is run in the interpreter to generate build effects and config, including any operator overloads needed
	- [ ] Build config is type checked and auto completed etc.
	- [ ] Version info as static arg to makePackage?
- [ ] `tako ci`
    - [ ] Runs install deps, builds & type check and tests
- [ ] "features" are just flags passed to build
- [ ] Statically declarable arg parsing
	- [ ] No more `int main(int argc, char* argv[])`
	- [ ] `main(expression: string, show_working: Arg(bool, "--show-working", default=false), *unknown: Arg[]): ErrorCode {`
	- [ ] Pass unknowns to `tako.cli(unknown)` to get `"--help"`, `"-h"`, `"--verbose"`, `"-v"`, `"--autocomplete"`
	- [ ] CLI generator is a default argument to makeBinary & the interpreter state
	- [ ] It uses Tako's reflection capabilities to do 'run time' code gen, but its statically available because it doesn't produce side effects.
- [ ] Git hash available in build artefacts
- [ ] Multiple entry points possible in a single file
	- [ ] `makeBinary(main, "main")` is default but
	- [ ] `makeBinary(test, "unit")` is just as valid


# Hoare Logic Solving via Hindley Milner

- Each 'time point' has a type
	- Function name + steps : <code>functionName<sub>t</sub></code>
	- This type contains:
		- The `pre`conditions to get to this step
		- The `post`-conditions (requirements) implied by this step
		- The in-variants of this step (in both pre & post)

> [!example] A worked example
> We start with a simple set of statements.
> ```rust
> a = 3;
> a += 1;
> print(a)
> ```
> 
> These can be annotated with pre and post conditions:
> ```rust
> anon1: {
>	!exists(a) or mutable(a)
>	-| a = 3 |-
>	exists(a), numeric(a), range(a, 3, 3)
>}
> anon2: {
>	exists(a), numeric(a), range(a, $unknown2, $unknown3)
>	-| a += 1 |-
>	exists(a), numeric(a), range(a, $unknown2+1, $unknown3+1)
>}
> anon3: {
> 	exists(a), printable(a), exists(print)
> 	-| print(a) |-
> 	exists(a), printable(a)
> }
> ```
> 
> The HM solver must merge either
> ```
>(anon1 and anon2) and anon3
 > ```
 > or
 > ```
 > anon1 and (anon2 and anon3)
 > ```
 > Either way the result should be
 > ```rust
> anon1_to_3: {
>	((!exists(a)) or mutable(a)), printable(a), exists(print)
>	-| a = 3; a+= 1; print(a) |-
>	exists(a), numeric(a), range(a, 4, 4), printable(a)
>}
> ```
> 
> > [!Important] An invariant
> > For *consistency*  HM solvers *must* produce the same result, regardless of order of computation.
> > 
> > This allows them to solve by graph walking, regardless of the direction of the walk AND also allows solving to happen on different sub-graphs (lines, functions, modules) without a particular ordering of evaluation.

