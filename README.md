# Tako

[![Cherry](docs/assets/tako.png)](https://takolang.dev)
[![Build Status](https://github.com/Cypher1/tako/workflows/Rust/badge.svg)](https://github.com/Cypher1/tako/actions)
[![GitHub issues](https://img.shields.io/github/issues/Cypher1/tako.svg)](https://github.com/Cypher1/tako/issues)

An experimental programming language for ergonomic software verification using [Hoare Logic](https://en.wikipedia.org/wiki/Hoare_logic).

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

Tako currently uses `cargo` for builds and running tests and git submodules for dependency management.

Installation instructions can be found at <https://rustup.rs/>

### Building

Building is a single step,

```bash
cargo build --release -Z unstable-options --out-dir .
```

Actually, only `cargo build` is needed, but this gives us an optimised build and copies the result into the current directory.

This allows us to run the compiler.

```bash
./tako examples/hello_name.tk
./build/examples_hello_name 'world'
```

And interactive interpreter:

```bash
./tako -i
```

And use the interpreter to run a tako file:

```bash
./tako -r examples/hello_name.tk -- 'world'
```

## Running the tests

Running the tests is also a single step.

```bash
cargo test
```

Note: Currently this tests using an optimised build as some of the tests rely on rust optimisations that decrease stack usage. Tracking bug: <https://github.com/Cypher1/tako/issues/179>

## Installation

> **Warning:** Don't use tako. Use some other language & compiler.
>
> I recommend rust, haskell or idris.
>
> tako is not stable, reliable, or efficient.
>
> You have been warned.

tako is a standalone single file. It can be installed simply by building and copy/moving ./tako into your /usr/bin directory.

## Contributing

Please read the [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) and [CONTRIBUTING.md](CONTRIBUTING.md) guides for details on our code of conduct, and the process for submitting pull requests.

## Versioning

We will use [SemVer](http://semver.org/) for versioning. There are no versions currently available, but see the [tags on this repository](https://github.com/Cypher1/tako/tags).

## Authors

* **J Pratt** - *Initial work* - [Cypher1](https://github.com/Cypher1)

See also the list of [contributors](https://github.com/Cypher1/tako/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* **Billie Thompson** - *Readme Templates* - [PurpleBooth](https://github.com/PurpleBooth)
* **Chris Hall** - *Design advice and instruction in programming language theory* - [Chrisosaurus](https://github.com/chrisosaurus)
