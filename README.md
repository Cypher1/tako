# Tako

[![Cherry](docs/assets/tako.png)](https://takolang.dev)
[![Build Status](https://github.com/Cypher1/tako/workflows/Rust/badge.svg)](https://github.com/Cypher1/tako/actions)
[![GitHub issues](https://img.shields.io/github/issues/Cypher1/tako.svg)](https://github.com/Cypher1/tako/issues)

An experimental programming language for ergonomic software verification using [Hoare Logic](https://en.wikipedia.org/wiki/Hoare_logic).


## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

Tako currently uses `cargo` for builds and running tests and git submodules for dependency management.

Installation instructions can be found at https://rustup.rs/
Non-rust dependencies can be installed via
```
git submodule update --init --recursive
```

### Building

Building requires a single step,
```
cargo build
```

This allows us to run our prototype parser.

```
./target/debug/tako <file>
```

## Running the tests

Running the tests should also be fairly simple, but relies on some dependencies which we will fetch using git.

```
cargo test
```

## Installation

tako is a standalone single file. It can be installed simply by building and copy/moving ./tako into your /usr/bin directory.

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We will use [SemVer](http://semver.org/) for versioning. There are no versions currently available, but see the [tags on this repository](https://github.com/Cypher1/tako/tags).

## Authors

* **Joshua Pratt** - *Initial work* - [Cypher1](https://github.com/Cypher1)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* **Billie Thompson** - *Readme Templates* - [PurpleBooth](https://github.com/PurpleBooth)
* **Chris Hall** - *Design advice and instruction in programming language theory* - [Chrisosaurus](https://github.com/chrisosaurus)
* **Iterait** - *Opensourced a CircleCI config that uses std=C++17* - [Hipipe](https://github.com/iterait/hipipe)
