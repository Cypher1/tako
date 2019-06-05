# Tako

[![CircleCI](https://img.shields.io/circleci/project/github/Cypher1/Tako.svg)](https://circleci.com/gh/Cypher1/Tako/tree/master) [![GitHub issues](https://img.shields.io/github/issues/Cypher1/Tako.svg)](https://github.com/Cypher1/Tako/issues) [![GitHub stars](https://img.shields.io/github/stars/Cypher1/Tako.svg?style=social)](https://github.com/Cypher1/Tako)

An programming language for ergonomic software verification using [Hoare Logics](https://en.wikipedia.org/wiki/Hoare_logic).

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

Tako currently uses cabal for dependency management and running builds and tests.
Feel free to use stack, but it is not currently supported.

Installation steps for Debian flavoured Linux:
```
sudo apt install cabal
```

Cabal will also need to be set up and have the required packages installed.
Use the following to do so.
```
cabal update
cabal install
cabal configure --enable-tests
```

### Building

Building is a fairly simple single step.

```
cabal new-build
```

This allows us to run our prototype IR interpreter.

```
cabal new-run tako
```

We can even give it some preprepared IR and have it evaluate it, showing working.

```
echo "Load (0,0,1,1,) b\nLoad (0,1,0,1,) a\nAnd a b c\nFree a\nFree b\nNot c c" | cabal new-run tako
```

## Running the tests

Running the tests should also be fairly simple.

```
cabal new-test
```

## Installation

Tako can be installed using `cabal new-install tako` but I do not recommend doing so at this time.

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We will use [SemVer](http://semver.org/) for versioning. There are no versions currently available, but see the [tags on this repository](https://github.com/Cypher1/Tako/tags).

## Authors

* **Joshua Pratt** - *Initial work* - [Cypher1](https://github.com/Cypher1)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* **Billie Thompson** - *Readme Templates* - [PurpleBooth](https://github.com/PurpleBooth)
* **Chris Hall** - Design advice and instruction in programming language theory - [Chrisosaurus](https://github.com/chrisosaurus)
