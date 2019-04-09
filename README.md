# HTriple

[![CircleCI](https://img.shields.io/circleci/project/github/Cypher1/HTriple.svg)](https://circleci.com/gh/Cypher1/HTriple/tree/master) [![GitHub issues](https://img.shields.io/github/issues/Cypher1/HTriple.svg)](https://github.com/Cypher1/HTriple/issues) [![GitHub stars](https://img.shields.io/github/stars/Cypher1/HTriple.svg?style=social)](https://github.com/Cypher1/HTriple)

An programming language for ergonomic software verification using [Hoare Logics](https://en.wikipedia.org/wiki/Hoare_logic).

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

HTriple currently uses cabal for dependency management and running builds and tests.
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
cabal run Htriple
```

We can even give it some preprepared IR and have it evaluate it, showing working.

```
echo "L 32 b\nL 12 a\nAdd a b c\nFree a\nFree b" | cabal run Htriple
```

## Running the tests

Running the tests should also be fairly simple.

```
cabal test
```

## Installation

Htriple can be installed using `cabal install Htriple` but I do not recommend doing so at this time.

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We will use [SemVer](http://semver.org/) for versioning. There are no versions currently available, but see the [tags on this repository](https://github.com/Cypher1/Htriple/tags).

## Authors

* **Joshua Pratt** - *Initial work* - [Cypher1](https://github.com/Cypher1)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* **Billie Thompson** - *Readme Templates* - [PurpleBooth](https://github.com/PurpleBooth)
* **Chris Hall**
