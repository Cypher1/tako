# Tako

[![CircleCI](https://img.shields.io/circleci/project/github/Cypher1/tako.svg)](https://circleci.com/gh/Cypher1/tako/tree/master) [![GitHub issues](https://img.shields.io/github/issues/Cypher1/tako.svg)](https://github.com/Cypher1/tako/issues) [![GitHub stars](https://img.shields.io/github/stars/Cypher1/tako.svg?style=social)](https://github.com/Cypher1/tako)

An programming language for ergonomic software verification using [Hoare Logics](https://en.wikipedia.org/wiki/Hoare_logic).

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

Tako currently uses cmake for builds and running tests and git submodules for dependency management.

Installation steps for Debian flavoured Linux:
```
sudo apt install make cmake git
```

### Building

Building is a fairly simple single step.

```
make tako
```

This allows us to run our prototype parser.

```
./tako <file>
```

Soon we'll be able to give it some preprepared IR and have it evaluate it, showing working.

```
echo "Load (0,0,1,1,) b\nLoad (0,1,0,1,) a\nAnd a b c\nFree a\nFree b\nNot c c" | ./tako
```

## Running the tests

Running the tests should also be fairly simple, but relies on some dependencies which we will fetch using git.

```
git submodule init
git submodule update
make test
```

## Installation

Tako is a standalone single file. It can be installed simply by building and copy/moving ./tako into your /usr/bin directory.

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
