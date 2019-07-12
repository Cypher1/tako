# Tako

[![Cherry](docs/assets/tako.png)](https://takolang.dev)
[![CircleCI](https://img.shields.io/circleci/project/github/Cypher1/tako.svg)](https://circleci.com/gh/Cypher1/tako/tree/master)
[![GitHub issues](https://img.shields.io/github/issues/Cypher1/tako.svg)](https://github.com/Cypher1/tako/issues)

An experimental programming language for ergonomic software verification using [Hoare Logic](https://en.wikipedia.org/wiki/Hoare_logic).


## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

Tako currently uses meson for builds and running tests and git submodules for dependency management.

Installation steps for Debian flavoured Linux:
```
sudo apt install meson git
```

### Installing dependencies

We just use git's submodules.
```
git submodule init
git submodule update
```

### Building

Building requires a setup step,
```
meson build
```

And after that building is a single step:

```
ninja -C build
```

This allows us to run our prototype parser.

```
./build/tako <file>
```

Soon we'll be able to give it some preprepared code and have it evaluate it, showing working.
Currently we can see what the parser 'sees', but the interpreter and compiler aren't ready yet.

```
echo -e "nand(a, b) = sequence(And(a, b, c),Free(a),\nFree(b),\nNot(c, c))" | ./build/tako -i -s Parse
```
The `-i` tells tako to run in interactive mode (taking command line input) and `-s Parse` means to stop after parsing.

## Running the tests

Running the tests should also be fairly simple, but relies on some dependencies which we will fetch using git.

```
ninja -C build test
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
* **Chris Hall** - *Design advice and instruction in programming language theory* - [Chrisosaurus](https://github.com/chrisosaurus)
* **Iterait** - *Opensourced a CircleCI config that uses std=C++17* - [Hipipe](https://github.com/iterait/hipipe)
