# Tako
<!-- ALL-CONTRIBUTORS-BADGE:START - Do not remove or modify this section -->
[![All Contributors](https://img.shields.io/badge/all_contributors-1-orange.svg?style=flat-square)](#contributors-)
<!-- ALL-CONTRIBUTORS-BADGE:END -->

[![Cherry](./takoweb/static/tako.png)](https://takolang.dev)
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
cargo build --release
```

You can also install tako, though I don't recommend this.
```
cargo install --path=./tako
```


This allows us to run the compiler.

```bash
tako examples/hello_name.tk
./build/examples_hello_name 'world'
```

And interactive interpreter:

```bash
tako -i
```

And use the interpreter to run a tako file:

```bash
tako -r examples/hello_name.tk -- 'world'
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

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* **Billie Thompson** - *Readme Templates* - [PurpleBooth](https://github.com/PurpleBooth)
* **Chris Hall** - *Design advice and instruction in programming language theory* - [Chrisosaurus](https://github.com/chrisosaurus)

## Contributors

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tbody>
    <tr>
      <td align="center" valign="top" width="14.28%"><a href="https://cypher1.github.io"><img src="https://avatars.githubusercontent.com/u/1120798?v=4?s=100" width="100px;" alt="Jay Pratt"/><br /><sub><b>Jay Pratt</b></sub></a><br /><a href="https://github.com/Cypher1/tako/commits?author=cypher1" title="Code">💻</a> <a href="#design-cypher1" title="Design">🎨</a></td>
    </tr>
  </tbody>
</table>

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->
