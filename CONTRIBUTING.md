# Contributing to erl2

We want to make contributing to this project as easy and transparent as
possible.

## Repository Structure

This repository is for prototyping and collaborating around the ErlT language. It consists of these sub-projects:
- `./erltc` - erltc is the compiler for ErlT. erltc does not compile classic Erlang or do type-checking.
- `./sterlang` - stErlang is standalone type-checker for the checked parts of ErlT
- `./analyzer` - this tool gets information about properties of an Erlang project. For available analyses and how to run them, see [./analyzer/README.md](./analyzer/README.md).

> See ./doc/08_compiler_architecture.md for more information about how these parts work together
> See the top-level READMEs for each of these sub-directories for more information about a particular tool

## Prerequisites

- [erlang](https://www.erlang.org/) with OTP 23 or higher
- [rebar3](https://www.rebar3.org/) version 3.14.1 or higher
- [sbt](https://www.scala-sbt.org/)

Use your platform's standard mechanism for obtaining these tools, e.g. on MacOS:

```
brew install erlang rebar3 sbt
```

## Building

`erltc`:

```
rebar3 compile
```

`stErlang`:

```
cd sterlang; sbt assembly
```

built-in definitions and modules: see [./erltc/built_ins/README.md](./erltc/built_ins/README.md)


## Testing

### erltc tests

``` sh
make -C tests test # runs the tests in ./tests

make -C tests/<example-dir> # build the example in <example-dir>
make -C tests/elm-core # build the example based on the Elm standard library
```

The tests for erltc come in these two flavors:
- the tests under `./tests/check` verify compiler errors against .exp files. Run these with `make -C tests/check/src`
- the other tests in `./tests` verify the compiler against .P files, which are represent the Erlang AST.
    - See [./tests/README.md]( ./tests/README.md )for more information about running and updating these tests
    - See also [erltc_architecture](./doc/08_compiler_architecture.md).


### stErlang tests

```
cd sterlang/
sbt test
```

## Development

See [here](doc/01_intro.md#development).

## Pull Requests
We actively welcome your pull requests.

1. Fork the repo and create your branch from `master`.
2. If you've added code that should be tested, add tests.
3. If you've changed APIs, update the documentation.
4. Ensure the test suite passes: `make -C examples test`
5. Ensure the CI test suite passes on GitHub.
6. If you haven't already, complete the Contributor License Agreement ("CLA").

## Contributor License Agreement ("CLA")
In order to accept your pull request, we need you to submit a CLA. You only need
to do this once to work on any of Facebook's open source projects.

Complete your CLA here: <https://code.facebook.com/cla>

## Issues
We use GitHub issues to track public bugs. Please ensure your description is
clear and has sufficient instructions to be able to reproduce the issue.

Facebook has a [bounty program](https://www.facebook.com/whitehat/) for the safe
disclosure of security bugs. In those cases, please go through the process
outlined on that page and do not file a public issue.

## Coding Style

To be defined.

## License

By contributing to erl2, you agree that your contributions will be licensed
under the LICENSE file in the root directory of this source tree.
