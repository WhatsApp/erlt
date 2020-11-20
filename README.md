# ErlT: Erlang with Types

ErlT is an experimental extension of the Erlang programming language which aims
to empower Erlang writers to work even more efficiently, especially in WhatsApp-scale
code bases.

## Quick Links

- **[ErlT Overview](./LANGUAGE_OVERVIEW.md)** - A quick tour of the new language features
- **[ErlT Reference](./doc/README.md)** - A detailed breakdown of the differences between Erlang and ErlT
- **[Examples](./examples/README.md)** - A collection of small ErlT projects that demonstrate how the language can be used

## Introduction

ErlT builds on Erlang to provide greater support for working with large Erlang
codebases. It does this by introducing static-types in a way which opens the
door to greater IDE integration, safer refactorings, and faster, more specific
feedback from the compiler.

By default, ErlT is checked (i.e. statically typed), but it also allows some
functions or modules to be unchecked (i.e. dynamically typed), both to aid
migration and to allow code to be written for which we don't yet have a good
typing model, most notably around concurrency - we provide only basic, low-level
support for `receive`. ErlT requires adding specs to unchecked code in order for
it to be accessible and typeable from checked code.

Notably, checked ErlT is currently limited to only the sequential parts of Erlang,
with some features banned, in particular:
- Atoms
- Erlang maps
- Dynamic applications (`M:F(A)`)

## Getting started using ErlT

**More coming soon**


## Working on the ErlT project itself

### Repository Structure

This repository is for prototyping and collaborating around the ErlT language. It consists of these sub-projects:
- `./erltc` - erltc is the compiler for ErlT. erltc does not compile classic Erlang or do type-checking.
- `./sterlang` - stErlang is standalone type-checker for the checked parts of ErlT
- `./analyzer` - this tool gets information about properties of an Erlang project. For available analyses and how to run them, see [./analyzer/README.md](./analyzer/README.md).

> See ./doc/08_compiler_architecture.md for more information about how these parts work together
> See the top-level READMEs for each of these sub-directories for more information about a particular tool

### Prerequisites

- [erlang](https://www.erlang.org/) with OTP 23 or higher
- [rebar3](https://www.rebar3.org/) version 3.14.1 or higher
- [sbt](https://www.scala-sbt.org/)

Use your platform's standard mechanism for obtaining these tools, e.g. on MacOS:

```
brew install erlang rebar3 sbt
```

### Building

`erltc`:

```
rebar3 compile
```

`stErlang`:

```
cd sterlang; sbt assembly
```

built-in definitions and modules: see [./erltc/built_ins/README.md](./erltc/built_ins/README.md)


### Testing

#### erltc tests

``` sh
make -C examples test # runs the tests in ./examples

make -C examples/<example-dir> # build the example in <example-dir>
make -C examples/elm-core # build the example based on the Elm standard library
```

The tests for erltc come in these two flavors:
- the tests under `./examples/check` verify compiler errors against .exp files. Run these with `make -C examples/check/src`
- the other tests in `./examples` verify the compiler against .P files, which are represent the Erlang AST.
    - See [./examples/README.md]( ./examples/README.md )for more information about running and updating these tests
    - See also [erltc_architecture](./doc/08_compiler_architecture.md).


#### stErlang tests

```
cd sterlang/
sbt test
```

## Development

See [here](doc/01_intro.md#development).

# Join the ErlT community

See the [CONTRIBUTING](CONTRIBUTING.md) file for how to help out.

## License

erl2 is Apache licensed, as found in the LICENSE file.

## IDE Support

### Emacs

Limited emacs support, provided as a thin layer on top of
`erlang-mode`.

*Features supported*

- Everything in Erlang mode.  This is not necessarily a good thing,
  some of them will need to be adapted still.  Please report problems
  as issues in this repo.

- Highlighting/font-lock. This is using the Erlang highlighting at the
  moment, will be tweaked for ErlT in time.

- Mechanics to start a language server (erlang_ls). This does not
  support ErlT at present, so the results are not good.  But it is a
  plumbing step, it is easier to keep it in than to remove it now,
  only to add it again shortly.  Time it short.

*Installation*

It can be enabled by putting the following into your `~/.emacs`

The snipper below uses REPO_PATH to stand for the actual place you
have put the code on your machine.

```elisp
;; Install the official Erlang mode
(package-require 'erlang)

;; Include the Language Server Protocol Clients
(package-require 'lsp-mode)

;; Enable LSP for Erlang files
(add-hook 'erlang-mode-hook #'lsp)

(add-to-list 'load-path "<REPO_PATH>/elisp")
(require 'lsp-erlt)
(require 'erlt-mode)
```

If you get stuck, take a look at the [erlang_ls instructions](https://erlang-ls.github.io/editors/emacs/)

Note: for `lsp-mode` it offers to start the server, this is a bit
pointless at the moment, I just blacklist the project or choose `n`.

But given `erlt-mode` is a derivative of `erlang-mode`, it is simpler
to do it this way, as we will have a language server real soon now.

### VS Code

**Coming soon**
