# 1. Erl1+ Intro

## This documentation

The current documentation consists of technical information about Erl1+.
It serves the following purposes:

* To keep the things about Erl1+ accountable (especially such subtle points -
  like different aspects of new lint checks) and up to date.
* To satisfy the appetite of a curious reader.

This documentation is not a tutorial or a manual for Erl1+ (yet).

## What is Erl1+

Erl1+ is an experimental extension of the Erlang programming language.

It has three dialects (or modes): DT (dynamically typed), ST (statically typed)
and FFI (bridge between ST and DT parts).

**Erl1+ DT** is very close to classic Erlang (erl1).
There are only few differences:

* Erl1+ DT is stricter about scoping of variables - it enforces lexical scoping.
  (Right now it does it by forbidding ambiguity)
* Erl1+ DT is explicit about already bound and free variables in patterns.
  (Right now it does it by forbidding ambiguity)
* It introduces a new construct: enums
  (see [Enum syntax and semantics](04_enums.md))
* The new [`import_type`](07_import_type.md) construct allows using types from
  remote modules via short names.

**Erl1+ ST** is (syntactically) a subset of Erl1+ DT. It enforces a typing
discipline. The type system is standard [Hindley-Milner type
system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system).
**Erl1+ ST** supports only sequential part of the language. Some sequential
constructs (like list comprehensions or exceptions) are not supported yet.

**Erl1+ FFI** mode allows to pack DT code with ST API.

## How to play with Erl1+

### Prerequisites

Prerequisites (on MacOS):

```
$ brew install erlang
$ brew install opam
$ opam init -c 4.10.0
$ opam install ocamlformat 0.14.1 -y
$ eval $(opam env)
```

It might be a good idea to agree with default options when installing opam via
`brew` (it will add a line to your `.profile` or `.bashrc`, so that
`eval $(opam env)` will happen automatically for you).

The similar is for Linux. [This](https://git.io/JfLLe) is an example how to
install prerequisites on ubuntu.

### Exploring

See [examples](https://git.io/JfLLR) folder.
[Calculator example](https://git.io/JfLLu) is a tiny end-to-end "showcase"
involving parsing, erl1+ FFI and erl1+ ST.

## A bit on internals

Erl1+ compiler is `erl2c`.
For usual Erlang files it just delegates all work to the classic `erlc`.
For Erl1+ DT files it runs additional checks and transformations, and finally
transpiles erl1+ code into classic erl1 code.
For Erl1+ ST and FFI files in addition to checks and transpiling, it also runs
type-checking procedures.
Currently type-checking is implemented via translating erl1+ ST parts to OCaml
and running `ocamlc` on translated files. If there is an error from `ocamlc`,
then compilation of the file fails and the `ocamlс` feedback is displayed back
to the user.
