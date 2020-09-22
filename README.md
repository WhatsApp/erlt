# erl2: Modern Erlang Project

This repository is for prototyping and collaborating around the ErlT language. It consists of these sub-projects:
- ./erltc: erltc is the compiler for ErlT (ST and DT). erltc does not compile classic Erlang or do type-checking.
- ./sterlang: stErlang is standalone type-checker for ST
- ./analyzer:  this tool gets information about properties of an Erlang project. It can be used standalone, but is also is [used by other tools in this repo](./doc/08_compiler_architecture.md).
- ./erlbuild: erltc only handles one file at a time. erlbuild compiles a collection of ErlT files in the same directory. See [the erlbuild docs](./erlbuild/README.md) for more information.

> See ./doc/08_compiler_architecture.md for more information about how these parts work together
> See the top-level READMEs for each of these sub-directories for more information about a particular tool

## What is ErlT

ErlT is an experimental extension of the Erlang programming language.

It has three dialects (or modes): DT (dynamically typed), ST (statically typed)
and FFI (bridge between ST and DT parts).

**ErlT DT** is very close to classic Erlang
There are only few differences:

* ErlT DT is stricter about scoping of variables - it enforces lexical scoping.
  (Right now it does it by forbidding ambiguity)
* ErlT DT is explicit about already bound and free variables in patterns.
  (Right now it does it by forbidding ambiguity)
* It replaces records with new constructs: [enums](./doc/04_enums.md), [structs and anonymous structs](./doc/05_structs.md).
* The new [`import_type`](07_import_type.md) construct allows using types from
  remote modules via short names.

**ErlT ST** is (syntactically) a subset of ErlT DT. It enforces a typing
discipline. The type system is based on [the Hindley-Milner type
system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system), extended to support erlT data types and pattern matching.
**ErlT ST** supports only sequential part of the language. Some sequential
constructs (like list comprehensions or exceptions) are not supported yet.

**ErlT FFI** mode allows to pack DT code with ST API. See [modes](./doc/03_modes.md) for more information.

# Contributing

./contributing.md

## Examples

See the [examples](examples/) folder.

## Requirements

- [erlang](https://www.erlang.org/) with OTP 23 or higher
- [sbt](https://www.scala-sbt.org/)

## Building

`erltc` and `erlbuild`:

```
rebar3 compile
```

`stErlang`:

```
cd sterlang; sbt assembly
```

## Testing

### erltc tests

    make -C examples test # runs the tests in ./examples

The tests for erltc come in these two flavors:
- the tests under `./examples/check` verify compiler errors against .exp files
- the other tests in `./examples` verify the compiler against .P files, which are represent the Erlang AST. See [erltc_architecutre](./doc/08_compiler_architecture.md) for more information.

### stErlang tests

    cd sterlang; sbt assembly

## Development

See [here](doc/01_intro.md#development).

## Full documentation

See the [doc](doc) folder.

## Join the erl2 community

See the [CONTRIBUTING](CONTRIBUTING.md) file for how to help out.

## License

erl2 is Apache licensed, as found in the LICENSE file.
