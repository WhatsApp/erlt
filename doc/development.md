# ErlT development guide

This document includes technical information about ErlT.

For description of ErlT, see [ErlT language overview](language_overview.md).


## Prerequisites

- [erlang](https://www.erlang.org/) with OTP 23 or higher
- [rebar3](https://www.rebar3.org/) version 3.14.1 or higher
- [sbt](https://www.scala-sbt.org/)

Use your platform's standard mechanism for obtaining these tools, e.g. on MacOS:

```
brew install erlang rebar3 sbt
```


## Repository Structure

This repository consists of two sub-projects:
- [erltc](../erltc) - erltc is the compiler for ErlT and a rebar plugin.
  erltc does not compile Erlang nor does type-checking.
- [sterlang](../sterlang) - stErlang a type-checker for the checked parts of
  ErlT


## Building

`erltc`:

```
rebar3 compile
```

`stErlang`:

```
cd sterlang; sbt assembly

```

Built-in definitions: see [builtins](../builtins).


## Testing

### erltc tests

``` sh
make -C tests test  # runs the tests in ./tests
```

The tests for erltc come in these two flavors:
- the tests under `tests/check` verify compiler errors against .exp files. Run
  these with `make -C tests/check/src`
- the other tests in `tests` verify the compiler against .P files, which are
  represent the Erlang AST.
- See [tests/README.md](../tests/README.md) for more information about running
  and updating these tests.


### stErlang tests

```
cd sterlang/
sbt test
```

## How compilation works

For ErlT unchecked code `erltc` runs additional checks and transformations, and
finally transpiles ErlT code into Erlang code.

For ErlT checked code, in addition to checks and transpiling, `erltc` also runs
type-checking procedures.

The type checker (code name stErlang) is implemented in Scala.

Type-checking works this way:
- when processing a source `.erlt` file, `erltc` serializes the abstract forms
  into [ETF](http://erlang.org/doc/apps/erts/erl_ext_dist.html)
- then `erltc` invokes stErlang to type-check the serialized forms
- stErlang deserializes the forms (via
[Jinterface](http://erlang.org/doc/apps/jinterface/jinterface_users_guide.html))
and performs type-checking.

## Development

You may be focused on some particular areas: ErlT frontend, stErlang
backend, or everything together.

This sections explain different development modes.

### 1. StErlang development

- `build.sbt` is the source of truth
- IJ scala plugin is well usable

StErlang can type-check ErlT files (and directories!) on its own (without being
a part of `erltc` toolchain).

```
sbt:sterlang> run examples/elm-core
[info] running com.whatsapp.sterlang.Main examples/elm-core
examples/elm-core/basics.erl
examples/elm-core/erl2.erl
examples/elm-core/list.erl
examples/elm-core/map.erl
examples/elm-core/map_ffi.erl
examples/elm-core/maybe.erl
examples/elm-core/result.erl
examples/elm-core/tuple.erl
[success] Total time: 1 s, completed ...
```

Coverage report:

```
sbt clean coverage test coverageReport
```

### 2. using stErlang with `erltc`

`erltc` relies on stErlang for type-checking. It supports invoking stErlang
using two different ways:
- Using precompiled native `sterlang` executables for linux and darwin. This is
  the default.
- Using `sterlang.jar`. This is the easiest way to iterate on the typechecker.
  To build `sterlang.jar` and put it where `erltc` expects it, `cd sterlang &&
  make`. After that, build tests/examples with `USE_STERLANG_JAR=1` environment
  variable.

However, `java -jar ..` is relatively slow for quick tasks - as it starts JVM
each time. We have a solution! - the native image of stErlang (through
[GraalVM](https://www.graalvm.org/)). To build it `from sterlang.jar`:

```
native-image --no-server --no-fallback -O4 \
  -jar sterlang/target/scala-2.13/sterlang.jar erltc/bin/sterlang-darwin
```

(Caveat: on mac you have to de-quarantine it via
`xattr -d com.apple.quarantine erltc/bin/sterlang-darwin`).

The native image is really fast - type-checking takes ~20-40ms for a file.


### 3. `erltc` frontend development

`erltc` is a standard Erlang / rebar3 project.
