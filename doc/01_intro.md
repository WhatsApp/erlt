# 1. Erl1+ Intro

## This documentation

The current documentation consists of technical information about Erl1+.
It serves the following purposes:

* To keep the things about Erl1+ accountable (especially such subtle points -
  like different aspects of new lint checks) and up to date.
* To satisfy the appetite of a curious reader.

This documentation is not a tutorial or a manual for Erl1+ (yet).

NB: this documentation reflects more about StErlang as Erl1+ frontend is not
stable/precise yet.

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
$ brew install sbt
$ make
$ cd sterlang; sbt assembly; cd ..
```

The similar is for Linux. - Install erlang and sbt via your favourite package
manager.

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

The type checker (code name stErlang) is implemented in Scala.

Type-checking works this way:
- when processing a file in `st` or `ffi` mode, `erl2c` serialises the abstract
forms into [ETF](http://erlang.org/doc/apps/erts/erl_ext_dist.html)
- then `erl2c` invokes stErlang to type-check the serialised forms
- stErlang deserialises the forms (via
[Jinterface](http://erlang.org/doc/apps/jinterface/jinterface_users_guide.html))
and performs type-checking.

## Development

You may be focused on some particular areas (like erl2c frontend, stErlang
backend, or everything together) - the next sections explain different
"development modes".

### 1. StErlang development

- `build.sbt` is the source of truth
- IJ scala plugin is well usable

StErlang can type-check erl1+ files (and directories!) on its own (without being
a part of erl2c toolchain).

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

### 2. `erl2c` frontend development

`erl2c` relies on stErlang for type-checking. It supports invoking stErlang in
[two ways](https://git.io/JJlhF):

`java -jar sterlang.jar ...` - it uses the `sterlang.jar` you get by running
`sbt assembly`. The easiest way to pull all the things together.

However, `java -jar ..` is relatively slow for quick tasks - as it starts JVM
each time. We have a solution! - the native image of stErlang (through
[GraalVM](https://www.graalvm.org/)). To build it `from sterlang.jar`:

```
native-image --no-server --no-fallback -O4 \
  -jar sterlang/target/scala-2.13/sterlang.jar erl2c/bin/sterlang
```

If building native image via Graal is too heavy for you locally - you can just
grab the latest version (for mac or linux) we build via GH
[actions](https://git.io/JJlji): take `sterlang-mac` or `sterlang-linux`
artifact, rename it to `sterlang` and place into `erl2c/bin` directory.

(Caveat: on mac you have to de-quarantine it via
`xattr -d com.apple.quarantine erl2c/bin/sterlang`).

The native image is really fast - type-checking takes ~20-40ms for a file.
