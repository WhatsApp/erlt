# 1. ErlT Intro

## This documentation

The current documentation consists of technical information about ErlT.
It serves the following purposes:

* To keep the things about ErlT accountable (especially such subtle points -
  like different aspects of new lint checks) and up to date.
* To satisfy the appetite of a curious reader.

This documentation is not a tutorial or a manual for ErlT (yet).

NB: this documentation reflects more about StErlang as ErlT frontend is not
stable/precise yet.

## A bit on internals

ErlT compiler is `erltc`.
For ErlT DT files it runs additional checks and transformations, and finally
transpiles ErlT code into classic erl1 code.
For ErlT ST and FFI files in addition to checks and transpiling, it also runs
type-checking procedures.

The type checker (code name stErlang) is implemented in Scala.

Type-checking works this way:
- when processing a file in `st` or `ffi` mode, `erltc` serialises the abstract
forms into [ETF](http://erlang.org/doc/apps/erts/erl_ext_dist.html)
- then `erltc` invokes stErlang to type-check the serialised forms
- stErlang deserialises the forms (via
[Jinterface](http://erlang.org/doc/apps/jinterface/jinterface_users_guide.html))
and performs type-checking.

## Development

You may be focused on some particular areas (like ErlT frontend, stErlang
backend, or everything together) - the next sections explain different
"development modes".

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

### 2. `erltc` frontend development

`erltc` relies on stErlang for type-checking. It supports invoking stErlang in
[two ways](https://git.io/JJlhF):

`java -jar sterlang.jar ...` - it uses the `sterlang.jar` you get by running
`sbt assembly`. The easiest way to pull all the things together.

However, `java -jar ..` is relatively slow for quick tasks - as it starts JVM
each time. We have a solution! - the native image of stErlang (through
[GraalVM](https://www.graalvm.org/)). To build it `from sterlang.jar`:

```
native-image --no-server --no-fallback -O4 \
  -jar sterlang/target/scala-2.13/sterlang.jar erltc/bin/sterlang
```

If building native image via Graal is too heavy for you locally - you can just
grab the latest version (for mac or linux) we build via GH
[actions](https://git.io/JJlji): take `sterlang-mac` or `sterlang-linux`
artifact, rename it to `sterlang` and place into `erltc/bin` directory.

(Caveat: on mac you have to de-quarantine it via
`xattr -d com.apple.quarantine erltc/bin/sterlang`).

The native image is really fast - type-checking takes ~20-40ms for a file.

## compiler architecture

Some key characteristics of the design of ErlT that influence architecture are:
- Interop between language variants (classic Erlang, DT, and ST) 
- ErlT is compiled to classic Erlang. In particular, every ErlT construct has a relatively straightforward representation as a classic Erlang data structure
- Types are erased
- Types do not influence codegen (except that type-incorrect ST modules are rejected by the compiler)

## Dependency structure of an ErlT project

For **M0 (milestone 0)**, we will support:

compiling a single directory of ErlT files that use a standard library

In the future, we plan to additionally support:

- multiple directories, apps, and libraries
- interop between classic Erlang modules, ST, and DT
- modules of any langauge variant that depend on parse transforms and/or behaviors
- parse transforms and behaviors written in any language variant (ST, DT, or classic Erlang)
- yecc and leex files
- .hrlt files (header files for erlt)
- parse transforms that depend on other parse transforms ([maybe](https://github.com/WhatsApp/erlt/pull/167/files#r488671728))

we **do not** plan to support:
- parse transforms that modify the dependencies of a module

## Relationship between erltc, the rebar plugin, sterlang, and the classic Erlang compiler

- erltc turns ErlT code (ST+DT+FFI) into classic Erlang:
  - in-memory, we create an annotated AST so we can map back to locations in the source code
  - erltc_compile.erl is based on OTP Erlang's compile.erl. We made the following changes:
      - add additional front end passes
      - delegate to the classic Erlang back end
  - we invoke the classic Erlang compiler to generate beam files from the AST that we generate
  - in our tests, we snapshot the AST in .P format, which does not contain these annotated source locations

- The rebar plugin in ../erltc/rebar_prv_erlt.erl is our only public API for the compiler (see ../play/README.md for docs). It is a thin wrapper around erlt_build.erl, which drives erltc.erl (our single-file compiler). erlbuild drives erltc in two phases:
    - scan phase: erltc generates
        - a .defs file for each .erlt file which contains the definitions from the erlt file (specs, types, enums, structs, etc.).
        - a .D file for each .erlt file which records dependencies. The .D files are currently unused but may be used for incremental builds in the future, see https://github.com/WhatsApp/erlt/blob/80877ae15b4a9300c69ce34ffb2e844bc9acc74b/erlbuild/README.md.
    - build phase: erltc builds based on the source .erlt files and the .defs files produced from the previous phase.

- erltc calls stErlang to do type-checking.