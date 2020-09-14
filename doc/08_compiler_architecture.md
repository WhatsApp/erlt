# compiler architecture

Some key characteristics of the design of ErlT that influence architecture are:
- Interop between language variants (classic Erlang, DT, and ST) 
- ErlT is compiled to classic Erlang. In particular, every ErlT construct has a relatively straightforward representation as a classic Erlang data structure
- Types are erased
- Types do not influence codegen (except that type-incorrect ST modules are rejected by the compiler)

## Dependency structure of an ErlT project

For **M0 (milestone 0)**, we will support:

compiling a single directory of ErlT files that use a standard library

An ST module may depend on:
- ST modules
- FFI modules

A DT module may depend on:
- ST modules
- DT modules

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

## Relationship between erltc, sterlang, analyzer, and the classic Erlang compiler

- erltc figures out the dependency graph
    - details are in flux. See ../erlbuild/README.md for a general idea of the build model 

- erltc turns ErlT code (ST+DT+FFI) into classic Erlang:
  - in-memory, we create an annotated AST so we can map back to locations in the source code
  - erltc_compile.erl is based on OTP Erlang's copile.erl. We made the following changes:
      - add additional front end passes
      - delegate to the classic Erlang back end
  - we invoke the classic Erlang compiler to generate beam files from the AST that we generate
  - in our tests, we snapshot the AST in .P format, which does not contain these annotated source locations

- In the future, erltc will call into stErlang to do type-checking.
    - We have currently decoupled erltc and stErlang for better parallelization of developer effort. So the current state is:
        - erltc currently does no-op type-checking
        - stErlang has its own lightweight parser and operates on an IR rather than surface syntax
    - erltc will communicate with stErlang. The code is in erltc_compile.erl in the `run_sterlang` function. erltc:
        - generates .etf files in a format that contains a normalized format representing:
            - if ST: all forms, but in a simplified format
            - if FFI: all forms except functions, since those are not type-checked
        - has code to shell out to stErlang, providing paths to:
            - the single file to type-check
            - the path to the FFI file for stErlang to use in type-checking

stErlang calls into [analyzer](../analyzer/README.md) for some of its analyses using the [OTP Java Interface ](http://erlang.org/doc/apps/jinterface/java/com/ericsson/otp/erlang/package-summary.html).


## stErlang

### entrypoints

stErlang is written in Scala and can produce either:
- jar for use during development of erlT itself
- native code, using GraalVM. See [the intro](./01_intro.md) for more information.

erltc knows how to invoke stErlang in either format, but [(temporarily)](https://github.com/WhatsApp/erlt/pull/152) skips invoking stErlang

stErlang can be run and tested standalone. See the main README for how.

<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 1c80242... more notes on sterlang
### design and type system

- The best resource is https://fb.quip.com/zyy6ABoAWutb. See esp. the section on type inference.
- See also the [[http://moscova.inria.fr/~maranget/papers/warn/warn.pdf Warnings for pattern matching]] for how our pattern matching warnings work.


<<<<<<< HEAD
=======
>>>>>>> 53ae049... update README
=======
>>>>>>> 1c80242... more notes on sterlang
