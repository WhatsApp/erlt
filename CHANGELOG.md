### 2020-04-24 - M0-RC0

The first preview of erl1+. The focus of this release is initial foundations
for introducing compile-time type-checking (erl1+ ST).

Main highlights:

* `erl2c` - a driver to compile erl1, erl1+ ST/DT/FFI
* erl1+ dialects: DT, ST, FFI
* Language additions:
    * Enums (algebraic data types)
    * Polymorphic records
* Stricter language:
    * Moving towards lexical scoping of variables
      (for now: forbidding ambiguity of variable usage)
* Initial documentation
* Issues closed in M0-RC0 - https://git.io/JfLYT
* Some examples:
  * Porting of [elm-core](https://github.com/elm/core) to erl1+ ST:
    * [examples/elm-core/src](examples/elm-core/src)
  * A tiny "calculator". Parsing is done via FFI, but hanlding of expressions
    is done in a type-safe manner (in erl1+ ST):
    * [examples/calc](examples/calc/README.md)

Next:

* M0-RC1 - polishing, stabilising M0, more clarity in documentation.
* Scoping and planning M1.
