# ErlT Reference

This `doc/` directory contains detailed descriptions of the various new and
altered language forms that constitute ErlT.

**More coming soon**

## Checked ErlT

By default, ErlT code is checked, which is to say it is _statically typed_ with
a type system based on the
[the Hindley-Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system),
extended to support ErlT data types and pattern matching. Most notably, the
type system offers type inference, so in many cases the types don't need to be
given explicitly. Beyond ruling out whole classes of bugs and enforcing a standardised
structure to code, checked ErlT also opens the door to deeper IDE integration and other tooling.

**More coming soon - link to relevant reference files**

## Unchecked ErlT

Unchecked ErlT is the _dynamically typed_ part of ErlT which introduces new ways to
describe your programs, particularly its data structures, but does not check types at
compile time. Unchecked code is useful in order to aid migration, and to allow
certain features to be expressed, even if they are hard or impossible to type
check at the moment, such as Erlang's concurrency.

**More coming soon - link to relevant reference files**

## Interop between Checked and Unchecked ErlT

Unchecked code can call any ErlT code.

Checked code, however, needs types to check against, so to call unchecked code
that code needs to have a spec.

**More coming soon - link to relevant reference files**
