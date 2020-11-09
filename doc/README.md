# ErlT Reference

This `docs/` directory contains detailed descriptions of the various new and
altered language forms that constitute ErlT.

**More coming soon**

## Checked ErlT

By default, ErlT code is checked, which is to say it is _statically typed_ with an ML-like type system,
offering both type inference and checking at compiler time.
Beyond ruling out whole classes of bugs and enforcing a standardised structure to code, it also opens the door
to deeper IDE integration and other tooling.

**More coming soon - link to relevant reference files**

## Unchecked ErlT

Unchecked ErlT is the _dynamically typed_ part of ErlT which introduces new ways to
describe your programs, particularly its data structures, but does not check types at
compile time, both in order to aid migration, and to allow certain features to be expressed,
even if they are hard or impossible to type check at the moment, for example, concurrency
falls into this category.

**More coming soon - link to relevant reference files**

## ErlT FFI

ErlT FFI is the _interface_ between checked and unchecked ErlT code, allowing unchecked code to be exposed
to checked code.

**More coming soon - link to relevant reference files**
