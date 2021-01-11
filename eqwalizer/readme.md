# eqwalizer

First steps in eqwalizer.

Prerequisites: sbt and rebar.
Testing: sbt test

Right now it handles a subset of Erlang (see ast package for more details).
It's extremely minimalistic and simplistic for now. 
Because of this it should be really easy to port it (as an exercise/experiment) 
to Rust or Haskell - using the current ELP implementation.

More snapshot tests are on the way, however even now it is possible to describe/study features/facts like:

- Differences with dialyzer (via examples)
- Subtleties and peculiarities of type checking in light of "let is crash" and happy path philosophy of coding.
- Soundness in terms of checking specs as contracts.
- Patterns and anti-patterns of coding in Erlang
