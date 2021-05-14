# ErlT - experimental Erlang dialect

This repository contains an early prototype of ErlT, an experimental Erlang
dialect with first-class support for static typing.

ErlT targets existing Erlang users and codebases. It builds on Erlang to
provide greater support for working with large Erlang codebases. It does this
by introducing static types in a way which opens the door to greater IDE
integration, safer refactorings, and faster, more specific feedback from the
compiler.

The main goal of ErlT is enriching Erlang with static typing capabilities. This
involves:

- Providing a feasible path for gradually adopting ErlT with static typing for
  *existing* teams and codebases.
- Designing and integrating static typing capabilities of ErlT in a coherent and
  ergonomic way. It should be a language that users want to use.

See more details in [ErlT Vision](doc/vision.md).


## ErlT overview

By default, ErlT is checked (i.e. statically typed), but it also allows some
functions or modules to be unchecked (i.e. dynamically typed), both to aid
migration and to allow code to be written for which we don't yet have a good
typing model. ErlT requires adding specs to unchecked code in order for it to
be accessible and typeable from checked code.

Scope of the current prototype includes:

- first-class optional static typing model, based on Hindley-Milner
- new first-class data types
    - enums, aka [discriminated union types](https://en.wikipedia.org/wiki/Algebraic_data_type)
    - structs, aka first-class nominal records
    - shapes, aka ad-hoc structured records
- interoperability between statically typed (checked) and dynamically typed
  (unchecked) of the language within a single module
- improved variable scoping model: (almost) lexical scoping and more explicit rules for
  variable shadowing
- minimal integrations with rebar3

See more details in [ErlT Language Overview](./doc/language_overview.md).

Examples of things that didn't make it into this prototype:

- typing of concurrency primitives
- namespaces and namespace management
- stdlib and library wrapping mechanisms
- IDE/editor integration
- language capabilities to aid the adoption of static typing
- tooling for migrating from Erlang to ErlT


## Getting started with ErlT

Read [ErlT Language Overview](./doc/language_overview.md).

To play with ErlT code, see [Examples](./examples).

Prerequisites:

- [erlang](https://www.erlang.org/) with OTP 23 or higher
- [rebar3](https://www.rebar3.org/) version 3.14.1 or higher

E.g. on MacOS:

```
brew install erlang rebar3
```


## Quick Links

- [Docs](./doc)
- [Examples](./examples)
- [Tests](./tests)


## Contributing

This project is read-only and not accepting contributions.

Feel free to clone. It is open-sourced under the Apache 2 license. see
[LICENSE](./LICENSE) file for details.


## Development

For information on how to navigate ErlT development, see the [Development
Guide](doc/development.md).
