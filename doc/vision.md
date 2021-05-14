# ErlT – Erlang dialect with static typing

ErlT is an Erlang dialect with first-class support for static typing.

ErlT targets existing Erlang users and codebases. It builds on Erlang to
provide greater support for working with large Erlang codebases. It does this
by introducing static types in a way which opens the door to greater IDE
integration, safer refactorings, and faster, more specific feedback from the
compiler.

## Design goals and constraints

The main design goal of ErlT is enriching Erlang with static typing capabilities
to make Erlang teams and developers more productive.

Design requirements and constraints:

- Providing a feasible path for gradually adopting ErlT with static typing for
  *existing* teams and codebases.
- Designing and integrating static typing capabilities of ErlT in a coherent and
  ergonomic way. It should be a language that users want to use.
- Designing ErlT in such a way that would allow it to evolve further and deliver
  more value to the users of the language.

Design of ErlT is guided by the [ErlT Design
Principles](design_principles.md).


## Design non-goals

Non-goals include improving runtime performance and making big changes in the
Erlang VM.

To limit the scope of the project, we are not looking at making major changes in
the Erlang VM. However, we are likely going to need to make relatively smaller
changes for static typing, modularity, and ergonomics.


## ErlT Overview

First, let’s introduce some useful abbreviations:

- ST ErlT — statically typed sub-language of ErlT
- DT ErlT — dynamically typed sub-language of ErlT

ErlT is designed for gradual adoption/migration for existing Erlang teams and
codebases, and gradual adoption of static typing discipline for the code being
migrated. This implies:

- being familiar to Erlang users. Erlang users start can start using it right
  away, after looking at some code examples, and several pages of documentation
- this means that ErlT syntax, stdlib, tooling, and many other language
  conventions are kept close to Erlang
- there is well-defined tooling and practices to aid migration/adoption
- zero-cost or cost efficient mapping for the new data types to Erlang types and
  runtime representation
- ErlT compiles to BEAM - the same runtime as Erlang

### ErlT scope

What is included ErlT? Here are some highlights:

- design of the ErlT language, its statically and dynamically typed parts. Major
  design areas include:
  - static typing model
  - coherent design of the new data types — enums, structs, shapes. See below
    for details
  - variable scoping model (rules for lexical scoping, pinned vars, shadowing)
  - FFI design (calling between ST and DT parts of the language)
  - interop with Erlang, mechanisms for wrapping Erlang libraries, migratability
    from Erlang. Potentially, demigration to Erlang
  - typing gen_server-like patterns
  - statically typed error model, e.g. result type, exceptions
  - wrappers for stdlib and OTP
  - capabilities and tools to aid adoption of static typing
- production-ready implementation
- integration with existing tooling, e.g. rebar3, test infrastructure
- tools: formatter, IDE integration, shell
- tool for migrating from Erlang and evolving the language
- good ergonomics of all of the above
- user documentation

Potentially included together with static typing (in no particular order):

- hierarchical namespaces
- typing lower-level process and messaging primitives
- lightweight mechanism for ad-hoc polymorphism that would allow to express
  commonly used “overloaded” operations for data types, e.g. comparison,
  printing, serialization/de-serialization
- module types which can be seen as a generalization of Erlang behavior
  mechanism that would allow to promote consistency of library and application
  APIs
- improved error model, e.g.  composable error handling to eliminate extra
  branching/indentation, mechanisms and conventions for including extra context
  in errors, formatting errors, chaining/nesting errors

Potential future scope after static typing (in no particular order):

- a lightweight and ergonomic test tool and a framework (e.g. Elixir’s mix test)
  as a replacement for CT and eunit
- first-class runtime support for enums/structs/shapes
- new stdlib
- new framework as a replacement for OTP
- improved syntax and ergonomics, e.g. revise use of commas and other separators
  in the language
- new language extension mechanism as a replacement for macros and parse
  transforms
- removing the need for header files
- frameworks for easy interoperability with other languages and runtimes, e.g
  GRPC, interop with Rust, etc
- packaging and package repository for ErlT library ecosystem
- new build system

### Static typing

Static typing highlights:

- strong guarantees (aka soundness)
- simple static typing model. It should be easy for users to reason about its
  behavior.
- fast type checker with good error messages. The type checker is integrated
  with the compiler and the IDE.

### New data types

New additions include:

- **enums**, aka [discriminated union types](https://en.wikipedia.org/wiki/Algebraic_data_type)
- **structs**, aka first-class nominal records
- **shapes**, aka ad-hoc structured records

Enums is a nominal alternative to tagged tuples commonly used in Erlang. Enums
are a critical feature needed for the ErlT static typing model.

Structs are a revision of Erlang records, offering an alternative record system
designed from the ground up and tightly integrated with the static typing model.
Compared to Erlang records, structs do not rely on header files, do not have a
global namespace, and are designed as a first-class language datatype, rather
than a thin syntactic layer on top of tuples.

Shapes roughly correspond to statically typed Erlang maps with atom keys. They
can also be seen as a generalization of tuples in a sense that they allow named
arguments instead of only positional ones.

### Interoperability between ErlT ST and DT code

Importantly, there will always be a dynamically typed part of ErlT:

- this allows gradual adoption of static typing for existing Erlang codebases
- dynamic typing provides an “escape hatch” for certain cases that reach the
  limits of a simple static type system

For interoperability between ST and DT portions of the code we employ the following mechanisms:

- ST code is allowed to call DT code via an
  [FFI](https://en.wikipedia.org/wiki/Foreign_function_interface) bridge
- DT code is allowed to call ST code
- extra mechanisms are needed to support migration, e.g. runtime compatibility
  of the data between statically and dynamically typed code
- potentially, runtime checks when crossing between ST and DT domain to make
  sure type errors don’t violate guarantees provided by the ST model.

### Conversion of Erlang to ErlT DT

Erlang users will be able to migrate to DT ErlT in a controlled and gradual way
using the following mechanisms:

- there will be a fully automated migration tool for migrating Erlang to an
  equivalent DT ErlT syntax at a module granularity
- the tools will support both language dialects. This includes build tool, IDE,
  formatter, etc.

### Interop between Erlang and ErlT

There will be direct interoperability between Erlang and ErlT code:

- it is important to be able to try/test ErlT on a small scale before committing
  to larger-scale migration
- even with automatic Erlang → ErlT migration tool, it may not be possible to
  migrate all Erlang dependencies at once

### Erlang → ST ErlT migration path

This section presents a high-level overview of migration path from dynamically
typed Erlang codebase to statically typed ErlT could work. 

Migration of Erlang to DT ErlT can be performed **fully automatically**. This is
a promise of ErlT to avoid problems like with Python3 migration .

**Step 1. Erlang → DT ErlT**

At this step, dynamically typed Erlang code is automatically converted to
dynamically typed ErlT. Depending on how the migration project is structured and
depending on the stage of the migration, such conversion can be done at a
module-by-module, or at a library-by-library granularity.

**Step 2. DT ErlT → ST ErlT**

This is the most complex, and the most involved step that requires dedicated
investment and focus from the whole org. It can’t be fully automated, but can be
tool-assisted to some degree. In general, it requires refactoring of Dialyzer
specs and refactoring of dynamically typed code to follow statically typed
discipline and conventions.

Note that not everything is expected to be statically typed in the end. In many
cases, dynamically typed code can be wrapped and exposed using a statically
typed interface. In other cases, migrating some stable or legacy parts of the
codebase is not worth the effort, especially if they are well isolated.

But to bring consistent benefits of static typing, all active parts of the
codebase have to be migrated. Granularity of such migration as well as many
logistical details are going to be ironed out during later phases of the
project.

## Major open questions

### Solutions for typing concurrency patterns and primitives

More exploration and prototyping is needed to find solutions for these patterns
of Erlang:

- gen_server-like patterns
- typing lower-level process and messaging primitives

### Exact path for migration from Erlang to ErlT and adoption of static typing

We know it can be done, but we don’t yet know the exact migration path and the
cost of static typing adoption:

- The main cost of adoption is refactoring existing code to comply with the new
  static typing model.
- We strive to bring the cost and complexity of adoption down by designing smart
  language capabilities and tools to aid adoption.
- We are going to be running smaller scale experiments to estimate the costs and
  fine tune the adoption process and tooling.

Figuring out the adoption path is one of the next phases of the project.

### Demigration from ErlT to Erlang

While this is technically possible, because ErlT compiles to Erlang, details
and practical aspects of such demigration are not yet known.

## Comparison of ErlT with similar languages

The closest project approaches and languages to ErlT are Hack and ReasonML.
Coincidentally, both were created at Facebook.

Like Hack and ReasonML, ErlT target sound static typing and the productivity
gains that this brings, as the one of primary goals. Hack enables static typing
for PHP, and ReasonML allows to write statically typed code targeting JavaScript
runtime.

Most other dynamic languages and type checkers for dynamic languages do not set
soundness as a design goal. This includes, for example: Type Script, Dart, Pyre
and Mypy for Python, Sorbet for Ruby.

Beyond this important high-level aspect, comparison gets very nuanced and
articulating this is generally out of scope of this high-level document.
