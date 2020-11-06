# RPC Proof of Concepts

In this folder we have proof of concepts for typed RPC services in Erlang.
The goal is to evaluate different ideas, but do so by having the ideas concretely implemented.

  - [count_headert](./count_headert) is an example gen server in erlt where:
    + the header file contains generated dynamic boilerplate gen server code and
    + the implementation is statically typed using only Hindley Milner.
  - [count_implt](./count_implt) is an example gen server in erlt where:
    + a separate spec file is used to generate the dynamic boiler plate gen server
    + we emulate type classes using a struct with fields of functions, which allows multiple implementations to reuse generated code.
  - [count_gadt_implt](./count_gadt_implt) is an example gen server in erlt based on count_implt, where:
    + the generated code is statically typable using a GADT or generalized enum
  - [count_gadt_headert](./count_gadt_headert) is an example gen server in erlt based on count_headert, where:
    + the generated code is statically typable using a GADT or generalized enum
  - [count_gadt](./count_gadt) is an example gen server in erlt based that has no generated code, but is totally statically typed using a GADT.
  - [countt](./countt) is an example gen server in erlt based that has no generated code, that shows how enums fall short for typing handle_call.
  - [rptpc](./rpctc): Is the code generator, than can generate the several alternatives.

## How to Run

Each project includes its own `rebar.config` and `Makefile`:

All projects can run their tests using:
```
$ make test
```

Most of them can run using:
```
$ make run
```

Some can regenerate code using:
```
$ make generate
```

## Other Reference Implementations

Sometimes it is useful to look at references to double check our sanity.

Classic Erlang:

- [count](./count): An example gen server on which all the proof of concepts are based upon.
- [kitty](./kitty): An example gen server from the learn you some erlang book in classic erlang.
- [count_impl](./count_impl): An example gen server split into two parts, where the module is passed as a parameter to the hypothetically generated gen server.
- [count_header](./count_header): An example gen server split into a hypothetically header and an implementation that imports the header.

Erlt:

- [hello_worldt](./hello_worldt): An example erlt application, where it is easy to check syntax without any gen server complexities.