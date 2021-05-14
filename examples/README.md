# ErlT Examples

This directory includes several Erlang-style applications written in ErlT.

- `elm_core` -- a subset of [Elm](https://elm-lang.org/)'s standard library ported to ErlT
- `erltodo` -- this example shows usage of ErlT structs and enums in a runnable app
- `typed_lib` -- straightforward port of Erlang's gb_trees library to ErlT
- `advent_of_code` -- [Advent of Code 2020](https://adventofcode.com/) solutions in ErlT


## Working on ErlT code

Edit ErlT code in `<app-name>/src/*.erlt`. The directory structure is the same
as for Erlang.

Use normal rebar3 workflow from this directory: `rebar3 compile` to compile
examples, `rebar3 shell` to interact with example apps.



## Make a new ErlT app

In this directory:

```sh
rebar3 new erlt <app-name>
```


## Example session

```
rebar3 new erlt test_app

% rebar3 shell
...
Erlang/OTP 23 [erts-11.1.5] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe] [dtrace]

Eshell V11.1.5  (abort with ^G)

1> test_app:main([]).
hello test_app
ok
2>
```
