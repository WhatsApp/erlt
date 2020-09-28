# To build all examples

```sh
rebar3 erlt
```

# To build a single example

```sh
rebar3 erlt <app-name>
```

For example:

```sh
rebar3 erlt dev_enum
```

> rebar commands can be run in any subdirectory of `examples`

# To peep into erl2 internals

    make ir

`make ir` puts intermediate representation (IR) of the build into `_build/ir/lib/<app-name>/ebin/*.P`

These `.P` files are a classic Erlang representation of ErlT files.

# To run tests

```sh
make test
```

# IR (internal representation) tests

You can run only the IR tests with `make test-ir`

You can update these tests with `make update-ir-spec`

# To run a specific ir test

    make test-ir/<app-name>

for example:

    make test-ir/example

# Checks Test

run this test with `make -C checks/src`

[checks/src](checks/src) - some examples of new checks in `-lang([erl2, st])` and `-lang([erl2, dt])`.


# To debug the Makefile

    make VERBOSE=2

or

    make --debug
