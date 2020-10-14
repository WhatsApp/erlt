# To build all examples

```sh
rebar3 compile
```

# To peep into erl2 internals

```sh
rebar3 as ir compile
```

This puts intermediate representation (IR) of the build into `_build/ir/lib/<app-name>/ebin/*.P`

These `.P` files are a classic Erlang representation of ErlT files.

# To Focus on a Specific Example

> Make sure erltc is on your PATH and points to `<repo-root>../_build/default/bin/`

Run

```sh
DEBUG=1 rebar3 compile | grep 'erltc --build' | sed G
```

The output will look something like this:

```
erltc --build compile --src-dir examples/dev_dots/src --build-dir examples/_build/default/lib/dev_dots/build -o examples/_build/default/lib/dev_dots/ebin -I examples/_build/default/lib/dev_dots/include -pa examples/_build/default/lib/dev_dots/ebin +debug_info +report_warnings dots_mod01.erlt

erltc --build compile --src-dir examples/calc/src --build-dir examples/_build/default/lib/calc/build -o examples/_build/default/lib/calc/ebin -I examples/_build/default/lib/calc/include -pa examples/_build/default/lib/calc/ebin +debug_info +report_warnings calc.erlt calc_core.erlt calc_parser_ffi.erlt

...

```

Copy the command for the example you want to focus on.

You can do the same for IR with:

```sh
DEBUG=1 rebar3 as ir compile | grep 'erltc --build' | sed G
```

> Note: the rebar plugin does not shell out to erltc: it passes a list of strings in memory.

# To run tests

```sh
make test
```

# IR (internal representation) tests

You can run only the IR tests with `make test-ir`

You can update these tests with `make update-ir-spec`

# Checks Test

run this test with `make -C checks/src`

[checks/src](checks/src) - some examples of new checks in `-lang([erl2, st])` and `-lang([erl2, dt])`.


# To debug the Makefile

    make VERBOSE=2

or

    make --debug

# To see beam disassembly

We disassemble BEAMS in our tests for the accuracy of our generated .erl files.
For more information on how and why, see [the PR](https://github.com/WhatsApp/erlt/pull/236).

