# Examples

## Build all tests

```sh
make
```

## Run tests quickly

```sh
make test
```

## Modes for running tests

`make test-jar` runs with a sterlang jar. It is used for running Erltc tests in CI on each push.

`make test-daemon` runs with sterlang running as a daemon.

`make test-native` this is the default mode, same as `make test`

> In order to run `make test-native` locally, you will need a native sterlang binary in your
`<repo root>/erltc/priv/<platform>` directory. To see how to build one yourself,
look at `<repo-root>/.github/workflows`.


## Run and update IR (internal representation) tests

You can run only the IR tests with `make test-ir`

You can update these tests with `make update-ir-spec`

## Run 'checks' tests

run these compiler error snapshot tests with `make -C checks/src`

[checks/src](checks/src)

## To Focus on a Specific Example

> assumes you have run `make` in this directory recently so the type checker (sterlang) is an up-to-date

You can skip running rebar, and run erltc directory.

This is helpful for debugging and for faster development.

```sh
DEBUG=1 rebar3 compile | grep 'erltc --build' | sed G
```

The output will look something like this:

```
erltc --build compile --src-dir examples/dev_dots/src --build-dir examples/_build/default/lib/dev_dots/build -o examples/_build/default/lib/dev_dots/ebin -I examples/_build/default/lib/dev_dots/include -pa examples/_build/default/lib/dev_dots/ebin +debug_info +report_warnings dots_mod01.erlt

erltc --build compile --src-dir examples/calc/src --build-dir examples/_build/default/lib/calc/build -o examples/_build/default/lib/calc/ebin -I examples/_build/default/lib/calc/include -pa examples/_build/default/lib/calc/ebin +debug_info +report_warnings calc.erlt calc_core.erlt calc_parser_ffi.erlt

...

```

Copy the command for the example you want to focus on, then run it

> Note: the rebar plugin does not shell out to erltc: it passes a list of strings in memory.

## To see beam disassembly

We disassemble BEAMS in our tests for the accuracy of our generated .erl files.

The human-readable .erl files end up in `./_build/default/lib/<example-name>/build/`

These .erl files are so devs can familiarize themselves with the output, which has the following benefits:
- this gives some people peace of mind
- they learn about our representation of erlt data structures in classic Erlang
- could form part of a demigration story if we need that
- we test that compiling and disassembling the generated .erl matches disassembly for the generated BEAM:

```pre
erlt -------> (compile time) .erl  ----> (at test time) .beam  --> disassembly
|                                                                     ||
|                                                                     ||     equal
|                                                                     ||
v                                                                     ||
(compile time)   .beam         -------------------------> (at test time) disassembly
```

> implementation: test_utils/disassemble.escript writes .dis files > (disassembly), which we then diff in tests/Makefile. It is based on a script by the OTP team, the main changes are noted at the top of the file.

- upate: "IR" tests to use .erl instead of .P
    - This PR changes how our IR tests work so that we no longer generate .P files. Instead we compare against .erl. This will hopefully be easier for us to inspect as well.
    - We no longer use a separate `ir` rebar profile for our "ir" tests: we use the same compiler options as for building. We **no longer use +deterministic** because it breaks error reporting (https://github.com/WhatsApp/erlt/issues/210). Not using +deterministic means that long paths show up in our generated .erl.

