# Examples


## Prerequisites

- `cd ../sterlang && sbt assembly` to generate sterlang.jar. Then copy the jar to `<repo-root>`/erltc/priv

## Build all examples

```sh
../scripts/rebar3 compile # wrapper that downloads v3.14.2 of rebar
```

> you can use your own rebar if it is at least version 3.14.2

## Run tests quickly

```sh
make test
```

## Run tests just like in ci

`make test-jar` runs with a sterlang jar. It is used in Erltc tests on each push

`make test-native` *requires* a native sterlang binary in order to run. It is used
in our release CI and when you make changes to sterlang.

> In order to run `make test-native` locally, you will need a native sterlang in your
`<repo root>/erltc/priv` directory. You can get one from the "releases" page of this repo
or build one. To see how to build one yourself, look at `<repo-root>/.github/workflows`.


## Run and update IR (internal representation) tests

You can run only the IR tests with `make test-ir`

You can update these tests with `make update-ir-spec`

## Run 'checks' tests

run these tests with `make -C checks/src`

[checks/src](checks/src) - some examples of new checks in `-lang([erl2, st])` and `-lang([erl2, dt])`.

## To see beam disassembly

We disassemble BEAMS in our tests for the accuracy of our generated .erl files.
For more information on how and why, see [the PR](https://github.com/WhatsApp/erlt/pull/236).

The .erl files end up in `./_build/default/lib/<example-name>/build/`

## Run rebar directly

> assumes you have run `make` in this directory recently so the type checker (sterlang) is an up-to-date

In this directory:

```sh
../scripts/rebar3 compile
```

Or, if your rebar is at least version 3.14.2:

```sh
rebar3 compile
```

## To Focus on a Specific Example

> assumes you have run `make` in this directory recently so the type checker (sterlang) is an up-to-date

You can skip running rebar, and run erltc directory.

This is helpful for debugging and for faster development.

```sh
DEBUG=1 ../scripts/rebar3 compile | grep 'erltc --build' | sed G
```

The output will look something like this:

```
erltc --build compile --src-dir examples/dev_dots/src --build-dir examples/_build/default/lib/dev_dots/build -o examples/_build/default/lib/dev_dots/ebin -I examples/_build/default/lib/dev_dots/include -pa examples/_build/default/lib/dev_dots/ebin +debug_info +report_warnings dots_mod01.erlt

erltc --build compile --src-dir examples/calc/src --build-dir examples/_build/default/lib/calc/build -o examples/_build/default/lib/calc/ebin -I examples/_build/default/lib/calc/include -pa examples/_build/default/lib/calc/ebin +debug_info +report_warnings calc.erlt calc_core.erlt calc_parser_ffi.erlt

...

```

Copy the command for the example you want to focus on, then run it

> Note: the rebar plugin does not shell out to erltc: it passes a list of strings in memory.

