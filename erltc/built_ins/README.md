# Built-ins

## What built-ins are

We have two types of built-ins:
- new built-in modules, such as `t_io` and `t_lists`, that wrap existing modules to make them more ErlT-friendly
- types for the `erlang` module, which we don't wrap

We include these with each build.

The current implementation is a hack for M0. We will do something more maintainable and configurable in the future, likely using the same mechanisms as for building erlt libs that depend on other erlt libs.

## How to update

in this directory:
- update the erlt files
- update ./make_statics.escript main/1 to add your module to the list of modules.
- `make`

