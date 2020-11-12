# Built-ins

## What built-ins are

We have two types of built-ins:
- types for existing modules, such as `lists`
- new built-in modules, such as t_io.

We include these with each build.

The current implementation is a hack for M0. We will do something more maintainable and configurable in the future, likely using the same mechanisms as for building erlt libs that depend on other erlt libs.

## How to update

in this directory:
- update the erlt files
- update ./make_statics.escript main/1 to indicate, for each module, whether it contains types for an existing module (such as `lists`) or is a new module (such as `t_io`).
- `make`
- you may need to update the `-x` (exclude) flags in our tests, currently in ../../examples/Makefile. For example, we ignore generated t_io.* files when diffing.

