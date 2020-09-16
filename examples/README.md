# To build all examples

    make

    (make -j also works)


# To peep into erl2 internals

    make ir

`make ir` puts intermediate representation (IR) of the build into `$(BUILD_DIR)/ir`
folder of a corresponding project. Namely:

- `.ml` and `.mli` files - `ocamlformat`ted representation of erl2 files (with `-lang([erl2, st]).`)
- `.P` files - erl1 representation of erl2 files.

# How to debug builds

    make VERBOSE=2

# Some specific examples

[checks/src](checks/src) - some examples of new checks in `-lang([erl2, st])` and `-lang([erl2, dt])`.
