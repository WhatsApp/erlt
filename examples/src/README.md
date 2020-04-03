# To build examples

    make

    (make -j also works)


# To peep into erl2 internals

    make ir

`make ir` puts intermediate representation (IR) of the build into `$(BUILD_DIR)/ir` folder. Namely:

- `.ml` and `.mli` files - `ocamlformat`ted representation of erl2 files (with `-lang([erl2, st]).`)
- `.P` files - erl1 representation of erl2 files.

# How to debug builds

    make VERBOSE=2


# How to set explicit compile-time dependencies for enforcing compile/typechecking order

Add `-depends_on([mod01, ...]).` to your .erl files -- see `depends_test.erl` for example.
