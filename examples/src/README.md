# To build examples

    make

    (make -j also works)


# How to debug builds

    make VERBOSE=2


# How to set explicit compile-time dependencies for enforcing compile/typechecking order

Add `-depends_on([mod01, ...]).` to your .erl files -- see `depends_test.erl` for example.
