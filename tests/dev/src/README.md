# examples

## Example of a circular dependency

str1.erl and str2.erl use structs from each other.

> Note: we are still not sure if circular dependencies will be supported in future

## Example of a transitive dependency **incremental builds are currently incorrect**

This example shows that incremental builds are currently incorrect. Here is how it works:

- str3.erl uses a struct from str2.erl, which uses a struct from str1.erl.
- A default value for a field in str3 reference str2.
- A default value for a field in str2 reference str1.

These default values are *inlined* during code generation. See ../ir-spec/str3.P for an example


In the following scenario, str3.erl should be recompiled when str1 is updated, but it currently is not:

```sh
cd examples/dev && make clean && make -j 10 && touch str1.erl && make VERBOSE=2
```

To debug our representation of the dependency graph:

```sh
tail ../build/str?.d
```

issue for fixing this: https://github.com/WhatsApp/erlt/issues/184

