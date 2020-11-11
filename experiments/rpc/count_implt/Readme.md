## Count Impl T

This is an erlt version of [count_impl](../count_impl).

Count Impl T has a [spec file](./src/count_spec.erlt), which is used to generate the [server](./src/count_server.erlt) using [rpctc impl](../rpctc).

The [implementation](./src/count_server_impl.erlt) is kept clean and statically typed using Hindley-Milner.
The server is dynamically typed and generated and can be considered hidden, but still transparent to the user.

The generated code includes a naive and restricted "type class", which we emulate using a struct with fields for functions:
```erlang
-struct impl_module(Input, State) :: (
    init :: fun((Input) -> State),
    equal :: fun((integer(), State) -> {boolean(), State}),
    closer :: fun((integer(), integer(), State) -> {integer(), State}),
    inc :: fun((integer(), State) -> State),
    dec :: fun((integer(), State) -> State)
).
```
The instantiation of this typeclass is hidden a [header file](./src/count_server_impl.hrl),
because the idea of this implementation is that it would require adding this feature to the type system and the user would not have to do this part manually.

The count server can now be instantiated with different implementation modules:
```erlang
count_server:start(Name, InitArgs, count_server_impl:impl()),
```

You play with the example and run it using:
```
$ make run
```
Or you can test it using:
```
$ make test
```
You can also regenerate your code using, before running and testing, if you changed the rpc definitions using:
```
$ make generate
```

## Future Work

### Better Syntax for Name

This not a great syntax for the name, but it was the easiest way to hack it into the syntax for now.

```erlang
-rpc name() -> count_server.
```

### Generate impl in the impl header

We could easily generate [count_server_impl.hrl](https://github.com/WhatsApp/erlt/blob/rpctc/rpc/count_implt/src/count_server_impl.hrl)

## Issues

### Generated code is not statically typed

The generated code is dynamically typed, see [count_gadt_implt](../count_gadt_implt) for a statically typed version.

### Dispatch is not free

There is a cost to the dispatch from impl modules, which can be quite considerable.
The call of a class method is always to an unknown function, and it can among other things never be inlined or optimized in other ways.
