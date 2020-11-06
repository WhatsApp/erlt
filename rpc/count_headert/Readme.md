# Count Header T

This is an erlt version of [count_header](../count_header).

`count header t` shows how boiler plate code from an RPC server can be factored out into a
[header file](./src/count_server_boilerplate.hrl) and generated using [rpctc header](../rpctc) to keep the [implementation](./src/count_server.erlt) statically typed using Hindley-Milner.

The header file would contain dynamically typed code for handle_call, etc, but it is all generated and can be considered hidden, but still transparent to the user.
The implementation code, that the user writes, is statically typable via Hindley-Milner.

In this repo we include two examples:
  - A classic implementation of count gen server [count_server.erlt](./src/count_server.erlt)
  - A singleton implementation of count gen server [count_singleton.erlt](./src/count_singleton.erlt)

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

We didn't implement a lot of accompanying ideas in this single example, we just want to show case the use of headers.
Here follows other ideas that can easily go with and enhance the use of generated headers.

### One module can implement multiple services

The shortcut rpc syntax, which is used to specify the service doesn't show case the original idea.
In this example we define the rpc spec using 3 separate attributes:
```erlang
-rpc equal(integer()) -> boolean().
-rpc inc(integer()) -> no_return().
-rpc dec(integer()) -> no_return().
```

The original idea was to define it all in one service attribute:
```erlang
-service count_server :: (
    [call]
    equal(integer()) -> boolean(),

    [cast]
    inc(integer()),

    [cast]
    dec(integer())
).
```

This would allow more than one service to be defined per module.
```erlang
-service a_different_service :: (...).
```

But this is not implemented here, partly because of the syntax shortcut and partly to highlight the bigger differences between alternatives, like [count_implt](../count_implt).

### More Customization

It would be easy to pass more options to the generator using another parsable attribute.
This could even result in the use implementing their own `handle_info` function, instead using the generated one.

```erlang
-generate_gen_server(count_server, #(
    custom_handle_info=true,
    type=singleton
)).

[callback, dynamic]
handle_info() -> ...
```

## Issues

### Confusing Routing

If two servers have the same rpc layer definition, for example a string_server:

```erlang
-rpc equal(integer()) -> boolean().
-rpc inc(integer()) -> no_return().
-rpc dec(integer()) -> no_return().

-include("string_server_boilerplate.hrl").

-type input() :: string().
-type state() :: string().
```

Now the count_server method can be used to route messages for the string_server:

```erlang
confusing_routing(_Config) ->
    count_server:start(ints, 10),
    string_server:start(strings, "0"),
    true = string_server:equal(strings, "0"),
    true = count_server:equal(strings, "0"),
```

See the `confusing_routing` test in [./test/count_server_SUITE.erl](./test/count_server_SUITE.erl)

### New Reserved Words

Since implementation functions are prefixed with `handle_` we have introduced some implicit reserved words: `info`, `call` and `cast`.

### Generated code is not statically typed

The generated code is dynamically typed, see [count_gadt_headert](../count_gadt_headert) for a statically typed version.