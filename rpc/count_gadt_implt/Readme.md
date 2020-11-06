## Count GADT implt

This is count server is based of, of [count_implt](../count_implt), but has well typed generated code.
Statically typing the generated code, requires introducing only a single new typing construct a GADT, or generalised enum.

Just like [count_implt](../count_implt) count GADT has a [spec file](./src/count_spec.erlt), which is used to generate the [server](./src/count_server.erlt) using [rpctc gadtimpl](../rpctc), but we could easily swap it out for the header file method used in [count_headert](../count_headert).
Try to focus on the GADT part of this implementation, rather than how the rpc is specified and provided as input to the generator.

The [implementation](./src/count_server_impl.erlt) is kept clean and statically typed using Hindley-Milner.
Unlike other generated servers, the generated code is now also statically typed, since it is possible to type handle_call using a gadt.
The generated code can be considered hidden, but still transparent to the user.

The generated code includes a GADT or generalized enum.
```erlang
-genum call_request(Result) :: {
    equal(integer()) -> call_request(boolean()),
    closer(integer(), integer()) -> call_request(integer())
}
```
This can be used to type the handle_call function:
```erlang
-spec handle_call(call_request(Result), pid(), State) -> {reply, Result, State}.
```

I have found examples of handle_call being typed using dialyzer before.
See how closely this represents the `genum` above:
```erlang
-spec handle_call
    ({equal, integer()}, pid(), State) -> {reply, boolean(), State};
    ({closer, integer(), integer()}, pid(), State) -> {reply, integer(), State}.
```

We can also compare our original rpc spec with the GADT and if we squint the `genum`, handle_call spec and rpc spec all look the same:
```erlang
-rpc equal(integer()) -> boolean().
-rpc closer(integer(), integer()) -> integer().
```

The generated code also includes a naive and restricted "type class", see [count_implt](../count_implt) for more details.

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

## GADTs

If you want to see GADTs in action without any generated code at play, see [count_gadt](../count_gadt)

## Future Work

### Better Syntax for the GADT

Syntax can be discussed at a future date, what is more important is whether we want to introduce GADTs into the language.
When that has been decided we can bikeshed the syntax.

```erlang
-genum call_request(Result) :: {
    equal(integer()) -> call_request(boolean()),
    closer(integer(), integer()) -> call_request(integer())
}
```