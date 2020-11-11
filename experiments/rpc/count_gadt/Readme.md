## Count GADT

This is count server is based of, of [countt](../countt), but has no generated code and all code is statically typed.
The static typing requires introducing only a single new typing construct a GADT, or generalised enum.

The statically typed code includes a GADT or generalized enum.
```erlang
-genum call_request(Result) :: {
    equal(integer()) -> call_request(boolean()),
    closer(integer(), integer()) -> call_request(integer())
}
```
This can be used to type the handle_call function:
```erlang
-spec handle_call(call_request(Result), pid(), state()) -> {reply, Result, state()}.
```

I have found examples of handle_call being typed using dialyzer before.
See how closely this represents the `genum` above:
```erlang
-spec handle_call
    ({equal, integer()}, pid(), state()) -> {reply, boolean(), state()};
    ({closer, integer(), integer()}, pid(), state()) -> {reply, integer(), state()}.
```

You play with the example and run it using:
```
$ make run
```
Or you can test it using:
```
$ make test
```

## GADTs

If you are new to GADTs you can see an example of an expression that can be evaluated for different types in haskell and a hypothetical erlt:
  - [Expr.hs](./Expr.hs)
  - [expr.erlt](./expr.erlgadt)

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