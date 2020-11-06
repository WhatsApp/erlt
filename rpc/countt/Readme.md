## Count T

This is count server is a gen server, which has no generated code and all code is statically typed.
This show cases how statically typing calls can result in some enum weirdness, which [count_gadt](../count_gadt) solves.

The statically typed code includes two enums for requests and responses from handle_call.
```erlang
-enum call_request() ::
    equal{integer()}
    | closer{integer(), integer()}.

-enum call_response() ::
    equal{boolean()}
    | closer{integer()}.
```
This can be used to type the handle_call function:
```erlang
-spec handle_call(call_request(), pid(), state()) -> {reply, call_response(), state()}.
```

The problem occurs in the `equal` and `closer` functions, where we can see that not all cases of the enum are matched on:
```erlang
equal(Name, Arg0) ->
    call_response.equal{Ans} = gen_server:call(Name, call_request.equal{Arg0}),
    Ans.
```

You play with the example and run it using:
```
$ make run
```
Or you can test it using:
```
$ make test
```
