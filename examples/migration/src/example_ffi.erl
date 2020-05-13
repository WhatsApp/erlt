-lang([erl2, ffi]).
-module(example_ffi).

-export_type([msg/0]).
-enum msg() :: log{string()} | stop{}.

-export([loop/0]).

-spec loop() -> {}.
loop() ->
    receive
        {timeout, _, msg.log{Msg}} ->
            io:format("Log: ~s~n", [Msg]),
            loop();
        {timeout, _, msg.stop{}} ->
            io:format("Bye~n", []),
            {}
    end.
