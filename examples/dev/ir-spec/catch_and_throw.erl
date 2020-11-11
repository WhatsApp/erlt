-file("dev/src/catch_and_throw.erlt", 1).

-module(catch_and_throw).

-export([test/0]).

-type exn() :: {'$#catch_and_throw:exn', integer()}.

-spec test() -> [integer()].

test() -> [test_checked(X) || X <- [0, 1, 2, 3]].

test(X) ->
    case X of
        0 -> erlang:throw({'$#catch_and_throw:exn', X});
        1 -> erlang:error({'$#catch_and_throw:exn', X});
        2 -> erlang:exit({'$#catch_and_throw:exn', X});
        _ -> X
    end.

test_checked(X) ->
    try test(X) catch
        {'$#catch_and_throw:exn', Y} -> Y;
        error:{'$#catch_and_throw:exn', Y}:Stck ->
            deal_with_stack_trace(Stck),
            Y;
        exit:{'$#catch_and_throw:exn', Y} -> Y
    end.

-spec deal_with_stack_trace([_X]) -> {}.

deal_with_stack_trace(Stck) ->
    io:format("~p~n", [Stck]).



