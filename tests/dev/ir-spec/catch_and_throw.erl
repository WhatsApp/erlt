-file("dev/src/catch_and_throw.erlt", 1).

-module(catch_and_throw).

-eqwalizer_unchecked([{test, 1}]).

-export([test/0]).

-type exn() :: {'$#catch_and_throw:exn', integer()}.

-spec test() -> [integer()].

test() -> [test_checked(X) || X <- [0, 1, 2, 3]].

-spec test(integer()) -> integer().

test(X) ->
    case X of
        0 -> erlang:throw({'$#catch_and_throw:exn', X});
        1 -> erlang:error({'$#catch_and_throw:exn', X});
        2 -> erlang:exit({'$#catch_and_throw:exn', X});
        _ -> X
    end.

test_checked(X) ->
    try test(X) catch {'$#catch_and_throw:exn', Y} -> Y end.



