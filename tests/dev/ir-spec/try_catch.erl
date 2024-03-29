-file("dev/src/try_catch.erlt", 1).

-module(try_catch).

-eqwalizer_unchecked([{t1, 2}]).

-export([t1/2, t2/1]).

-type reason() :: {'$#try_catch:reason'}.

t1(F, Class) ->
    try F() catch
        Class:Reason:Stack -> {Reason, Stack};
        exit:Reason -> Reason;
        error:_ -> error;
        Reason -> Reason
    end.

-spec t2(_F) -> string().

t2(_F) ->
    try "ok" catch {'$#try_catch:reason'} -> "ok" end.



