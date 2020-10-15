-file("dev/src/try_catch.erlt", 1).

-module(try_catch).

-export([t1/2]).

t1(F, Class) ->
    try F() catch
        Class:Reason:Stack -> {Reason, Stack};
        exit:Reason -> Reason;
        error:_ -> error;
        Reason -> Reason
    end.


