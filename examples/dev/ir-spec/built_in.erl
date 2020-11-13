-file("dev/src/built_in.erlt", 1).

-module(built_in).

-export([use_format/0, use_lists/0]).

-spec use_format() -> string().

use_format() ->
    t_io:format("hello t_io~n"),
    t_io:format("~p~p~n", {1, 2}).

-spec use_lists() -> [number()].

use_lists() ->
    lists:filter(fun (N) -> N rem 2 =:= 0 end,
                 [1, 2, 3, 4, 5, 6]).



