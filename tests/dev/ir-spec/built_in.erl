-file("dev/src/built_in.erlt", 1).

-module(built_in).

-unchecked([]).

-export([use_format/0, use_lists/0]).

-spec use_format() -> atom().

use_format() ->
    t_io:format("hello t_io~n"),
    t_io:format("~p~p~n", {1, 2}).

-spec use_lists() -> [number()].

use_lists() ->
    L = [1, 3, 2, 7, 5, 4, 0],
    t_io:format("~p",
                {[] ++ t_lists:sort(fun (A, B) -> A < B end, L)}),
    t_io:format("~p",
                {[] ++ t_lists:flatten([[1], [2], [3]], L)}),
    [] ++ t_lists:filter(fun (N) -> N rem 2 =:= 0 end, L).



