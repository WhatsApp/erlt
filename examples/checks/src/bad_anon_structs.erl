-lang([erl2]).

-module(bad_anon_structs).

-export([test/0, test/1]).

-spec test() -> #(a::integer(), a::float()).

test() ->
    #(a=1,a=2).

test(#(a=X, a=Y)) ->
    ok.