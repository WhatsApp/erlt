-lang([erl2]).

-module(bad_anon_structs).

-export([test/0, test/1]).

-spec test() -> #(a::integer(), a::float()).

-type my_map(A, B) :: #(id :: A, id :: B).

-type bad_type() :: #(id :: atom() | A).

test() ->
    #(a=1,a=2).

test(#(a=X, a=Y)) ->
    ok.