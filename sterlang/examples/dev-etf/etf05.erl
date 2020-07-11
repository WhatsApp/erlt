-lang([erl2, st]).

-module(etf05).

-export([tuple0/1, tuple1/1, tuple2/1, tuple3/1]).

-spec tuple0({}) -> {}.
tuple0([]) -> [].

-spec tuple1({X}) -> {X}.
tuple1({X}) -> {X}.

-spec tuple2({X, Y}) -> {X, Y}.
tuple2({X, Y}) -> {X, Y}.

-spec tuple3({X, Y, Z}) -> {X, Y, Z}.
tuple3({X, Y, {}}) -> {X, Y, {}};
tuple3({X, Y, Z}) -> {X, Y, Z}.