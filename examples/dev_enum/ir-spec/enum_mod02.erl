-file("dev_enum/src/enum_mod02.erlt", 1).

-module(enum_mod02).

-export([p/0, q/1, r/1, s/1]).

-type possibly(T) :: enum_mod01:possibly(T).

-spec p() -> possibly(any()).

p() -> {'$#enum_mod01:possibly.none'}.

q(X) -> {'$#enum_mod01:possibly.some', X}.

r({'$#enum_mod01:possibly.some', X = 41}) -> X + 1;
r(E) ->
    case E of
        {'$#enum_mod01:possibly.none'} -> [];
        {'$#enum_mod01:possibly.some', X} -> [X]
    end.

s(E) ->
    try E of
        {'$#enum_mod01:possibly.none'} -> [];
        {'$#enum_mod01:possibly.some', X} -> [X]
    catch
        throw:{'$#enum_mod01:possibly.some', thing}:Trace ->
            {caught, Trace}
    end.



