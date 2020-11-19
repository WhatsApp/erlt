-file("dev_enum/src/enum_mod02.erlt", 1).

-module(enum_mod02).

-export([p/0, q/1, r/1, s/1]).

-type possibly(T) :: enum_mod01:possibly(T).

-spec p() -> possibly(any()).

-type
     lost_possibility() :: {'$#enum_mod02:lost_possibility',
                            possibly(string())}.

p() -> {'$#enum_mod01:possibly.none'}.

-spec q(T) -> possibly(T).

q(X) -> {'$#enum_mod01:possibly.some', X}.

-spec r(possibly(number())) -> [number()].

r({'$#enum_mod01:possibly.some', X = 41}) -> [X + 1];
r(E) ->
    case E of
        {'$#enum_mod01:possibly.none'} -> [];
        {'$#enum_mod01:possibly.some', X} -> [X]
    end.

-spec s(possibly(string())) -> [string()].

s(E) ->
    try E of
        {'$#enum_mod01:possibly.none'} -> [];
        {'$#enum_mod01:possibly.some', X} -> [X]
    catch
        {'$#enum_mod02:lost_possibility',
         {'$#enum_mod01:possibly.some', Loss}} ->
            [Loss]
    end.



