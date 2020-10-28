-file("dev_enum/src/enum_mod01.erlt", 1).

-module(enum_mod01).

-export([f/0, g/0, p/0, q/1, r/2, s/2]).

-type possibly(T) :: {'$#enum_mod01:possibly.some', T} |
                     {'$#enum_mod01:possibly.none'}.

-export_type([possibly/1]).

-type pair(T1, T2) :: {'$#enum_mod01:pair.p', T1, T2}.

-spec f() -> term().

f() -> none.

-spec g() -> term().

g() -> {some, true}.

-spec p() -> possibly(any()).

p() -> {'$#enum_mod01:possibly.none'}.

-spec q(T) -> possibly(T).

q(X) -> {'$#enum_mod01:possibly.some', X}.

-spec r(possibly(T), T) -> [T].

r(E, Y) ->
    case E of
        {'$#enum_mod01:possibly.none'} -> [];
        {'$#enum_mod01:possibly.some', X}
            when X =:= {'$#enum_mod01:possibly.none'} ->
            [];
        {'$#enum_mod01:possibly.some', Y} -> [];
        {'$#enum_mod01:possibly.some', X} -> [X]
    end.

-spec s(T1, T2) -> pair(T1, T2).

s(H, T) -> {'$#enum_mod01:pair.p', H, T}.



