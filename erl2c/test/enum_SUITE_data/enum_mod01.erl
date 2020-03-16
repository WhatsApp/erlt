-module(enum_mod01).

-export([f/0, g/0, p/0, q/1, r/1, s/2]).


-opaque maybe(T) :: {some, T} | none.
-export_type([ maybe/1 ]).


-enum possibly(T) :: some{T} | none{}.
-export_type([ possibly/1 ]).

-type pair(T1, T2) :: p{T1, T2}.

-spec f() -> maybe(any()).
f() ->
    none.

-spec g() -> maybe(boolean()).
g() ->
    {some, true}.


-spec p() -> possibly(any()).
p() ->
    none{}.

-spec q(T) -> possibly(T).
q(X) ->
    some{X}.

-spec r(possibly(T)) -> [T].
r(E) ->
    case E of
        none{} ->
            [];
        some{X} ->
            [X]
    end.


-spec s(T1, T2) -> pair(T1, T2).
s(H, T) ->
    p{H, T}.
