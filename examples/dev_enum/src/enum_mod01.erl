-lang([erl2]).
-module(enum_mod01).

-export([f/0, g/0, p/0, q/1, r/1, s/2]).


%% old style tagged tuple for comparison
-opaque maybe(T) :: {some, T} | none.
-export_type([ maybe/1 ]).


%% proper enum declarations
-enum possibly(T) :: some{T} | none{}.

%% exporting an enum type
-export_type([ possibly/1 ]).

%% a non-exported enum type
-enum pair(T1, T2) :: p{T1, T2}.

%% a plain type that refers to constructors from another enum type
-type t(T) :: possibly.some{T} | possibly.none{}.
-export_type([ t/1 ]).

-spec f() -> maybe(any()).
f() ->
    none.

-spec g() -> maybe(boolean()).
g() ->
    {some, true}.


-spec p() -> possibly(any()).
p() ->
    possibly.none{}.

-spec q(T) -> possibly(T).
q(X) ->
    possibly.some{X}.

-spec r(possibly(T)) -> [T].
r(E) ->
    case E of
        possibly.none{} ->
            [];
        possibly.some{X} when X =:= possibly.none{} ->
            [];
        possibly.some{X} ->
            [X]
    end.


-spec s(T1, T2) -> pair(T1, T2).
s(H, T) ->
    pair.p{H, T}.
