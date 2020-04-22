%% tests for remote constructors
-lang([erl2]).
-module(enum_mod02).

-export([p/0, q/1, r/1, s/1]).

%% alias for remote type using dot-qualified name
-type possibly(T) :: enum_mod01.possibly(T).

%% local type using remote constructors
-type perhaps(T) :: enum_mod01.possibly.some{T} | enum_mod01.possibly.none{}.


-spec p() -> possibly(any()).

p() ->
    %% only dots can be used for constructor module qualifiers, not colon
    enum_mod01.possibly.none{}.


-spec q(T) -> perhaps(T).

q(X) ->
    enum_mod01.possibly.some{X}.


-spec r(perhaps(T)) -> [T].

%% remote constructors in patterns
r(enum_mod01.possibly.some{X=41}) ->
    X+1;
r(E) ->
    case E of
        enum_mod01.possibly.none{} ->
            [];
        enum_mod01.possibly.some{X} ->
            [X]
    end.

-spec s(perhaps(T)) -> [T].

s(E) ->
    %% remote constructors in try-patterns
    try E of
        enum_mod01.possibly.none{} ->
            [];
        enum_mod01.possibly.some{X} ->
            [X]
    catch
        throw: enum_mod01.possibly.some{thing} : Trace ->
            {caught, Trace}
    end.
