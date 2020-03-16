-module(enum_mod02).

-export([p/0, q/1, r/1]).


-spec p() -> enum_mod01:possibly(any()).
p() ->
    enum_mod02.none{}.

-spec q(T) -> enum_mod01:possibly(T).
q(X) ->
    enum_mod02.some{X}.

-spec r(enum_mod01:possibly(T)) -> [T].
r(E) ->
    case E of
        enum_mod02.none{} ->
            [];
        enum_mod02.some{X} ->
            [X]
    end.
