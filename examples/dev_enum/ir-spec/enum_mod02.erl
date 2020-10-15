-file("dev_enum/src/enum_mod02.erlt", 1).

-module(enum_mod02).

-export([p/0, q/1, r/1, s/1]).

-type possibly(T) :: enum_mod01:possibly(T).

-type perhaps(T) :: {969696,
                     enum_mod01,
                     possibly,
                     some,
                     T} |
                    {969696, enum_mod01, possibly, none}.

-spec p() -> possibly(any()).

p() -> {969696, enum_mod01, possibly, none}.

-spec q(T) -> perhaps(T).

q(X) -> {969696, enum_mod01, possibly, some, X}.

-spec r(perhaps(T)) -> [T].

r({969696, enum_mod01, possibly, some, X = 41}) ->
    X + 1;
r(E) ->
    case E of
        {969696, enum_mod01, possibly, none} -> [];
        {969696, enum_mod01, possibly, some, X} -> [X]
    end.

-spec s(perhaps(T)) -> [T].

s(E) ->
    try E of
        {969696, enum_mod01, possibly, none} -> [];
        {969696, enum_mod01, possibly, some, X} -> [X]
    catch
        throw:{969696,
               enum_mod01,
               possibly,
               some,
               thing}:Trace ->
            {caught, Trace}
    end.



