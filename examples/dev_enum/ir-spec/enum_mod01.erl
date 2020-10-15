-file("dev_enum/src/enum_mod01.erlt", 1).

-module(enum_mod01).

-export([f/0, g/0, p/0, q/1, r/1, s/2]).

-opaque maybe(T) :: {some, T} | none.

-export_type([maybe/1]).

-type possibly(T) :: {969696,
                      enum_mod01,
                      possibly,
                      some,
                      T} |
                     {969696, enum_mod01, possibly, none}.

-export_type([possibly/1]).

-type pair(T1, T2) :: {969696,
                       enum_mod01,
                       pair,
                       p,
                       T1,
                       T2}.

-type t(T) :: {969696, enum_mod01, possibly, some, T} |
              {969696, enum_mod01, possibly, none}.

-export_type([t/1]).

-spec f() -> maybe(any()).

f() -> none.

-spec g() -> maybe(boolean()).

g() -> {some, true}.

-spec p() -> possibly(any()).

p() -> {969696, enum_mod01, possibly, none}.

-spec q(T) -> possibly(T).

q(X) -> {969696, enum_mod01, possibly, some, X}.

-spec r(possibly(T)) -> [T].

r(E) ->
    case E of
        {969696, enum_mod01, possibly, none} -> [];
        {969696, enum_mod01, possibly, some, X}
            when X =:= {969696, enum_mod01, possibly, none} ->
            [];
        {969696, enum_mod01, possibly, some, X} -> [X]
    end.

-spec s(T1, T2) -> pair(T1, T2).

s(H, T) -> {969696, enum_mod01, pair, p, H, T}.


