-lang([erl2, st]).

-module(etf04).

-export([empty_list/1, id_list/1]).

-spec empty_list(list(_)) -> list(_).
empty_list([]) -> [].

-spec id_list(list(A)) -> [A].
id_list([X | []]) -> [X | []];
id_list([X | [Y | Z]]) -> [X | [ Y | Z]];
id_list([X | Y]) -> [X | Y];
id_list([]) -> [].
