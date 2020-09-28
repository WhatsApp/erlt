-lang(st).

-module(etf11).

-export([case1/1, case2/2]).

-spec case1([_]) -> boolean().
case1(L) ->
    case L of
        [] -> true;
        _ -> false
    end.

-spec case2([_],[_]) -> boolean().
case2(L1, L2) ->
    case L1 of
        [] ->
            case L2 of
                [] -> true;
                _ -> false
            end;
        _ -> false
    end.
