-lang(st).

-module(etf08).

-export([get_id/1, get_id_id/1]).

-spec get_id(#{id := Id, _ := _}) -> Id.
get_id(Rec) ->
    Rec.id.

-spec get_id_id(#{id := #{id := Id, _ := _}, _ := _}) -> Id.
get_id_id(Rec) ->
    Rec.id.id.
