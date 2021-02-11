-module(wip_maps).

-compile([export_all, nowarn_export_all]).

% only atom keys can be updated
% unconditionally
-spec update_req_non_atom_neg
    (map()) -> map().
update_req_non_atom_neg(M) ->
    M#{1 := 1}.

% "mixed" updates are not supported
% (they are not used in WA codebase)
-spec bad_mixed_update1
    (#{a := any()}) -> #{a := a, b := b}.
bad_mixed_update1(M) ->
    M#{a := a, b => b}.

-spec bad_mixed_update2
    (#{a := any()}) -> #{a := a, b := b}.
bad_mixed_update2(M) ->
    M#{b => b, a := a}.
