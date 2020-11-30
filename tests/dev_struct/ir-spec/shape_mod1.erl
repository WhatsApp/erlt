-file("dev_struct/src/shape_mod1.erlt", 1).

-module(shape_mod1).

-export([test/0, test/1, test2/1, test_in_lc/1]).

-spec test() -> integer().

test() ->
    erlang:size({#{},
                 test3(#{a => 1, b => 2}),
                 test4((#{a => 1, b => 2})#{}),
                 test5(((#{a => 1})#{b => 2})#{}),
                 erlang:map_get(a, #{a => 1, b => 2}),
                 erlang:map_get(a, (#{a => 1})#{b => 2}),
                 (erlang:map_get(b,
                                 (#{})#{a => 1, b => #{c => 2, d => 3}}))#{e =>
                                                                               4},
                 #{a := 1, b := 2} = mk_map(1, 2),
                 #{a := 2} = update_map1(#{a => 1}, 2),
                 #{a := 2, b := 1} = update_map3(mk_map(1, 2)),
                 {x, {25, 36}} = access_map(#{id => x,
                                              location => {25, 36}}),
                 ok = access_map2(#{inner1 => #{inner2 => ok}})}).

-spec test(#{a := integer(),
             b := #{c := integer()}}) -> atom().

test(A) when A =:= #{a => 1, b => #{c => 2}} ->
    closed_ok;
test(#{a := 1, b := #{c := 2}}) -> open_ok.

-spec test2(#{}) -> {}.

test2(#{}) -> {}.

test3(A) when erlang:map_get(b, A) =:= 2 -> {}.

test4(A) when A =:= (#{a => 1, b => 1})#{b => 2} -> {}.

test5(#{a := 1}) -> {}.

-spec test_in_lc([#{id := A, atom() => any()}]) -> [A].

test_in_lc(List) -> [Id || #{id := Id} <- List].

-spec mk_map(A, B) -> #{a := A, b := B}.

mk_map(A, B) -> #{a => A, b => B}.

-spec update_map1(#{a := A}, A) -> #{a := A}.

update_map1(M, A) -> M#{a => A}.

-spec update_map3(#{a := A, b := A}) -> #{a := A,
                                          b := A}.

update_map3(M) ->
    A0 = erlang:map_get(a, M),
    B0 = erlang:map_get(b, M),
    M#{a => B0, b => A0}.

-spec access_map(#{id := Id, location := Location,
                   atom() => any()}) -> {Id, Location}.

access_map(M) ->
    {erlang:map_get(id, M), erlang:map_get(location, M)}.

-spec access_map2(#{inner1 := #{inner2 := A}}) -> A.

access_map2(M) ->
    erlang:map_get(inner2, erlang:map_get(inner1, M)).



