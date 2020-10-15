-file("dev_struct/src/anon_struct_mod1.erlt", 1).


-module(anon_struct_mod1).


-export([test/0,test_in_lc/1]).


test() ->
    [ 
     (test(X)) ||
         X <-
             [#{},
              #{a => 1, b => 2},
              (#{a => 1, b => 2})#{},
              ((#{a => 1})#{b => 2})#{},
              (#{})#{a => 1, b => #{c => 2, d => 3}},
              map_get(a, #{a => 1, b => 2}),
              map_get(a, (#{a => 1})#{b => 2}),
              (map_get(b, (#{})#{a => 1, b => #{c => 2, d => 3}}))#{e =>
                                                                        4}]
    ],
    #{a := 1, b := 2} = mk_map(1, 2),
    #{a := 2} = update_map1(#{a => 1}, 2),
    #{a := 2, b := 1} = update_map3(mk_map(1, 2)),
    {x, {25, 36}} = access_map(#{id => x, location => {25, 36}}),
    ok = access_map2(#{inner1 => #{inner2 => ok}}).


-spec test(#{a := 1, b := #{c := 2, atom() => any()}, atom() => any()}) ->
              really_ok;
          (#{atom() => any()}) -> ok;
          (any()) -> not_ok.


test(A) when A =:= #{a => 1, b => #{c => 2}} ->
    closed_ok;
test(#{a := 1, b := #{c := 2}}) ->
    open_ok;
test(A) when map_get(b, A) =:= 2 ->
    ok;
test(A) when A =:= (#{a => 1})#{b => 2} ->
    ok;
test(#{a := 1}) ->
    ok;
test(#{}) ->
    ok;
test(_) ->
    not_ok.


test_in_lc(List) ->
    [ 
     Id ||
         #{id := Id} <- List
    ].


-spec mk_map(A, B) -> #{a := A, b := B}.


mk_map(A, B) ->
    #{a => A, b => B}.


-spec update_map1(#{a := A}, A) -> #{a := A}.


update_map1(M, A) ->
    M#{a => A}.


-spec update_map3(#{a := A, b := A}) -> #{a := A, b := A}.


update_map3(M) ->
    A0 = map_get(a, M),
    B0 = map_get(b, M),
    M#{a => B0, b => A0}.


-spec access_map(#{id := Id, location := Location, atom() => any()}) ->
                    {Id, Location}.


access_map(M) ->
    {map_get(id, M), map_get(location, M)}.


-spec access_map2(#{inner1 := #{inner2 := A}}) -> A.


access_map2(M) ->
    map_get(inner2, map_get(inner1, M)).





