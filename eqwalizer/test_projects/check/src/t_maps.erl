-module(t_maps).

-compile([export_all, nowarn_export_all]).

-type b() :: boolean().
-type n() :: number().
-type a() :: atom().

-spec empty_map_01() -> #{}.
empty_map_01() -> #{}.

-spec empty_map_02() -> map().
empty_map_02() -> #{}.

-spec empty_map_03() -> any().
empty_map_03() -> #{}.

-spec dict_map_01() ->
    #{integer() => atom()}.
dict_map_01() ->
    #{0 => zero, 1 => one}.

-spec dict_map_02_neg() ->
    #{integer() => atom()}.
dict_map_02_neg() ->
    #{0 => 1, 1 => 2}.

-spec dict_map_03_neg() ->
    #{integer() => atom()}.
dict_map_03_neg() ->
    #{0 => zero, 1 => 2}.

-spec dict_map_04() ->
    #{integer() => atom() | number()}.
dict_map_04() ->
    #{0 => zero, 1 => 2}.

-spec shape_01() ->
    #{zero := number(), one => number()}.
shape_01() ->
    #{zero => 0, one => 1}.

-spec shape_02_neg() ->
    #{zero := number(), one => number()}.
shape_02_neg() ->
    #{zero => 0, one => one}.

% only atom keys can be updated
% unconditionally
-spec update_req_non_atom_neg
    (map()) -> map().
update_req_non_atom_neg(M) ->
    M#{1 := 1}.

% it is not allowed to use
% a dict for unconditional update
-spec dict_update_req_neg
    (map()) -> map().
dict_update_req_neg(M) ->
    M#{one := 1}.

-spec empty_update1
    (map()) -> map().
empty_update1(M) ->
    M#{}.

-spec empty_update2_neg
    (any()) -> map().
empty_update2_neg(M) ->
    M#{}.

-spec empty_update3
    () -> #{}.
empty_update3() ->
    (#{})#{}.

-spec dict_update1
    (#{atom() => true})
  -> #{atom() => boolean()}.
dict_update1(D) ->
    D#{foo => false}.

-spec dict_update2
    (#{atom() => true})
  -> map().
dict_update2(D) ->
    D#{1 => bar}.

-spec shape_update1
    (#{foo => b(), bar => n()})
    -> #{foo => b(), bar => b()}.
shape_update1(S) ->
    S#{bar => true}.

-spec shape_update2
    (#{foo => b(), bar := n()})
        -> #{foo => b(), bar := b()}.
shape_update2(S) ->
    S#{bar := true}.

-spec shape_update3_neg
    (#{foo => b(), bar => n()})
        -> #{foo => b(), bar := b()}.
shape_update3_neg(S) ->
    S#{bar := true}.

-spec shape_update4_neg
    (any()) -> #{bar := b()}.
shape_update4_neg(S) ->
    S#{bar := true}.

-spec meet_dict1
    (#{b() | n() => any()}, #{a() => any()}) ->
     #{b() => any()}.
meet_dict1(D, D) -> D.

-spec meet_dict2_neg
    (#{b() | n() => any()}, #{a() => any()}) ->
    #{n() => any()}.
meet_dict2_neg(D, D) -> D.

-spec meet_shape1
    (#{a := a()}, #{a := b()}) ->
    #{a := b()}.
meet_shape1(S, S) -> S.

-spec meet_shape2_neg
    (#{a := a()}, #{a := b()}) ->
    #{a := n()}.
meet_shape2_neg(S, S) -> S.

-spec meet_shape3_neg
    (#{a := a()}, #{a := b()}) ->
    #{a => n()}.
meet_shape3_neg(S, S) -> S.

-spec meet_shape4
    (#{a => a()}, #{a => b() | n()}) ->
    #{a => b()}.
meet_shape4(S, S) -> S.

-spec meet_shape5_neg
    (#{a => a()}, #{a => b() | n()}) ->
    #{a := b()}.
meet_shape5_neg(S, S) -> S.

-spec meet_shape6
    (#{a := a()}, #{a => b() | n()}) ->
    #{a := b()}.
meet_shape6(S, S) -> S.

-spec meet_shape7
    (#{a := a()}, #{b := b()}) ->
    none().
meet_shape7(S, S) -> S.

% this seems to be really cool
-spec u_shape1
    (#{a := a()} | #{a := b(), b := b()}) ->
    (#{a := n()} | #{a := n(), b := b()}).
u_shape1(S) ->
    S#{a := 1}.

-spec u_shape2_neg
    (#{a := a()}) ->
    (#{a := a(), b := a()}).
u_shape2_neg(S) ->
    S#{b := foo}.

-spec u_shape3
    (#{a := a()}) ->
    (#{a := a(), b := a()}).
u_shape3(S) ->
    S#{b => foo}.

-spec u_empty1
    (#{}) ->
    (#{n() => a()}).
u_empty1(S) ->
    S#{1 => one}.

-spec shape@dict1
    (#{a := a()}, a(), a()) ->
    (#{a() => a()}).
shape@dict1(S, K, V) ->
    S#{K => V}.

-spec shape@dict2_neg
    (#{a := a()}, n(), a()) ->
    (#{a() => a()}).
shape@dict2_neg(S, K, V) ->
    S#{K => V}.

-spec shape@dict3_neg
    (#{a := a()}, n(), a()) ->
    (#{n() => a()}).
shape@dict3_neg(S, K, V) ->
    S#{K => V}.

-spec shape@dict4_neg
    (Shape, n(), a()) ->
    (Dict) when
    Shape :: #{a := a()},
    Dict :: #{n() => a()}.
shape@dict4_neg(S, K, V) ->
    S#{K => V}.

-spec shape@dict5_neg
    (Shape, n(), a()) ->
    (Dict) when
    Shape :: #{a => a()},
    Dict :: #{n() => a()}.
shape@dict5_neg(S, K, V) ->
    S#{K => V}.

-type foo_bar(F, B) ::
    #{foo := F, bar := B}.

-type foo_bar_opt(F, B) ::
    #{foo => F, bar => B}.

-spec foo_bar_u
    (foo_bar(F1, B1), F1, B1) ->
    foo_bar(F1, B1).
foo_bar_u(FB, F1, B1) ->
    FB#{foo := F1, bar := B1}.

-spec foo_bar_u_neg
    (foo_bar(F1, B1), F1, B1) ->
    foo_bar(B1, F1).
foo_bar_u_neg(FB, F1, B1) ->
    FB#{foo := F1, bar := B1}.

-spec foo_bar_u_opt
    (foo_bar_opt(F1, B1), F1, B1) ->
    foo_bar(F1, B1).
foo_bar_u_opt(FB, F1, B1) ->
    FB#{foo => F1, bar => B1}.

-spec foo_bar_u_opt_neg
    (foo_bar_opt(F1, B1), F1, B1) ->
    foo_bar_opt(B1, F1).
foo_bar_u_opt_neg(FB, F1, B1) ->
    FB#{foo => F1, bar => B1}.

-type kv(K, V) :: #{K => V}.

-spec kvs(kv(K1, V1), K2, V2) ->
    kv(K1 | K2, V1 | V2).
kvs(Dict, K2, V2) ->
    Dict#{K2 => V2}.

-spec kvs_neg(kv(K1, V1), K2, V2) ->
    kv(K1 | K2, V1 | V2).
kvs_neg(Dict, K2, V2) ->
    Dict#{V2 => K2}.

-spec lit_type_neg(a) ->
    #{a => number()}.
lit_type_neg(A) -> #{A => 3}.

-spec needs_shape_a
    (#{a := any()}) -> ok.
needs_shape_a(_) -> ok.

-spec needs_shape_ab
    (#{a := any(), b := any()}) -> ok.
needs_shape_ab(_) -> ok.

-spec shapeab_neg(#{a := 3}) -> ok.
shapeab_neg(X) ->
    needs_shape_a(X),
    needs_shape_a(X#{b => hello}).

-spec shape_ab(#{a := any()}) -> ok.
shape_ab(X) ->
    needs_shape_a(X),
    needs_shape_ab(X#{b => hello}).
