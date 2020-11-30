-file("typed_libs/src/typed_gb_trees.erlt", 1).

-module(typed_gb_trees).

-export([empty/0,
         is_empty/1,
         size/1,
         lookup/2,
         get/2,
         insert/3,
         update/3,
         enter/3,
         delete/2,
         delete_any/2,
         balance/1,
         is_defined/2,
         keys/1,
         values/1,
         to_list/1,
         from_orddict/1,
         smallest/1,
         largest/1,
         take/2,
         take_any/2,
         take_smallest/1,
         take_largest/1,
         iterator/1,
         iterator_from/2,
         next/1,
         map/2]).

-export_type([tree/2, iter/2]).

-import_type({erlang, [{option, 1}]}).

-opaque tree(Key, Value) :: {non_neg_integer(),
                             node(Key, Value)}.

-opaque iter(Key, Value) :: [node(Key, Value)].

-type node(K, V) :: {'$#typed_gb_trees:node.nil'} |
                    {'$#typed_gb_trees:node.branch',
                     K,
                     V,
                     node(K, V),
                     node(K, V)}.

-spec empty() -> tree(_K, _V).

empty() -> {0, {'$#typed_gb_trees:node.nil'}}.

-spec is_empty(tree(_K, _V)) -> boolean().

is_empty({0, {'$#typed_gb_trees:node.nil'}}) -> true;
is_empty(_) -> false.

-spec size(tree(_K, _V)) -> non_neg_integer().

size({Size, _}) when Size >= 0 -> Size.

-spec lookup(Key,
             tree(Key, Value)) -> erlang:option(Value).

lookup(Key, {_, T}) -> lookup_1(Key, T).

lookup_1(Key,
         {'$#typed_gb_trees:node.branch', Key1, _, Smaller, _})
    when Key < Key1 ->
    lookup_1(Key, Smaller);
lookup_1(Key,
         {'$#typed_gb_trees:node.branch', Key1, _, _, Bigger})
    when Key > Key1 ->
    lookup_1(Key, Bigger);
lookup_1(_,
         {'$#typed_gb_trees:node.branch', _, Value, _, _}) ->
    {'$#erlang:option.value', Value};
lookup_1(_, {'$#typed_gb_trees:node.nil'}) ->
    {'$#erlang:option.none'}.

-spec is_defined(Key, tree(Key, _Value)) -> boolean().

is_defined(Key, {_, T}) -> is_defined_1(Key, T).

is_defined_1(Key,
             {'$#typed_gb_trees:node.branch', Key1, _, Smaller, _})
    when Key < Key1 ->
    is_defined_1(Key, Smaller);
is_defined_1(Key,
             {'$#typed_gb_trees:node.branch', Key1, _, _, Bigger})
    when Key > Key1 ->
    is_defined_1(Key, Bigger);
is_defined_1(_,
             {'$#typed_gb_trees:node.branch', _, _, _, _}) ->
    true;
is_defined_1(_, {'$#typed_gb_trees:node.nil'}) -> false.

-spec get(Key, tree(Key, Value)) -> Value.

get(Key, {_, T}) -> get_1(Key, T).

get_1(Key,
      {'$#typed_gb_trees:node.branch', Key1, _, Smaller, _})
    when Key < Key1 ->
    get_1(Key, Smaller);
get_1(Key,
      {'$#typed_gb_trees:node.branch', Key1, _, _, Bigger})
    when Key > Key1 ->
    get_1(Key, Bigger);
get_1(_,
      {'$#typed_gb_trees:node.branch', _, Value, _, _}) ->
    Value.

-spec update(Key, Value, tree(Key, Value)) -> tree(Key,
                                                   Value).

update(Key, Val, {S, T}) ->
    T1 = update_1(Key, Val, T),
    {S, T1}.

update_1(Key, Value,
         {'$#typed_gb_trees:node.branch',
          Key1,
          V,
          Smaller,
          Bigger})
    when Key < Key1 ->
    {'$#typed_gb_trees:node.branch',
     Key1,
     V,
     update_1(Key, Value, Smaller),
     Bigger};
update_1(Key, Value,
         {'$#typed_gb_trees:node.branch',
          Key1,
          V,
          Smaller,
          Bigger})
    when Key > Key1 ->
    {'$#typed_gb_trees:node.branch',
     Key1,
     V,
     Smaller,
     update_1(Key, Value, Bigger)};
update_1(Key, Value,
         {'$#typed_gb_trees:node.branch',
          _,
          _,
          Smaller,
          Bigger}) ->
    {'$#typed_gb_trees:node.branch',
     Key,
     Value,
     Smaller,
     Bigger}.

-type
     insert_return() :: {'$#typed_gb_trees:insert_return.plain',
                         node()} |
                        {'$#typed_gb_trees:insert_return.rebalance',
                         node(),
                         integer(),
                         integer()}.

-spec insert(Key, Value, tree(Key, Value)) -> tree(Key,
                                                   Value).

insert(Key, Val, {S, T}) ->
    S1 = S + 1,
    {'$#typed_gb_trees:insert_return.plain', Result} =
        insert_1(Key, Val, T, S1 * S1),
    {S1, Result}.

insert_1(Key, Value,
         {'$#typed_gb_trees:node.branch',
          Key1,
          V,
          Smaller,
          Bigger},
         S)
    when Key < Key1 ->
    case insert_1(Key, Value, Smaller, S bsr 1) of
        {'$#typed_gb_trees:insert_return.rebalance',
         T1,
         H1,
         S1} ->
            T = {'$#typed_gb_trees:node.branch',
                 Key1,
                 V,
                 T1,
                 Bigger},
            {H2, S2} = count(Bigger),
            H = erlang:max(H1, H2) bsl 1,
            SS = S1 + S2 + 1,
            P = SS * SS,
            if H > P ->
                   {'$#typed_gb_trees:insert_return.plain',
                    balance(T, SS)};
               true ->
                   {'$#typed_gb_trees:insert_return.rebalance', T, H, SS}
            end;
        {'$#typed_gb_trees:insert_return.plain', T1} ->
            {'$#typed_gb_trees:insert_return.plain',
             {'$#typed_gb_trees:node.branch', Key1, V, T1, Bigger}}
    end;
insert_1(Key, Value,
         {'$#typed_gb_trees:node.branch',
          Key1,
          V,
          Smaller,
          Bigger},
         S)
    when Key > Key1 ->
    case insert_1(Key, Value, Bigger, S bsr 1) of
        {'$#typed_gb_trees:insert_return.rebalance',
         T1,
         H1,
         S1} ->
            T = {'$#typed_gb_trees:node.branch',
                 Key1,
                 V,
                 Smaller,
                 T1},
            {H2, S2} = count(Smaller),
            H = erlang:max(H1, H2) bsl 1,
            SS = S1 + S2 + 1,
            P = SS * SS,
            if H > P ->
                   {'$#typed_gb_trees:insert_return.plain',
                    balance(T, SS)};
               true ->
                   {'$#typed_gb_trees:insert_return.rebalance', T, H, SS}
            end;
        {'$#typed_gb_trees:insert_return.plain', T1} ->
            {'$#typed_gb_trees:insert_return.plain',
             {'$#typed_gb_trees:node.branch', Key1, V, Smaller, T1}}
    end;
insert_1(Key, Value, {'$#typed_gb_trees:node.nil'}, S)
    when S =:= 0 ->
    {'$#typed_gb_trees:insert_return.rebalance',
     {'$#typed_gb_trees:node.branch',
      Key,
      Value,
      {'$#typed_gb_trees:node.nil'},
      {'$#typed_gb_trees:node.nil'}},
     1,
     1};
insert_1(Key, Value, {'$#typed_gb_trees:node.nil'},
         _S) ->
    {'$#typed_gb_trees:insert_return.plain',
     {'$#typed_gb_trees:node.branch',
      Key,
      Value,
      {'$#typed_gb_trees:node.nil'},
      {'$#typed_gb_trees:node.nil'}}}.

-spec enter(Key, Value, tree(Key, Value)) -> tree(Key,
                                                  Value).

enter(Key, Val, T) ->
    case is_defined(Key, T) of
        true -> update(Key, Val, T);
        false -> insert(Key, Val, T)
    end.

count({'$#typed_gb_trees:node.branch',
       _,
       _,
       {'$#typed_gb_trees:node.nil'},
       {'$#typed_gb_trees:node.nil'}}) ->
    {1, 1};
count({'$#typed_gb_trees:node.branch', _, _, Sm, Bi}) ->
    {H1, S1} = count(Sm),
    {H2, S2} = count(Bi),
    {erlang:max(H1, H2) bsl 1, S1 + S2 + 1};
count({'$#typed_gb_trees:node.nil'}) -> {1, 0}.

-spec balance(tree(Key, Value)) -> tree(Key, Value).

balance({S, T}) -> {S, balance(T, S)}.

balance(T, S) -> balance_list(to_list_1(T), S).

balance_list(L, S) ->
    {T, []} = balance_list_1(L, S),
    T.

balance_list_1(L, S) when S > 1 ->
    Sm = S - 1,
    S2 = Sm div 2,
    S1 = Sm - S2,
    {T1, [{K, V} | L1]} = balance_list_1(L, S1),
    {T2, L2} = balance_list_1(L1, S2),
    T = {'$#typed_gb_trees:node.branch', K, V, T1, T2},
    {T, L2};
balance_list_1([{Key, Val} | L], 1) ->
    {{'$#typed_gb_trees:node.branch',
      Key,
      Val,
      {'$#typed_gb_trees:node.nil'},
      {'$#typed_gb_trees:node.nil'}},
     L};
balance_list_1(L, 0) ->
    {{'$#typed_gb_trees:node.nil'}, L}.

-spec from_orddict([{Key, Value}]) -> tree(Key, Value).

from_orddict(L) ->
    S = erlang:length(L),
    {S, balance_list(L, S)}.

-spec delete_any(Key, tree(Key, Value)) -> tree(Key,
                                                Value).

delete_any(Key, T) ->
    case is_defined(Key, T) of
        true -> delete(Key, T);
        false -> T
    end.

-spec delete(Key, tree(Key, Value)) -> tree(Key, Value).

delete(Key, {S, T}) when S >= 0 ->
    {S - 1, delete_1(Key, T)}.

delete_1(Key,
         {'$#typed_gb_trees:node.branch',
          Key1,
          Value,
          Smaller,
          Larger})
    when Key < Key1 ->
    Smaller1 = delete_1(Key, Smaller),
    {'$#typed_gb_trees:node.branch',
     Key1,
     Value,
     Smaller1,
     Larger};
delete_1(Key,
         {'$#typed_gb_trees:node.branch',
          Key1,
          Value,
          Smaller,
          Bigger})
    when Key > Key1 ->
    Bigger1 = delete_1(Key, Bigger),
    {'$#typed_gb_trees:node.branch',
     Key1,
     Value,
     Smaller,
     Bigger1};
delete_1(_,
         {'$#typed_gb_trees:node.branch',
          _,
          _,
          Smaller,
          Larger}) ->
    merge(Smaller, Larger).

merge(Smaller, {'$#typed_gb_trees:node.nil'}) ->
    Smaller;
merge({'$#typed_gb_trees:node.nil'}, Larger) -> Larger;
merge(Smaller, Larger) ->
    {Key, Value, Larger1} = take_smallest1(Larger),
    {'$#typed_gb_trees:node.branch',
     Key,
     Value,
     Smaller,
     Larger1}.

-spec take_any(Key,
               tree(Key, Value)) -> erlang:option({Value,
                                                   tree(Key, Value)}).

take_any(Key, Tree) ->
    case is_defined(Key, Tree) of
        true -> {'$#erlang:option.value', take(Key, Tree)};
        false -> {'$#erlang:option.none'}
    end.

-spec take(Key, tree(Key, Value)) -> {Value,
                                      tree(Key, Value)}.

take(Key, {S, T}) when S >= 0 ->
    {Value, Res} = take_1(Key, T),
    {Value, {S - 1, Res}}.

take_1(Key,
       {'$#typed_gb_trees:node.branch',
        Key1,
        Value,
        Smaller,
        Larger})
    when Key < Key1 ->
    {Value2, Smaller1} = take_1(Key, Smaller),
    {Value2,
     {'$#typed_gb_trees:node.branch',
      Key1,
      Value,
      Smaller1,
      Larger}};
take_1(Key,
       {'$#typed_gb_trees:node.branch',
        Key1,
        Value,
        Smaller,
        Bigger})
    when Key > Key1 ->
    {Value2, Bigger1} = take_1(Key, Bigger),
    {Value2,
     {'$#typed_gb_trees:node.branch',
      Key1,
      Value,
      Smaller,
      Bigger1}};
take_1(_,
       {'$#typed_gb_trees:node.branch',
        _Key,
        Value,
        Smaller,
        Larger}) ->
    {Value, merge(Smaller, Larger)}.

-spec take_smallest(tree(Key, Value)) -> {Key,
                                          Value,
                                          tree(Key, Value)}.

take_smallest({Size, Tree}) when Size >= 0 ->
    {Key, Value, Larger} = take_smallest1(Tree),
    {Key, Value, {Size - 1, Larger}}.

take_smallest1({'$#typed_gb_trees:node.branch',
                Key,
                Value,
                {'$#typed_gb_trees:node.nil'},
                Larger}) ->
    {Key, Value, Larger};
take_smallest1({'$#typed_gb_trees:node.branch',
                Key,
                Value,
                Smaller,
                Larger}) ->
    {Key1, Value1, Smaller1} = take_smallest1(Smaller),
    {Key1,
     Value1,
     {'$#typed_gb_trees:node.branch',
      Key,
      Value,
      Smaller1,
      Larger}}.

-spec smallest(tree(Key, Value)) -> {Key, Value}.

smallest({_, Tree}) -> smallest_1(Tree).

smallest_1({'$#typed_gb_trees:node.branch',
            Key,
            Value,
            {'$#typed_gb_trees:node.nil'},
            _Larger}) ->
    {Key, Value};
smallest_1({'$#typed_gb_trees:node.branch',
            _Key,
            _Value,
            Smaller,
            _Larger}) ->
    smallest_1(Smaller).

-spec take_largest(tree(Key, Value)) -> {Key,
                                         Value,
                                         tree(Key, Value)}.

take_largest({Size, Tree}) when Size >= 0 ->
    {Key, Value, Smaller} = take_largest1(Tree),
    {Key, Value, {Size - 1, Smaller}}.

take_largest1({'$#typed_gb_trees:node.branch',
               Key,
               Value,
               Smaller,
               {'$#typed_gb_trees:node.nil'}}) ->
    {Key, Value, Smaller};
take_largest1({'$#typed_gb_trees:node.branch',
               Key,
               Value,
               Smaller,
               Larger}) ->
    {Key1, Value1, Larger1} = take_largest1(Larger),
    {Key1,
     Value1,
     {'$#typed_gb_trees:node.branch',
      Key,
      Value,
      Smaller,
      Larger1}}.

-spec largest(tree(Key, Value)) -> {Key, Value}.

largest({_, Tree}) -> largest_1(Tree).

largest_1({'$#typed_gb_trees:node.branch',
           Key,
           Value,
           _Smaller,
           {'$#typed_gb_trees:node.nil'}}) ->
    {Key, Value};
largest_1({'$#typed_gb_trees:node.branch',
           _Key,
           _Value,
           _Smaller,
           Larger}) ->
    largest_1(Larger).

-spec to_list(tree(Key, Value)) -> [{Key, Value}].

to_list({_, T}) -> to_list(T, []).

to_list_1(T) -> to_list(T, []).

to_list({'$#typed_gb_trees:node.branch',
         Key,
         Value,
         Small,
         Big},
        L) ->
    to_list(Small, [{Key, Value} | to_list(Big, L)]);
to_list({'$#typed_gb_trees:node.nil'}, L) -> L.

-spec keys(tree(Key, _Value)) -> [Key].

keys({_, T}) -> keys(T, []).

keys({'$#typed_gb_trees:node.branch',
      Key,
      _Value,
      Small,
      Big},
     L) ->
    keys(Small, [Key | keys(Big, L)]);
keys({'$#typed_gb_trees:node.nil'}, L) -> L.

-spec values(tree(_Key, Value)) -> [Value].

values({_, T}) -> values(T, []).

values({'$#typed_gb_trees:node.branch',
        _Key,
        Value,
        Small,
        Big},
       L) ->
    values(Small, [Value | values(Big, L)]);
values({'$#typed_gb_trees:node.nil'}, L) -> L.

-spec iterator(tree(Key, Value)) -> iter(Key, Value).

iterator({_, T}) -> iterator_1(T).

iterator_1(T) -> iterator(T, []).

iterator({'$#typed_gb_trees:node.branch',
          _,
          _,
          {'$#typed_gb_trees:node.nil'},
          _} =
             T,
         As) ->
    [T | As];
iterator({'$#typed_gb_trees:node.branch', _, _, L, _} =
             T,
         As) ->
    iterator(L, [T | As]);
iterator({'$#typed_gb_trees:node.nil'}, As) -> As.

-spec iterator_from(Key, tree(Key, Value)) -> iter(Key,
                                                   Value).

iterator_from(S, {_, T}) -> iterator_1_from(S, T).

iterator_1_from(S, T) -> iterator_from(S, T, []).

iterator_from(S,
              {'$#typed_gb_trees:node.branch', K, _, _, T}, As)
    when K < S ->
    iterator_from(S, T, As);
iterator_from(_,
              {'$#typed_gb_trees:node.branch',
               _,
               _,
               {'$#typed_gb_trees:node.nil'},
               _} =
                  T,
              As) ->
    [T | As];
iterator_from(S,
              {'$#typed_gb_trees:node.branch', _, _, L, _} = T, As) ->
    iterator_from(S, L, [T | As]);
iterator_from(_, {'$#typed_gb_trees:node.nil'}, As) ->
    As.

-spec next(iter(Key, Value)) -> erlang:option({Key,
                                               Value,
                                               iter(Key, Value)}).

next([{'$#typed_gb_trees:node.branch', X, V, _, T}
      | As]) ->
    {'$#erlang:option.value', {X, V, iterator(T, As)}};
next([]) -> {'$#erlang:option.none'}.

-spec map(fun((Key, Value) -> NewValue),
          tree(Key, Value)) -> tree(Key, NewValue).

map(F, {Size, Tree}) -> {Size, map_1(F, Tree)}.

map_1(_, {'$#typed_gb_trees:node.nil'}) ->
    {'$#typed_gb_trees:node.nil'};
map_1(F,
      {'$#typed_gb_trees:node.branch',
       K,
       V,
       Smaller,
       Larger}) ->
    {'$#typed_gb_trees:node.branch',
     K,
     F(K, V),
     map_1(F, Smaller),
     map_1(F, Larger)}.



