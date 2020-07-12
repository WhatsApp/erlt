%% Copyright (c) Facebook, Inc. and its affiliates.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-lang([erl2, st]).
-module(tutorial).

minimal() ->
    X = 0,
    Y = 1,
    {}.

basic_values() ->
    My_int = 0,
    My_str = "hello",
    Empty_record = #{},
    Unit = {},
    Tuple1 = {1, 2},
    Tuple2 = {My_int, My_str},
    Int_list = [My_int, My_int],

    My_profile_rec = #{name => "Ilya", languages => ["Erlang", "Scala"]},
    Location_rec = #{city => "London", country => "UK"},

    Bool1 = true,
    Bool2 = false,

    F_id = fun (X) -> X end,
    F_first = fun (X1, Y1) -> X1 end,
    F_second = fun (X2, Y2) -> Y2 end,
    F_const = fun () -> 1 end,
    {}.

enums() ->
    None = core.option.none{},
    SomeInt = core.option.some{1},
    SomeStr = core.option.some{"str"},
    L1 = [None, SomeInt],
    L2 = [None, SomeStr],
    % ill-typed
    % L3 = [SomeInt, SomeStr],
    {}.


id(X) -> X.
mkSome(X) -> core.option.some{X}.
mkList(E1, E2) -> [E1, E2].

funs() ->
    V1 = mkSome(1),
    V2 = mkSome(""),
    {}.

pieces() ->
    My_rec = #{n => 1, st => "", z => []},
    NVal = My_rec.n,
    #{n := N1, st := S1, z := Z1} = My_rec,
    #{n := N, st := St, z := Z} = My_rec,
    Rec1 = #{n => N, z => Z},
    ##{n := NVal} = My_rec,
    % transforming record:
    My_rec1 = My_rec #{n := 2},
    My_pair = {1, []},
    {Elem1, Elem2} = My_pair,
    {}.

list1(L) ->
    [] = L,
    L.

list2(L) ->
    [] = L,
    [].

list3(L) ->
    [H|T] = L,
    {H, T}.

list4(L) ->
    [E1, E2, E3] = L,
    {E1, E2, E3}.

map(F, Xs) ->
    case Xs of
        [] -> [];
        [X1 | Xs1] -> [F(X1) | map(F, Xs1)]
    end.

getIds(Recs) ->
    map(fun (X) -> X.id end, Recs).

%% named local funs
global(X, V) -> case X of true -> V; false -> global(true, V) end.

inside(Y) ->
    T = fun Local(X, V) -> case X of true -> V; false -> Local(true, V) end end,
    T(true, Y).

inside1() ->
    T = fun (X) -> X end,
    T.

inside2() ->
    T = fun Local (X) -> X end,
    T.

get_id(R) ->
    R.id.

% So, what to do if we want to process "heterogeneous" records together?
% Explicitly cast/restrict them!
extract(##{id := Id}) -> #{id => Id}.

records_example() ->
    % polymorphism - get_id is polymorphic
    Id1 = get_id(#{id => 1}),
    Id2 = get_id(#{id => "id", key => 3}),

    Rec1 = #{id => 1},
    Rec2 = #{id => 2, key => 3},

    % However, this is not well-typed:
    % Field mismatch: key
    % val Rec_list = [rec1, rec2]

    Rec_list = [extract(Rec1), extract(Rec2)],
    {}.

% record is a special "kind" of types
id_rec(R) ->
    ##{} = R,
    R.

get_id1(Rec) ->
    ##{id := IdVal} = Rec,
    IdVal.

update_a_b(R) ->
    R #{a := 1, b := 2}.
swap_a_b(R) ->
    R #{a := R.b, b := R.a}.

id01(A) -> A.
id02((A)) -> A.
id03(A) -> (A).

% enums

-enum my_option(A) :: none{} | some{A}.
-enum my_list(A) :: nil{} | cons{A, my_list(A)}.

isDefined1(Opt) ->
    case Opt of
        my_option.none{} -> false;
        my_option.some{_} -> true
    end.

map1(F, Xs) ->
    case Xs of
        my_list.nil{} ->
            my_list.nil {};
        my_list.cons{X1, Xs1} ->
            my_list.cons{F(X1), map1(F, Xs1)}
    end.

to_bools(Xs) ->
    map1(fun isDefined1/1, Xs).

onlySome(Xs) ->
    case Xs of
        my_list.nil{} ->
            my_list.nil{};
        my_list.cons{X1, Xs1} ->
            case X1 of
                my_option.none{} ->
                    onlySome(Xs1);
                my_option.some{A} ->
                    my_list.cons{my_option.some{A}, onlySome(Xs1)}
            end
    end.

dec_opt(Opt) ->
    my_option.none{} = Opt,
    1.

dec_opt2(Opt) ->
    my_option.some{V} = Opt,
    [V].

dec_opt3(Opt, V1, V2) ->
    my_option.some{F} = Opt,
    [F(V1), F(V2)].

dec_opt4(X, Y, Z) ->
    Id = fun (X) -> X end,
    OptId = my_option.some{Id},
    my_option.some{Id1} = OptId,
    my_option.some{Id2} = OptId,
    {Id1(X), Id1(Y), Id2(Z)}.
