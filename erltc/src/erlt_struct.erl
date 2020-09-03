%% Copyright Ericsson AB 1996-2020. All Rights Reserved.
%% Copyright (c) 2020 Facebook, Inc. and its affiliates.
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

-module(erlt_struct).

%% The skeleton for this module is erl_id_trans.

%% This module only traverses legal Erlang code. This is most noticeable
%% in guards where only a limited number of expressions are allowed.
%% N.B. if this module is to be used as a basis for transforms then
%% all the error cases must be handled otherwise this module just crashes!

-export([parse_transform/2]).

-record(context, {
    module :: atom(),
    struct = [],
    structs = #{}
}).

parse_transform(Forms, _Options) ->
    Context = init_context(Forms),
    forms(Forms, Context).

init_context(Forms) ->
    [Module] = [M || {attribute, _, module, M} <- Forms],
    Structs = [Def || {attribute, _, struct, Def} <- Forms],
    #context{
        module = Module,
        structs = init_structs(Structs, Module)
    }.

init_structs(Defs, Module) ->
    Map = [{Tag, struct_info(Module, Tag, Fields)} || {_Name, {type, _, struct, {atom, _, Tag}, Fields}, _Args} <- Defs],
    maps:from_list(Map).

struct_info(Module, Tag, Fields) ->
    RuntimeTag = list_to_atom("$#" ++ atom_to_list(Module) ++ ":" ++ atom_to_list(Tag)),
    Anno = erl_anno:set_generated(true, erl_anno:new(0)),
    FieldsMap = [{Field, _Default = undefined} || {struct_field, _, {atom, _, Field}, _} <- Fields],
    {{atom, Anno, RuntimeTag}, FieldsMap}.


%% forms(Fs,Context) -> lists:map(fun (F) -> form(F) end, Fs).

forms([F0 | Fs0], Context) ->
    F1 =
        try form(F0, Context)
        catch
            {error, Loc, Info} ->
                {error, {Loc, ?MODULE, Info}}
        end,
    Fs1 = forms(Fs0, Context),
    [F1 | Fs1];
forms([], _Context) ->
    [].

form({attribute, Line, struct, {TypeName, StructType, Args}}, Context) ->
    {type, TypeLine, struct, {atom, _, Tag}, Fields} = StructType,
    {RuntimeTag, _} = map_get(Tag, Context#context.structs),
    Type = {type, TypeLine, tuple, [RuntimeTag | [Type || {struct_field, _, _Name, Type} <- Fields]]},
    {attribute, Line, type, {TypeName, Type, Args}};
form({function, Line, Name0, Arity0, Clauses0}, Context) ->
    {Name, Arity, Clauses} = function(Name0, Arity0, Clauses0, Context),
    {function, Line, Name, Arity, Clauses};
form(Other, _Context) ->
    Other.

%% -type function(atom(), integer(), [Clause], Context) -> {atom(),integer(),[Clause]}.

function(Name, Arity, Clauses0, Context) ->
    Clauses1 = clauses(Clauses0, Context),
    {Name, Arity, Clauses1}.

%% -type clauses([Clause],Context) -> [Clause].

clauses([C0 | Cs], Context) ->
    C1 = clause(C0, Context),
    [C1 | clauses(Cs, Context)];
clauses([], _Context) ->
    [].

%% -type clause(Clause,Context) -> Clause.

clause({clause, Line, H0, G0, B0}, Context) ->
    H1 = head(H0, Context),
    G1 = guard(G0, Context),
    B1 = exprs(B0, Context),
    {clause, Line, H1, G1, B1}.

%% -type head([Pattern],Context) -> [Pattern].

head(Ps, Context) -> patterns(Ps, Context).

%% -type patterns([Pattern],Context) -> [Pattern].
%%  These patterns are processed "sequentially" for purposes of variable
%%  definition etc.

patterns([P0 | Ps], Context) ->
    P1 = pattern(P0, Context),
    [P1 | patterns(Ps, Context)];
patterns([], _Context) ->
    [].

%% -type pattern(Pattern,Context) -> Pattern.
%%  N.B. Only valid patterns are included here.

pattern({var, Line, V}, _Context) ->
    {var, Line, V};
pattern({match, Line, L0, R0}, Context) ->
    L1 = pattern(L0, Context),
    R1 = pattern(R0, Context),
    {match, Line, L1, R1};
pattern({integer, Line, I}, _Context) ->
    {integer, Line, I};
pattern({char, Line, C}, _Context) ->
    {char, Line, C};
pattern({float, Line, F}, _Context) ->
    {float, Line, F};
pattern({atom, Line, A}, _Context) ->
    {atom, Line, A};
pattern({string, Line, S}, _Context) ->
    {string, Line, S};
pattern({nil, Line}, _Context) ->
    {nil, Line};
pattern({cons, Line, H0, T0}, Context) ->
    H1 = pattern(H0, Context),
    T1 = pattern(T0, Context),
    {cons, Line, H1, T1};
pattern({tuple, Line, Ps0}, Context) ->
    Ps1 = pattern_list(Ps0, Context),
    {tuple, Line, Ps1};
pattern({enum, Line, N0, A0, Ps0}, Context) ->
    %% remote enum reference Mod.Enum.Constructor{...}
    N1 = pattern(N0, Context),
    A1 = pattern(A0, Context),
    Ps1 = pattern_list(Ps0, Context),
    {enum, Line, N1, A1, Ps1};
pattern({map, Line, Ps0}, Context) ->
    Ps1 = pattern_list(Ps0, Context),
    {map, Line, Ps1};
pattern({map_field_exact, Line, K, V}, Context) ->
    Ke = expr(K, Context),
    Ve = pattern(V, Context),
    {map_field_exact, Line, Ke, Ve};
%% TODO: handle remote structs
pattern({struct, Line, {atom, _, Name}, Fields}, Context) ->
    {RuntimeTag, Def} = map_get(Name, Context#context.structs),
    Fields1 = struct_pattern(Fields, Def, Context),
    {tuple, Line, [RuntimeTag | Fields1]};
pattern({record, Line, Name, Pfs0}, Context) ->
    Pfs1 = pattern_fields(Pfs0, Context),
    {record, Line, Name, Pfs1};
pattern({record_index, Line, Name, Field0}, Context) ->
    Field1 = pattern(Field0, Context),
    {record_index, Line, Name, Field1};
pattern({record_field, Line, Rec0, Name, Field0}, Context) ->
    Rec1 = expr(Rec0, Context),
    Field1 = expr(Field0, Context),
    {record_field, Line, Rec1, Name, Field1};
pattern({record_field, Line, Rec0, Field0}, Context) ->
    Rec1 = expr(Rec0, Context),
    Field1 = expr(Field0, Context),
    {record_field, Line, Rec1, Field1};
pattern({bin, Line, Fs}, Context) ->
    Fs2 = pattern_grp(Fs, Context),
    {bin, Line, Fs2};
pattern({op, Line, Op, A}, _Context) ->
    {op, Line, Op, A};
pattern({op, Line, Op, L, R}, _Context) ->
    {op, Line, Op, L, R};
pattern({remote, Line, M, N}, _Context) ->
    {remote, Line, M, N}.

pattern_grp([{bin_element, L1, E1, S1, T1} | Fs], Context) ->
    S2 =
        case S1 of
            default ->
                default;
            _ ->
                expr(S1, Context)
        end,
    T2 =
        case T1 of
            default ->
                default;
            _ ->
                bit_types(T1, Context)
        end,
    [{bin_element, L1, expr(E1, Context), S2, T2} | pattern_grp(Fs, Context)];
pattern_grp([], _Context) ->
    [].

bit_types([], _Context) ->
    [];
bit_types([Atom | Rest], Context) when is_atom(Atom) ->
    [Atom | bit_types(Rest, Context)];
bit_types([{Atom, Integer} | Rest], Context) when is_atom(Atom), is_integer(Integer) ->
    [{Atom, Integer} | bit_types(Rest, Context)].

%% -type pattern_list([Pattern],Context) -> [Pattern].
%%  These patterns are processed "in parallel" for purposes of variable
%%  definition etc.

pattern_list([P0 | Ps], Context) ->
    P1 = pattern(P0, Context),
    [P1 | pattern_list(Ps, Context)];
pattern_list([], _Context) ->
    [].

%% -type pattern_fields([Field],Context) -> [Field].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

pattern_fields([{record_field, Lf, {atom, La, F}, P0} | Pfs], Context) ->
    P1 = pattern(P0, Context),
    [{record_field, Lf, {atom, La, F}, P1} | pattern_fields(Pfs, Context)];
pattern_fields([{record_field, Lf, {var, La, '_'}, P0} | Pfs], Context) ->
    P1 = pattern(P0, Context),
    [{record_field, Lf, {var, La, '_'}, P1} | pattern_fields(Pfs, Context)];
pattern_fields([], _Context) ->
    [].

%% -type guard([GuardTest],Context) -> [GuardTest].

guard([G0 | Gs], Context) when is_list(G0) ->
    [guard0(G0, Context) | guard(Gs, Context)];
guard(L, Context) ->
    guard0(L, Context).

guard0([G0 | Gs], Context) ->
    G1 = guard_test(G0, Context),
    [G1 | guard0(Gs, Context)];
guard0([], _Context) ->
    [].

guard_test(Expr = {call, Line, {atom, La, F}, As0}, Context) ->
    case erl_internal:type_test(F, length(As0)) of
        true ->
            As1 = gexpr_list(As0, Context),
            {call, Line, {atom, La, F}, As1};
        _ ->
            gexpr(Expr, Context)
    end;
guard_test(Any, Context) ->
    gexpr(Any, Context).

%% Before R9, there were special rules regarding the expressions on
%% top level in guards. Those limitations are now lifted - therefore
%% there is no need for a special clause for the toplevel expressions.
%% -type gexpr(GuardExpr,Context) -> GuardExpr.

gexpr({var, Line, V}, _Context) ->
    {var, Line, V};
gexpr({integer, Line, I}, _Context) ->
    {integer, Line, I};
gexpr({char, Line, C}, _Context) ->
    {char, Line, C};
gexpr({float, Line, F}, _Context) ->
    {float, Line, F};
gexpr({atom, Line, A}, _Context) ->
    {atom, Line, A};
gexpr({string, Line, S}, _Context) ->
    {string, Line, S};
gexpr({nil, Line}, _Context) ->
    {nil, Line};
gexpr({map, Line, Map0, Es0}, Context) ->
    [Map1 | Es1] = gexpr_list([Map0 | Es0], Context),
    {map, Line, Map1, Es1};
gexpr({map, Line, Es0}, Context) ->
    Es1 = gexpr_list(Es0, Context),
    {map, Line, Es1};
gexpr({map_field_assoc, Line, K, V}, Context) ->
    Ke = gexpr(K, Context),
    Ve = gexpr(V, Context),
    {map_field_assoc, Line, Ke, Ve};
gexpr({map_field_exact, Line, K, V}, Context) ->
    Ke = gexpr(K, Context),
    Ve = gexpr(V, Context),
    {map_field_exact, Line, Ke, Ve};
gexpr({cons, Line, H0, T0}, Context) ->
    H1 = gexpr(H0, Context),
    %They see the same variables
    T1 = gexpr(T0, Context),
    {cons, Line, H1, T1};
gexpr({tuple, Line, Es0}, Context) ->
    Es1 = gexpr_list(Es0, Context),
    {tuple, Line, Es1};
gexpr({enum, Line, E0, A0, Es0}, Context) ->
    %% local qualified enum reference Enum.Constructor{...}
    E1 = gexpr(E0, Context),
    A1 = gexpr(A0, Context),
    Es1 = gexpr_list(Es0, Context),
    {enum, Line, E1, A1, Es1};
gexpr({record_index, Line, Name, Field0}, Context) ->
    Field1 = gexpr(Field0, Context),
    {record_index, Line, Name, Field1};
gexpr({record_field, Line, Rec0, Name, Field0}, Context) ->
    Rec1 = gexpr(Rec0, Context),
    Field1 = gexpr(Field0, Context),
    {record_field, Line, Rec1, Name, Field1};
gexpr({record, Line, Name, Inits0}, Context) ->
    Inits1 = grecord_inits(Inits0, Context),
    {record, Line, Name, Inits1};
gexpr({call, Line, {atom, La, F}, As0}, Context) ->
    case erl_internal:guard_bif(F, length(As0)) of
        true ->
            As1 = gexpr_list(As0, Context),
            {call, Line, {atom, La, F}, As1}
    end;
% Guard bif's can be remote, but only in the module erlang...
gexpr({call, Line, {remote, La, {atom, Lb, erlang}, {atom, Lc, F}}, As0}, Context) ->
    case
        erl_internal:guard_bif(F, length(As0)) or
            erl_internal:arith_op(F, length(As0)) or
            erl_internal:comp_op(F, length(As0)) or
            erl_internal:bool_op(F, length(As0))
    of
        true ->
            As1 = gexpr_list(As0, Context),
            {call, Line, {remote, La, {atom, Lb, erlang}, {atom, Lc, F}}, As1}
    end;
gexpr({bin, Line, Fs}, Context) ->
    Fs2 = pattern_grp(Fs, Context),
    {bin, Line, Fs2};
gexpr({op, Line, Op, A0}, Context) ->
    case
        erl_internal:arith_op(Op, 1) or
            erl_internal:bool_op(Op, 1)
    of
        true ->
            A1 = gexpr(A0, Context),
            {op, Line, Op, A1}
    end;
gexpr({op, Line, Op, L0, R0}, Context) when Op =:= 'andalso'; Op =:= 'orelse' ->
    %% R11B: andalso/orelse are now allowed in guards.
    L1 = gexpr(L0, Context),
    %They see the same variables
    R1 = gexpr(R0, Context),
    {op, Line, Op, L1, R1};
gexpr({op, Line, '.', L0, R0}, Context) ->
    L1 = gexpr(L0, Context),
    R1 = gexpr(R0, Context),
    {op, Line, '.', L1, R1};
gexpr({op, Line, Op, L0, R0}, Context) ->
    case
        erl_internal:arith_op(Op, 2) or
            erl_internal:bool_op(Op, 2) or
            erl_internal:comp_op(Op, 2)
    of
        true ->
            L1 = gexpr(L0, Context),
            %They see the same variables
            R1 = gexpr(R0, Context),
            {op, Line, Op, L1, R1}
    end.

%% -type gexpr_list([GuardExpr],Context) -> [GuardExpr].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

gexpr_list([E0 | Es], Context) ->
    E1 = gexpr(E0, Context),
    [E1 | gexpr_list(Es, Context)];
gexpr_list([], _Context) ->
    [].

grecord_inits([{record_field, Lf, {atom, La, F}, Val0} | Is], Context) ->
    Val1 = gexpr(Val0, Context),
    [{record_field, Lf, {atom, La, F}, Val1} | grecord_inits(Is, Context)];
grecord_inits([{record_field, Lf, {var, La, '_'}, Val0} | Is], Context) ->
    Val1 = gexpr(Val0, Context),
    [{record_field, Lf, {var, La, '_'}, Val1} | grecord_inits(Is, Context)];
grecord_inits([], _Context) ->
    [].

%% -type exprs([Expression],Context) -> [Expression].
%%  These expressions are processed "sequentially" for purposes of variable
%%  definition etc.

exprs([E0 | Es], Context) ->
    E1 = expr(E0, Context),
    [E1 | exprs(Es, Context)];
exprs([], _Context) ->
    [].

%% -type expr(Expression,Context) -> Expression.

expr({var, Line, V}, _Context) ->
    {var, Line, V};
expr({integer, Line, I}, _Context) ->
    {integer, Line, I};
expr({float, Line, F}, _Context) ->
    {float, Line, F};
expr({atom, Line, A}, _Context) ->
    {atom, Line, A};
expr({string, Line, S}, _Context) ->
    {string, Line, S};
expr({char, Line, C}, _Context) ->
    {char, Line, C};
expr({nil, Line}, _Context) ->
    {nil, Line};
expr({cons, Line, H0, T0}, Context) ->
    H1 = expr(H0, Context),
    %They see the same variables
    T1 = expr(T0, Context),
    {cons, Line, H1, T1};
expr({lc, Line, E0, Qs0}, Context) ->
    Qs1 = lc_bc_quals(Qs0, Context),
    E1 = expr(E0, Context),
    {lc, Line, E1, Qs1};
expr({bc, Line, E0, Qs0}, Context) ->
    Qs1 = lc_bc_quals(Qs0, Context),
    E1 = expr(E0, Context),
    {bc, Line, E1, Qs1};
expr({tuple, Line, Es0}, Context) ->
    Es1 = expr_list(Es0, Context),
    {tuple, Line, Es1};
expr({enum, Line, N0, A0, Es0}, Context) ->
    N1 = expr(N0, Context),
    A1 = expr(A0, Context),
    Es1 = expr_list(Es0, Context),
    {enum, Line, N1, A1, Es1};
expr({map, Line, Map0, Es0}, Context) ->
    [Map1 | Es1] = exprs([Map0 | Es0], Context),
    {map, Line, Map1, Es1};
expr({map, Line, Es0}, Context) ->
    Es1 = exprs(Es0, Context),
    {map, Line, Es1};
expr({map_field_assoc, Line, K, V}, Context) ->
    Ke = expr(K, Context),
    Ve = expr(V, Context),
    {map_field_assoc, Line, Ke, Ve};
expr({map_field_exact, Line, K, V}, Context) ->
    Ke = expr(K, Context),
    Ve = expr(V, Context),
    {map_field_exact, Line, Ke, Ve};
%%expr({struct,Line,Tag,Es0}) ->
%%    Es1 = pattern_list(Es0),
%%    {struct,Line,Tag,Es1};
expr({record_index, Line, Name, Field0}, Context) ->
    Field1 = expr(Field0, Context),
    {record_index, Line, Name, Field1};
expr({record, Line, Name, Inits0}, Context) ->
    Inits1 = record_inits(Inits0, Context),
    {record, Line, Name, Inits1};
expr({record_field, Line, Rec0, Name, Field0}, Context) ->
    Rec1 = expr(Rec0, Context),
    Field1 = expr(Field0, Context),
    {record_field, Line, Rec1, Name, Field1};
expr({record, Line, Rec0, Name, Upds0}, Context) ->
    Rec1 = expr(Rec0, Context),
    Upds1 = record_updates(Upds0, Context),
    {record, Line, Rec1, Name, Upds1};
expr({record_field, Line, Rec0, Field0}, Context) ->
    Rec1 = expr(Rec0, Context),
    Field1 = expr(Field0, Context),
    {record_field, Line, Rec1, Field1};
%% TODO: handle remote structs
expr({struct, Line, {atom, _, Name}, Fields}, Context) ->
    {RuntimeTag, Def} = map_get(Name, Context#context.structs),
    Fields1 = struct_init(Fields, Def, Context),
    {tuple, Line, [RuntimeTag | Fields1]};
expr({block, Line, Es0}, Context) ->
    %% Unfold block into a sequence.
    Es1 = exprs(Es0, Context),
    {block, Line, Es1};
expr({'if', Line, Cs0}, Context) ->
    Cs1 = icr_clauses(Cs0, Context),
    {'if', Line, Cs1};
expr({'case', Line, E0, Cs0}, Context) ->
    E1 = expr(E0, Context),
    Cs1 = icr_clauses(Cs0, Context),
    {'case', Line, E1, Cs1};
expr({'receive', Line, Cs0}, Context) ->
    Cs1 = icr_clauses(Cs0, Context),
    {'receive', Line, Cs1};
expr({'receive', Line, Cs0, To0, ToEs0}, Context) ->
    To1 = expr(To0, Context),
    ToEs1 = exprs(ToEs0, Context),
    Cs1 = icr_clauses(Cs0, Context),
    {'receive', Line, Cs1, To1, ToEs1};
expr({'try', Line, Es0, Scs0, Ccs0, As0}, Context) ->
    Es1 = exprs(Es0, Context),
    Scs1 = icr_clauses(Scs0, Context),
    Ccs1 = icr_clauses(Ccs0, Context),
    As1 = exprs(As0, Context),
    {'try', Line, Es1, Scs1, Ccs1, As1};
expr({'fun', Line, Body}, Context) ->
    case Body of
        {clauses, Cs0} ->
            Cs1 = fun_clauses(Cs0, Context),
            {'fun', Line, {clauses, Cs1}};
        {function, F, A} ->
            {'fun', Line, {function, F, A}};
        %% {function,M,F,A} when is_atom(M), is_atom(F), is_integer(A) ->
        %%     %% R10B-6: fun M:F/A. (Backward compatibility)
        %%     {'fun',Line,{function,M,F,A}};
        {function, M0, F0, A0} ->
            %% R15: fun M:F/A with variables.
            M = expr(M0, Context),
            F = expr(F0, Context),
            A = expr(A0, Context),
            {'fun', Line, {function, M, F, A}}
    end;
expr({named_fun, Loc, Name, Cs}, Context) ->
    {named_fun, Loc, Name, fun_clauses(Cs, Context)};
expr({call, Line, F0, As0}, Context) ->
    %% N.B. If F an atom then call to local function or BIF, if F a
    %% remote structure (see below) then call to other module,
    %% otherwise apply to "function".
    F1 = expr(F0, Context),
    As1 = expr_list(As0, Context),
    {call, Line, F1, As1};
expr({'catch', Line, E0}, Context) ->
    %% No new variables added.
    E1 = expr(E0, Context),
    {'catch', Line, E1};
expr({match, Line, P0, E0}, Context) ->
    E1 = expr(E0, Context),
    P1 = pattern(P0, Context),
    {match, Line, P1, E1};
expr({bin, Line, Fs}, Context) ->
    Fs2 = pattern_grp(Fs, Context),
    {bin, Line, Fs2};
expr({op, Line, Op, A0}, Context) ->
    A1 = expr(A0, Context),
    {op, Line, Op, A1};
expr({op, Line, Op, L0, R0}, Context) ->
    L1 = expr(L0, Context),
    %They see the same variables
    R1 = expr(R0, Context),
    {op, Line, Op, L1, R1};
%% The following are not allowed to occur anywhere!
expr({remote, Line, M0, F0}, Context) ->
    M1 = expr(M0, Context),
    F1 = expr(F0, Context),
    {remote, Line, M1, F1}.

%% -type expr_list([Expression],Context) -> [Expression].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

expr_list([E0 | Es], Context) ->
    E1 = expr(E0, Context),
    [E1 | expr_list(Es, Context)];
expr_list([], _Context) ->
    [].

struct_init(Fields, Defs, Context) ->
    Fun = fun({Name, Default}) ->
        case find_field(Name, Fields) of
            {struct_field, _, _, Value} -> expr(Value, Context);
            %% TODO: do we really need expr here?
            error -> expr(Default, Context)
        end
    end,
    lists:map(Fun, Defs).

struct_pattern(Fields, Defs, Context) ->
    Fun = fun({Name, _Default}) ->
        case find_field(Name, Fields) of
            {struct_field, _, _, Value} -> pattern(Value, Context);
            %% TODO: do we really need expr here?
            error ->
                Anno = erl_anno:set_generated(true, erl_anno:new(1)),
                {var, Anno, '_'}
        end
    end,
    lists:map(Fun, Defs).

find_field(Name, [{struct_field, _, {atom, _, Name}, _} = Field | _]) ->
    Field;
find_field(Name, [_ | Rest]) ->
    find_field(Name, Rest);
find_field(_Name, []) ->
    error.

%% -type record_inits([RecordInit],Context) -> [RecordInit].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_inits([{record_field, Lf, {atom, La, F}, Val0} | Is], Context) ->
    Val1 = expr(Val0, Context),
    [{record_field, Lf, {atom, La, F}, Val1} | record_inits(Is, Context)];
record_inits([{record_field, Lf, {var, La, '_'}, Val0} | Is], Context) ->
    Val1 = expr(Val0, Context),
    [{record_field, Lf, {var, La, '_'}, Val1} | record_inits(Is, Context)];
record_inits([], _Context) ->
    [].

%% -type record_updates([RecordUpd],Context) -> [RecordUpd].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_updates([{record_field, Lf, {atom, La, F}, Val0} | Us], Context) ->
    Val1 = expr(Val0, Context),
    [{record_field, Lf, {atom, La, F}, Val1} | record_updates(Us, Context)];
record_updates([], _Context) ->
    [].

%% -type icr_clauses([Clause],Context) -> [Clause].

icr_clauses([C0 | Cs], Context) ->
    C1 = clause(C0, Context),
    [C1 | icr_clauses(Cs, Context)];
icr_clauses([], _Context) ->
    [].

%% -type lc_bc_quals([Qualifier],Context) -> [Qualifier].
%%  Allow filters to be both guard tests and general expressions.

lc_bc_quals([{generate, Line, P0, E0} | Qs], Context) ->
    E1 = expr(E0, Context),
    P1 = pattern(P0, Context),
    [{generate, Line, P1, E1} | lc_bc_quals(Qs, Context)];
lc_bc_quals([{b_generate, Line, P0, E0} | Qs], Context) ->
    E1 = expr(E0, Context),
    P1 = pattern(P0, Context),
    [{b_generate, Line, P1, E1} | lc_bc_quals(Qs, Context)];
lc_bc_quals([E0 | Qs], Context) ->
    E1 = expr(E0, Context),
    [E1 | lc_bc_quals(Qs, Context)];
lc_bc_quals([], _Context) ->
    [].

%% -type fun_clauses([Clause],Context) -> [Clause].

fun_clauses([C0 | Cs], Context) ->
    C1 = clause(C0, Context),
    [C1 | fun_clauses(Cs, Context)];
fun_clauses([], _Context) ->
    [].
