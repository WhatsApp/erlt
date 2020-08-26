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

-module(erlt_module_record).

%% The skeleton for this module is erl_id_trans.

%% This module only traverses legal Erlang code. This is most noticeable
%% in guards where only a limited number of expressions are allowed.
%% N.B. if this module is to be used as a basis for transforms then
%% all the error cases must be handled otherwise this module just crashes!

-export([parse_transform/2]).

%% The terminology:
%% MR - module record
%% RecordTag - the runtime tag for a module record
%% Module Record Form - corresponding (desugared) Erlang Form `-record(...).`
%% An example:
%% -module(ma_mod01).
%% -record(?MODULE:r1, {field1, field2}).
%% In this example:
%% MR: r1
%% RecordTag:
%% 'ma_mod01:r1'
%% Module Record AST:
%% `-record('ma_mod01:r1', {field1, field2}).`

-record(module_record_context, {
    %% The name of the current module
    module :: atom(),
    %% The map Module -> (map ModuleRecord -> RecordTag) (including the current module).
    module_record_tags :: map(),
    % The map Module -> [RecordForm]
    remote_module_record_forms :: map()
}).

parse_transform(Forms0, _Options) ->
    %% unfolds runtime names of the records
    Forms1 = desugar_module_record_attribute(Forms0),
    ModuleRecords =
        [{MR, RecordTag} || {attribute, _Line, module_record, {MR, RecordTag}} <- Forms1],
    ModuleRecordUsages =
        collect(Forms1, fun collect_record_usage/1),
    %% Optimization: if there is nothing to transform, there is no need to transform.
    case {ModuleRecords, ModuleRecordUsages} of
        {[], []} ->
            Forms1;
        _ ->
            do_parse_transform(Forms1, ModuleRecords, ModuleRecordUsages)
    end.

%% Unfolds
%% -record(module:mr, fields)
%% into
%% -module_record(mr, 'module:mr').
%% -record('module:mr', fields).
desugar_module_record_attribute([
    {attribute, Line, record, {{module_record, _Line3, Module, ModuleRecordName}, Fields}}
    | T
]) ->
    RecordTag = list_to_atom(
        lists:append([atom_to_list(Module), ":", atom_to_list(ModuleRecordName)])
    ),
    ModuleRecordAttr = {attribute, Line, module_record, {ModuleRecordName, RecordTag}},
    RecordAttr = {attribute, Line, record, {RecordTag, Fields}},
    [ModuleRecordAttr, RecordAttr | desugar_module_record_attribute(T)];
desugar_module_record_attribute([H | T]) ->
    [H | desugar_module_record_attribute(T)];
desugar_module_record_attribute([]) ->
    [].

do_parse_transform(Forms, ModuleRecords, ModuleRecordUsages) ->
    %% Collecting the context.
    %% TODO: check that there are no clashes between records.
    Context = init_module_record_context(Forms, ModuleRecords, ModuleRecordUsages),
    %% Unfolding all invocations #M:MR -> RecordTag
    Forms1 = forms(Forms, Context),
    RecordForms = maps:from_list([{N, R} || R = {attribute, _, record, {N, _}} <- Forms1]),
    %% For each `-module_record({mr, tag}).`
    %% adding the synthetic attribute:
    %% -module_record_def({tag, module_record_form}).
    Forms2 = lists:flatmap(
        fun (Form) -> inject_module_record_forms(Form, RecordForms) end,
        Forms1
    ),
    %% For each remote module which module records are used,
    %% inject all -record(record_tag, ...) forms for module records.
    Forms3 = lists:flatmap(
        fun (Form) -> inject_remote_module_record_forms(Form, Context) end,
        Forms2
    ),
    %% Done.
    Forms3.

%% forms(Fs,Context) -> lists:map(fun (F) -> form(F) end, Fs).
%% The skeleton of this function is from erl_id_trans.

forms([F0 | Fs0], Context) ->
    F1 = form(F0, Context),
    Fs1 = forms(Fs0, Context),
    [F1 | Fs1];
forms([], _Context) ->
    [].

%% -type form(Form,Context) -> Form.
%%  Here we show every known form and valid internal structure. We do not
%%  that the ordering is correct!

%% First the various attributes.
form({attribute, Line, module, Mod}, _Context) ->
    {attribute, Line, module, Mod};
%This is valid anywhere.
form({attribute, Line, file, {File, Line}}, _Context) ->
    {attribute, Line, file, {File, Line}};
form({attribute, Line, export, Es0}, Context) ->
    Es1 = farity_list(Es0, Context),
    {attribute, Line, export, Es1};
form({attribute, Line, import, {Mod, Is0}}, Context) ->
    Is1 = farity_list(Is0, Context),
    {attribute, Line, import, {Mod, Is1}};
form({attribute, Line, export_type, Es0}, Context) ->
    Es1 = farity_list(Es0, Context),
    {attribute, Line, export_type, Es1};
form({attribute, Line, optional_callbacks, Es0}, Context) ->
    try farity_list(Es0, Context) of
        Es1 ->
            {attribute, Line, optional_callbacks, Es1}
    catch
        _:_ ->
            {attribute, Line, optional_callbacks, Es0}
    end;
form({attribute, Line, compile, C}, _Context) ->
    {attribute, Line, compile, C};
form({attribute, Line, record, {Name, Defs0}}, Context) ->
    Defs1 = record_defs(Defs0, Context),
    {attribute, Line, record, {Name, Defs1}};
form({attribute, Line, asm, {function, N, A, Code}}, _Context) ->
    {attribute, Line, asm, {function, N, A, Code}};
form({attribute, Line, type, {N, T, Vs}}, Context) ->
    T1 = type(T, Context),
    Vs1 = variable_list(Vs, Context),
    {attribute, Line, type, {N, T1, Vs1}};
form({attribute, Line, opaque, {N, T, Vs}}, Context) ->
    T1 = type(T, Context),
    Vs1 = variable_list(Vs, Context),
    {attribute, Line, opaque, {N, T1, Vs1}};
form({attribute, Line, spec, {{N, A}, FTs}}, Context) ->
    FTs1 = function_type_list(FTs, Context),
    {attribute, Line, spec, {{N, A}, FTs1}};
form({attribute, Line, callback, {{N, A}, FTs}}, Context) ->
    FTs1 = function_type_list(FTs, Context),
    {attribute, Line, callback, {{N, A}, FTs1}};
%The general attribute.
form({attribute, Line, Attr, Val}, _Context) ->
    {attribute, Line, Attr, Val};
form({function, Line, Name0, Arity0, Clauses0}, Context) ->
    {Name, Arity, Clauses} = function(Name0, Arity0, Clauses0, Context),
    {function, Line, Name, Arity, Clauses};
%% Extra forms from the parser.
form({error, E}, _Context) ->
    {error, E};
form({warning, W}, _Context) ->
    {warning, W};
form({eof, Line}, _Context) ->
    {eof, Line}.

%% -type farity_list([Farity],Context) -> [Farity] when Farity <= {atom(),integer()}.

farity_list([{Name, Arity} | Fas], Context) ->
    [{Name, Arity} | farity_list(Fas, Context)];
farity_list([], _Context) ->
    [].

%% -type variable_list([Var],Context) -> [Var]

variable_list([{var, Line, Var} | Vs], Context) ->
    [{var, Line, Var} | variable_list(Vs, Context)];
variable_list([], _Context) ->
    [].

%% -type record_defs([RecDef],Context) -> [RecDef].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *parser*!

record_defs([{record_field, Line, {atom, La, A}, Val0} | Is], Context) ->
    Val1 = expr(Val0, Context),
    [{record_field, Line, {atom, La, A}, Val1} | record_defs(Is, Context)];
record_defs([{record_field, Line, {atom, La, A}} | Is], Context) ->
    [{record_field, Line, {atom, La, A}} | record_defs(Is, Context)];
record_defs(
    [{typed_record_field, {record_field, Line, {atom, La, A}, Val0}, Type} | Is],
    Context
) ->
    Val1 = expr(Val0, Context),
    Type1 = type(Type, Context),
    [
        {typed_record_field, {record_field, Line, {atom, La, A}, Val1}, Type1}
        | record_defs(Is, Context)
    ];
record_defs(
    [{typed_record_field, {record_field, Line, {atom, La, A}}, Type} | Is],
    Context
) ->
    Type1 = type(Type, Context),
    [
        {typed_record_field, {record_field, Line, {atom, La, A}}, Type1}
        | record_defs(Is, Context)
    ];
record_defs([], _Context) ->
    [].

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
pattern({map, Line, Ps0}, Context) ->
    Ps1 = pattern_list(Ps0, Context),
    {map, Line, Ps1};
pattern({map_field_exact, Line, K, V}, Context) ->
    Ke = expr(K, Context),
    Ve = pattern(V, Context),
    {map_field_exact, Line, Ke, Ve};
%%pattern({struct,Line,Tag,Ps0}) ->
%%    Ps1 = pattern_list(Ps0),
%%    {struct,Line,Tag,Ps1};
pattern({record, Line, Name0, Pfs0}, Context) ->
    Pfs1 = pattern_fields(Pfs0, Context),
    Name1 = resolve_record(Name0, Line, Context),
    {record, Line, Name1, Pfs1};
pattern({record_index, Line, Name0, Field0}, Context) ->
    Field1 = pattern(Field0, Context),
    Name1 = resolve_record(Name0, Line, Context),
    {record_index, Line, Name1, Field1};
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
    {op, Line, Op, L, R}.

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
gexpr({record_index, Line, Name0, Field0}, Context) ->
    Field1 = gexpr(Field0, Context),
    Name1 = resolve_record(Name0, Line, Context),
    {record_index, Line, Name1, Field1};
gexpr({record_field, Line, Rec0, Name0, Field0}, Context) ->
    Rec1 = gexpr(Rec0, Context),
    Name1 = resolve_record(Name0, Line, Context),
    Field1 = gexpr(Field0, Context),
    {record_field, Line, Rec1, Name1, Field1};
gexpr({record, Line, Name0, Inits0}, Context) ->
    Inits1 = grecord_inits(Inits0, Context),
    Name1 = resolve_record(Name0, Line, Context),
    {record, Line, Name1, Inits1};
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
expr({record_index, Line, Name0, Field0}, Context) ->
    Field1 = expr(Field0, Context),
    Name1 = resolve_record(Name0, Line, Context),
    {record_index, Line, Name1, Field1};
expr({record, Line, Name0, Inits0}, Context) ->
    Inits1 = record_inits(Inits0, Context),
    Name1 = resolve_record(Name0, Line, Context),
    {record, Line, Name1, Inits1};
expr({record_field, Line, Rec0, Name0, Field0}, Context) ->
    Rec1 = expr(Rec0, Context),
    Name1 = resolve_record(Name0, Line, Context),
    Field1 = expr(Field0, Context),
    {record_field, Line, Rec1, Name1, Field1};
expr({record, Line, Rec0, Name0, Upds0}, Context) ->
    Rec1 = expr(Rec0, Context),
    Name1 = resolve_record(Name0, Line, Context),
    Upds1 = record_updates(Upds0, Context),
    {record, Line, Rec1, Name1, Upds1};
expr({record_field, Line, Rec0, Field0}, Context) ->
    Rec1 = expr(Rec0, Context),
    Field1 = expr(Field0, Context),
    {record_field, Line, Rec1, Field1};
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
        {function, M, F, A} when is_atom(M), is_atom(F), is_integer(A) ->
            %% R10B-6: fun M:F/A. (Backward compatibility)
            {'fun', Line, {function, M, F, A}};
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

function_type_list([{type, Line, bounded_fun, [Ft, Fc]} | Fts], Context) ->
    Ft1 = function_type(Ft, Context),
    Fc1 = function_constraint(Fc, Context),
    [{type, Line, bounded_fun, [Ft1, Fc1]} | function_type_list(Fts, Context)];
function_type_list([Ft | Fts], Context) ->
    [function_type(Ft, Context) | function_type_list(Fts, Context)];
function_type_list([], _Context) ->
    [].

function_type({type, Line, 'fun', [{type, Lt, product, As}, B]}, Context) ->
    As1 = type_list(As, Context),
    B1 = type(B, Context),
    {type, Line, 'fun', [{type, Lt, product, As1}, B1]}.

function_constraint([C | Cs], Context) ->
    C1 = constraint(C, Context),
    [C1 | function_constraint(Cs, Context)];
function_constraint([], _Context) ->
    [].

constraint({type, Line, constraint, [{atom, L, A}, [V, T]]}, Context) ->
    V1 = type(V, Context),
    T1 = type(T, Context),
    {type, Line, constraint, [{atom, L, A}, [V1, T1]]}.

type({ann_type, Line, [{var, Lv, V}, T]}, Context) ->
    T1 = type(T, Context),
    {ann_type, Line, [{var, Lv, V}, T1]};
type({atom, Line, A}, _Context) ->
    {atom, Line, A};
type({integer, Line, I}, _Context) ->
    {integer, Line, I};
type({op, Line, Op, T}, Context) ->
    T1 = type(T, Context),
    {op, Line, Op, T1};
type({op, Line, Op, L, R}, Context) ->
    L1 = type(L, Context),
    R1 = type(R, Context),
    {op, Line, Op, L1, R1};
type({type, Line, binary, [M, N]}, Context) ->
    M1 = type(M, Context),
    N1 = type(N, Context),
    {type, Line, binary, [M1, N1]};
type({type, Line, 'fun', []}, _Context) ->
    {type, Line, 'fun', []};
type({type, Line, 'fun', [{type, Lt, any}, B]}, Context) ->
    B1 = type(B, Context),
    {type, Line, 'fun', [{type, Lt, any}, B1]};
type({type, Line, range, [L, H]}, Context) ->
    L1 = type(L, Context),
    H1 = type(H, Context),
    {type, Line, range, [L1, H1]};
type({type, Line, map, any}, _Context) ->
    {type, Line, map, any};
type({type, Line, map, Ps}, Context) ->
    Ps1 = map_pair_types(Ps, Context),
    {type, Line, map, Ps1};
type({type, Line, record, [{atom, La, N} | Fs]}, Context) ->
    Fs1 = field_types(Fs, Context),
    Name = resolve_record(N, La, Context),
    {type, Line, record, [{atom, La, Name} | Fs1]};
type(
    {type, Line, record, [QR = {qualified_record, {atom, _, _}, {atom, _, _}} | Fs]},
    Context
) ->
    Fs1 = field_types(Fs, Context),
    Name = resolve_record(QR, Line, Context),
    {type, Line, record, [Name | Fs1]};
type({remote_type, Line, [{atom, Lm, M}, {atom, Ln, N}, As]}, Context) ->
    As1 = type_list(As, Context),
    {remote_type, Line, [{atom, Lm, M}, {atom, Ln, N}, As1]};
type({type, Line, tuple, any}, _Context) ->
    {type, Line, tuple, any};
type({type, Line, tuple, Ts}, Context) ->
    Ts1 = type_list(Ts, Context),
    {type, Line, tuple, Ts1};
type({type, Line, union, Ts}, Context) ->
    Ts1 = type_list(Ts, Context),
    {type, Line, union, Ts1};
type({var, Line, V}, _Context) ->
    {var, Line, V};
type({user_type, Line, N, As}, Context) ->
    As1 = type_list(As, Context),
    {user_type, Line, N, As1};
type({type, Line, N, As}, Context) ->
    As1 = type_list(As, Context),
    {type, Line, N, As1}.

map_pair_types([{type, Line, map_field_assoc, [K, V]} | Ps], Context) ->
    K1 = type(K, Context),
    V1 = type(V, Context),
    [{type, Line, map_field_assoc, [K1, V1]} | map_pair_types(Ps, Context)];
map_pair_types([{type, Line, map_field_exact, [K, V]} | Ps], Context) ->
    K1 = type(K, Context),
    V1 = type(V, Context),
    [{type, Line, map_field_exact, [K1, V1]} | map_pair_types(Ps, Context)];
map_pair_types([], _Context) ->
    [].

field_types([{type, Line, field_type, [{atom, La, A}, T]} | Fs], Context) ->
    T1 = type(T, Context),
    [{type, Line, field_type, [{atom, La, A}, T1]} | field_types(Fs, Context)];
field_types([], _Context) ->
    [].

type_list([T | Ts], Context) ->
    T1 = type(T, Context),
    [T1 | type_list(Ts, Context)];
type_list([], _Context) ->
    [].

%% Implementation of Module Records

%% Traverse Forms, applying F to each sub-expression.
%% Returns the list of "truthy" outputs of F(SubExpr).
collect(Forms, F) ->
    do_collect(Forms, F, []).

%% Relies on the fact that Forms are composed **only** of:
%% lists, tuples, atoms, numbers and binaries.
do_collect([], _F, Acc) ->
    Acc;
do_collect([H | T], F, Acc) ->
    Acc1 = do_collect(T, F, Acc),
    do_collect(H, F, Acc1);
do_collect(X, _F, Acc) when is_number(X) ->
    Acc;
do_collect(X, _F, Acc) when is_atom(X) ->
    Acc;
do_collect(X, _F, Acc) when is_bitstring(X) ->
    Acc;
do_collect(X, F, Acc) when is_tuple(X) ->
    L = tuple_to_list(X),
    Acc1 = do_collect(L, F, Acc),
    case F(X) of
        false -> Acc1;
        Delta -> [Delta | Acc1]
    end.

collect_record_usage({qualified_record, Module, Record}) when
    is_atom(Module), is_atom(Record)
->
    {Module, Record};
collect_record_usage({qualified_record, {atom, _, Module}, {atom, _, Record}}) when
    is_atom(Module), is_atom(Record)
->
    {Module, Record};
collect_record_usage(_) ->
    false.

init_module_record_context(Forms, ModuleRecords, ModuleRecordUsages) ->
    [Module] = [M || {attribute, _, module, M} <- Forms],
    UsedModules = lists:usort(lists:map(fun ({M, _}) -> M end, ModuleRecordUsages)),
    RemoteInfo = [{M, load_remote_module_records(M)} || M <- UsedModules, M =/= Module],
    RemoteMapping = [
        {M, maps:from_list(RemoteModuleRecords)}
        || {M, {RemoteModuleRecords, _}} <- RemoteInfo
    ],
    RemoteRecordDefs = [{GR, RecordDefs} || {GR, {_, RecordDefs}} <- RemoteInfo],
    LocalMapping = {Module, maps:from_list(ModuleRecords)},
    #module_record_context{
        module = Module,
        module_record_tags = maps:from_list([LocalMapping | RemoteMapping]),
        remote_module_record_forms = maps:from_list(RemoteRecordDefs)
    }.

load_remote_module_records(Module) ->
    case code:module_status(Module) of
        not_loaded -> ok;
        loaded -> ok
    end,
    case code:is_loaded(Module) of
        {file, _} ->
            ok;
        false ->
            %% TODO: error message if GM cannot be found
            {module, _} = code:load_file(Module),
            ok
    end,
    Attributes = Module:module_info(attributes),
    ModuleRecords = [{MR, RecordTag} || {module_record, [{MR, RecordTag}]} <- Attributes],
    ModuleRecordDefs = [RecordDef || {module_record_def, [{_, RecordDef}]} <- Attributes],
    {ModuleRecords, ModuleRecordDefs}.

resolve_qualified_record(M, MR, ModuleRecordTags) ->
    %% TODO case maps:find(MA, QualifiedRecords) of error ->
    %% error message that MA is not defined
    {ok, ModuleRecords} = maps:find(M, ModuleRecordTags),
    %% TODO case maps:find(MR, ModuleRecords) of error ->
    %% error message that MR is not defined
    {ok, RecordTag} = maps:find(MR, ModuleRecords),
    RecordTag.

%% Used from expressions
resolve_record({qualified_record, M, MR}, _Line, Context) when is_atom(M), is_atom(MR) ->
    ModuleRecordTags = Context#module_record_context.module_record_tags,
    resolve_qualified_record(M, MR, ModuleRecordTags);
%% Used from types
resolve_record({qualified_record, {atom, Line, M}, {atom, _, MR}}, _Line, Context) when
    is_atom(M), is_atom(MR)
->
    ModuleRecordTags = Context#module_record_context.module_record_tags,
    Resolved = resolve_qualified_record(M, MR, ModuleRecordTags),
    {atom, Line, Resolved};
resolve_record(Name, _Line, Context) when is_atom(Name) ->
    CurrentModule = Context#module_record_context.module,
    ModuleRecordTags = Context#module_record_context.module_record_tags,
    LocalModuleRecordTags = maps:get(CurrentModule, ModuleRecordTags),
    case maps:find(Name, LocalModuleRecordTags) of
        {ok, RecordTag} -> RecordTag;
        error -> Name
    end.

%% Injects Forms for the module records defined in the current module
%% inside generated -module_record_def attibute
inject_module_record_forms(
    ModuleRecord = {attribute, Line, module_record, {_, RecordTag}},
    RecordForms
) ->
    %% TODO case maps:find(GR,Records) of error -> error message that GR is not defined.
    {ok, RecordForm} = maps:find(RecordTag, RecordForms),
    ModuleRecordDef = {attribute, Line, module_record_def, {RecordTag, RecordForm}},
    [ModuleRecord, ModuleRecordDef];
inject_module_record_forms(Form, _Records) ->
    [Form].

%% Injects forms for module records from other modules directly into the current module.
%% It injects the forms right after the -module() attribute.
%% Modules are sorted by name, and the order of record definitions (inside each remote module)
%% is preserved.
inject_remote_module_record_forms(ModuleAttribute = {attribute, _Line, module, _}, Context) ->
    RemoteModuleRecordForms = Context#module_record_context.remote_module_record_forms,
    RemoteModules = lists:usort(maps:keys(RemoteModuleRecordForms)),
    InjectedRecordForms = lists:flatmap(
        fun (Module) ->
            {ok, RecordDefs} = maps:find(Module, RemoteModuleRecordForms),
            RecordDefs
        end,
        RemoteModules
    ),
    [ModuleAttribute | InjectedRecordForms];
inject_remote_module_record_forms(Form, _Records) ->
    [Form].
