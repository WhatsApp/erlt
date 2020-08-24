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

-module(erlt_enum).

%% The skeleton for this module is erl_id_trans.

%% This module only traverses legal Erlang code. This is most noticeable
%% in guards where only a limited number of expressions are allowed.
%% N.B. if this module is to be used as a basis for transforms then
%% all the error cases must be handled otherwise this module just crashes!

-export([parse_transform/2]).

-record(context, {
    module :: atom(),
    enum = [],
    enums = #{},
    enum_erl1_representations :: #{atom() => enum_er1_representation()},
    enum_erl1_types = #{} :: #{atom() => Type :: term()}
}).

-define(ENUM_COOKIE, 969696).

%% Maps each constructor of an enum to its erl1 representation.
-type enum_er1_representation() :: #{atom() => constructor_erl1_representation()}.

%% @doc An erl1 pattern with a guard that specifies the internal representation
%% of an enum constructor. The arguments of the constructor are referred to using
%% {constructor_argument, Line, integer()} where the integer argument is a
%% 1-based index.
-type constructor_erl1_representation() :: {erl2_parse:af_pattern(), erl2_parse:af_guard()}.

parse_transform(Forms, _Options) ->
    Context = init_context(Forms),
    forms(Forms, Context).

init_context(Forms) ->
    [Module] = [M || {attribute, _, module, M} <- Forms],
    Enums = [E || {attribute, _, enum, E} <- Forms],
    #context{
        module = Module,
        enums = init_enums(Enums, #{}),
        enum_erl1_representations = maps:from_list(lists:flatmap(fun parse_enum_erl1_representation/1, Forms)),
        enum_erl1_types = maps:from_list(lists:flatmap(fun parse_enum_erl1_type/1, Forms))
    }.

init_enums([{N, {type, _, enum, {atom, _, C}, _}, _Vs} | Es], Map) ->
    init_enums(Es, add_enum(C, N, Map));
init_enums([{N, {type, _, union, Cs}, _Vs} | Es], Map) ->
    Map1 = lists:foldl(fun (C, M) -> add_enum(C, N, M) end, Map, constrs(Cs)),
    init_enums(Es, Map1);
init_enums([], Map) ->
    Map.

add_enum(C, E, Map) ->
    case maps:find(C, Map) of
        {ok, Enums} ->
            Map#{C := ordsets:add_element(E, Enums)};
        error ->
            #{C => [E]}
    end.

constrs([{type, _, enum, {atom, _, C}, _} | Cs]) ->
    [C | constrs(Cs)];
constrs([]) ->
    [].

%% @doc Returns the erl1 representation of an enum if the given function definition
%% specifies one. Otherwise, returns the empty list.
%%
%% For now, we use plain, top-level functions for specifying the erl1
%% representation of an enum. The function should be named
%% erl1_type_<enum_name>, and should have one clause per constructor.
%% The head of the clause should be a single erl1 pattern, and the body should
%% be an application of the constructor to variables in the head.
%% A single pattern guard can optionally be specified.
-spec parse_enum_erl1_representation(erl2_parse:af_clause()) -> list({atom(), enum_er1_representation()}).
parse_enum_erl1_representation({function, _Line, FunctionName, 1, Clauses}) ->
    case atom_to_list(FunctionName) of
        "erl1_type_" ++ EnumName ->
            Representation = maps:from_list(lists:map(fun parse_constructor_erl1_representation/1, Clauses)),
            [{list_to_atom(EnumName), Representation}];
        _ ->
            []
    end;
parse_enum_erl1_representation(_) ->
    [].

%% @doc Returns the erl1 type of an enum given a function spec, if the given
%% function spec specifies one. Otherwise, returns the empty list.
parse_enum_erl1_type({attribute, _Line, spec, {{FunctionName, 1}, [FunctionType]}}) ->
    case atom_to_list(FunctionName) of
        "erl1_type_" ++ EnumName ->
            {type, _Line1, 'fun', [{type, _Line2, product, [Erl1Type]}, _]} = FunctionType,
            [{list_to_atom(EnumName), Erl1Type}];
        _ ->
            []
    end;
parse_enum_erl1_type(_) ->
    [].

-spec parse_constructor_erl1_representation(erl2_parse:af_clause()) -> {atom(), constructor_erl1_representation()}.
parse_constructor_erl1_representation({clause, _Line, [Pattern], Guards, Body}) when length(Guards) =< 1 ->
    [{enum, _, _Enum, {atom, _, Constructor}, Arguments}] = Body,

    % All constructor arguments must be variables (that appear in the clause head).
    ArgumentVariables = lists:map(fun({var, _, Name}) -> Name end, Arguments),

    % Maps variable names to their indexes.
    VariableMap = maps:from_list(lists:zip(ArgumentVariables, lists:seq(1, length(ArgumentVariables)))),

    % Replace variable references with constructor argument indexes.
    Rewrite =
        fun
            ({var, LineVar, Name}) -> {constructor_argument, LineVar, maps:get(Name, VariableMap)};
            (N) -> N
        end,

    Guard =
        case Guards of
            [] -> [];
            [G] -> G
        end,
    {Constructor, {erl2_util:rewrite(Rewrite, Pattern), erl2_util:rewrite(Rewrite, Guard)}}.


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
form({attribute, Line, enum, {N, T, Vs}}, Context) ->
    T1 = type(T, Context#context{enum = N}),
    Vs1 = variable_list(Vs, Context),
    {attribute, Line, type, {N, T1, Vs1}};
form({attribute, Line, spec, {{N, A}, FTs}}, Context) ->
    FTs1 = function_type_list(FTs, Context),
    {attribute, Line, spec, {{N, A}, FTs1}};
form({attribute, Line, spec, {{M, N, A}, FTs}}, Context) ->
    FTs1 = function_type_list(FTs, Context),
    {attribute, Line, spec, {{M, N, A}, FTs1}};
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

clause({clause, Line, P0, G0, B0}, Context) ->
    AdditionalTests = additional_guard_tests(P0, G0, Context),
    P1 = patterns(P0, Context),
    G1 = guard(G0, Context),
    B1 = exprs(B0, Context),
    % Splice additional tests into every guard
    G2 =
        case G1 of
            [] ->
                [AdditionalTests];
            _ ->
                [AdditionalTests ++ Guard || Guard <- G1]
        end,
    {clause, Line, P1, G2, B1}.

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
pattern({enum, _, _, _, _} = Enum, Context) ->
    {Pattern, _Guard} = pattern_enum(Enum, Context),
    Pattern;
pattern({map, Line, Ps0}, Context) ->
    Ps1 = pattern_list(Ps0, Context),
    {map, Line, Ps1};
pattern({map_field_exact, Line, K, V}, Context) ->
    Ke = expr(K, Context),
    Ve = pattern(V, Context),
    {map_field_exact, Line, Ke, Ve};
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


%% @doc Enums with custom erl1 mappings can generate guard tests that need to
%% be propagated up and added to the clause head. Given a clause head, this
%% function will recursively collect all tests that need to be added to the
%% clause.
-spec additional_guard_tests([erl2_parse:af_pattern()], erl2_parse:af_guard_sequence(), #context{})
        -> list(erl2_parse:af_guard_test()).
additional_guard_tests(Patterns, Guards, Context) ->
    %% TODO: these two functions can be combined easily
    CollectPatterns =
        fun
            ({enum, _, _, _, _} = Enum) ->
                {_Pattern, Guard} = pattern_enum(Enum, Context),
                Guard;
            (_) -> undefined
        end,
    CollectGuards =
        fun
            ({enum, _, _, _, _} = Enum) ->
                {_Expression, Guard} = gexpr_enum(Enum, Context),
                Guard;
            (_) -> undefined
        end,
    FromPatterns = lists:append(erl2_util:collect(CollectPatterns, Patterns)),
    FromGuards = lists:append(erl2_util:collect(CollectGuards, Guards)),
    FromPatterns ++ FromGuards.

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
gexpr({enum, _, _, _, _} = Enum, Context) ->
    gexpr_enum(Enum, Context);
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
expr({enum, _, _, _, _} = Enum, Context) ->
    expr_enum(Enum, Context);
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
    AdditionalTests = additional_guard_tests([P0], [], Context),
    P1 = pattern(P0, Context),
    case AdditionalTests of
        [] ->
            {match, Line, P1, E1};
        _ ->
            %% TODO: have to use case
            throw("TODO")
    end;
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
type({type, Line, record, [Name | Fs]}, Context) ->
    Fs1 = field_types(Fs, Context),
    %% minor change to not die on qualified records
    {type, Line, record, [Name | Fs1]};
type({remote_type, Line, [M, {atom, Ln, N}, As]}, Context) ->
    As1 = type_list(As, Context),
    %% minor change to not die on dotted remote types
    {remote_type, Line, [M, {atom, Ln, N}, As1]};
type({type, Line, tuple, any}, _Context) ->
    {type, Line, tuple, any};
type({type, Line, tuple, Ts}, Context) ->
    Ts1 = type_list(Ts, Context),
    {type, Line, tuple, Ts1};
type({type, _, enum, _, _, _} = EnumType, Context) ->
    type_enum(EnumType, Context);
type({type, _, enum, _, _} = EnumType, Context) ->
    type_enum(EnumType, Context);
type({type, Line, union, Ts}, Context) ->
    Ts1 = type_list(Ts, Context),
    {type, Line, union, Ts1};
type({var, Line, V}, _Context) ->
    {var, Line, V};
type({user_type, Line, N, As}, Context) ->
    As1 = type_list(As, Context),
    {user_type, Line, N, As1};
type({type, Line, N0, As}, Context) ->
    As1 = type_list(As, Context),
    {type, Line, N0, As1}.

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

%% More localized enum transformations

pattern_enum({enum, _Line, {remote, _L, M0, E0}, A0, Ps0}, Context) ->
    %% remote enum reference Mod.Enum.Constructor{...}
    M1 = pattern(M0, Context),
    E1 = pattern(E0, Context),
    A1 = pattern(A0, Context),
    Ps1 = pattern_list(Ps0, Context),
    compile_enum(M1, E1, A1, Ps1, Context);
pattern_enum({enum, Line, E0, A0, Ps0}, Context) ->
    %% local qualified enum reference Enum.Constructor{...}
    M = {atom, Line, Context#context.module},
    E1 = pattern(E0, Context),
    A1 = pattern(A0, Context),
    Ps1 = pattern_list(Ps0, Context),
    compile_enum(M, E1, A1, Ps1, Context).

gexpr_enum({enum, Line, E0, A0, Es0}, Context) ->
    %% local qualified enum reference Enum.Constructor{...}
    M = {atom, Line, Context#context.module},
    E1 = gexpr(E0, Context),
    A1 = gexpr(A0, Context),
    Es1 = gexpr_list(Es0, Context),
    {Expression, _Guard} = compile_enum(M, E1, A1, Es1, Context),
    Expression.

expr_enum({enum, _Line, {remote, _L, M0, E0}, A0, Es0}, Context) ->
    %% remote enum reference Mod.Enum.Constructor{...}
    M1 = expr(M0, Context),
    E1 = expr(E0, Context),
    A1 = expr(A0, Context),
    Es1 = expr_list(Es0, Context),
    {Expression, _Guard} = compile_enum(M1, E1, A1, Es1, Context),
    Expression;
expr_enum({enum, Line, E0, A0, Es0}, Context) ->
    %% local qualified enum reference Enum.Constructor{...}
    M = {atom, Line, Context#context.module},
    E1 = expr(E0, Context),
    A1 = expr(A0, Context),
    Es1 = expr_list(Es0, Context),
    {Expression, _Guard} = compile_enum(M, E1, A1, Es1, Context),
    Expression.

%% TODO: custom erl1 representation
type_enum({type, _Line, enum, {remote, _, M0, E0}, A0, Ts0}, Context) ->
    %% remote enum reference Mod.Enum.Constructor{...}
    M1 = type(M0, Context),
    E1 = type(E0, Context),
    A1 = type(A0, Context),
    Ts1 = type_list(Ts0, Context),
    enum_erl1_type(M1, E1, A1, Ts1, Context);
type_enum({type, Line, enum, E0, A0, Ts0}, Context) ->
    %% local qualified enum reference Enum.Constructor{...}
    M = {atom, Line, Context#context.module},
    E1 = type(E0, Context),
    A1 = type(A0, Context),
    Ts1 = type_list(Ts0, Context),
    enum_erl1_type(M, E1, A1, Ts1, Context);
type_enum({type, Line, enum, {atom, _, _} = A, Ts}, Context) ->
    A1 = type(A, Context),
    Ts1 = type_list(Ts, Context),
    %% unqualified use can only happen in an enum def, so the
    %% enum name should be given by the context
    E = {atom, Line, Context#context.enum},
    M = {atom, Line, Context#context.module},
    enum_erl1_type(M, E, A1, Ts1, Context).

%% @doc Returns a pattern/expression/guard expression representing the given
%% enum constructor applied to the given arguments. The return type depends on
%% the type of the arguments (e.g., given patterns, this function returns a
%% pattern).
compile_enum(Module, Enum, Constructor, Arguments, Context) ->
    Representation = get_enum_erl1_representation(Module, Enum, Constructor, length(Arguments), Context),
    Rewrite =
        fun
            ({constructor_argument, _, Index}) -> lists:nth(Index, Arguments);
            (Node) -> Node
        end,
    % TODO: handle duplicate arguments. This will replicate expressions if the representation refers
    %   to an argument multiple times. Correct way is to bind the expression to a name and replicate the name.
    erl2_util:rewrite(Rewrite, Representation).

%% @doc Returns the erl1 type for the given enum constructor.
enum_erl1_type(Module, Enum, Constructor, TypeArguments, Context) ->
    {atom, _, ModuleName} = Module,
    {atom, _, EnumName} = Enum,
    {atom, Line, _} = Constructor,
    Erl1Type = maps:get(EnumName, Context#context.enum_erl1_types, undefined),
    case Context#context.module =:= ModuleName andalso Erl1Type /= undefined of
        true ->
            % Local enum with custom representation
            %% TODO: this shouldn't be the same for all constructors
            Erl1Type;
        false ->
            % Default representation
            {type, Line, tuple, [{integer, Line, ?ENUM_COOKIE}, Module, Enum, Constructor | TypeArguments]}
    end.


%% @doc Returns the erl1 representation of the given enum constructor.
%% TODO: lookup representations for remote enums also.
-spec get_enum_erl1_representation(
    erl2_parse:af_lit_atom(), erl2_parse:af_lit_atom(), erl2_parse:af_lit_atom(), integer(), #context{})
        -> constructor_erl1_representation().
get_enum_erl1_representation(
    {atom, _, ModuleName} = Module,
    {atom, _, EnumName} = Enum,
    {atom, Line, ConstructorName} = Constructor,
    Arity,
    Context) ->
    EnumRepresentation = maps:get(EnumName, Context#context.enum_erl1_representations, undefined),
    case Context#context.module =:= ModuleName andalso EnumRepresentation /= undefined of
        true ->
            % Local enum with custom representation
            maps:get(ConstructorName, EnumRepresentation);
        false ->
            % Default representation
            Arguments = lists:map(fun(I) -> {constructor_argument, Line, I} end, lists:seq(1, Arity)),
            Pattern = {tuple, Line, [{integer, Line, ?ENUM_COOKIE}, Module, Enum, Constructor | Arguments]},
            Guard = [],
            {Pattern, Guard}
    end.
