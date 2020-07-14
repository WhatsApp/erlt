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
-module(analyzer).

-export([
    behaviours/1,
    bif_clashes/1,
    catches/1,
    dynamic_calls/1,
    error_handling/1,
    error_handling_verbose/1,
    exports/1,
    forms/1,
    functions/1,
    receives/1,
    functions_with_nonlinear_clauses/1,
    tries/1,
    used_funs/1,
    used_primitives/2
]).

%% Returns a list of functions used in a given module.
-spec used_funs(file:filename()) -> list(mfa()).
used_funs(BeamFile) ->
    {ok, Forms} = get_abstract_forms(BeamFile),
    used_funs_aux(Forms).

-spec used_funs_aux(list(erl_parse:abstract_form())) -> list(mfa()).
used_funs_aux(Forms) ->
    Module = get_module(Forms),
    Remotes = collect(Forms, fun pred/1, fun remote_fun/1),
    Remotes1 = lists:append(Remotes),
    FilteredRemotes = [{M, F, A} || {M, F, A} <- Remotes1, M =/= Module],
    lists:usort(FilteredRemotes).

-spec forms(file:filename()) -> [erl_parse:abstract_form()].
forms(BeamFile) ->
    {ok, Forms} = get_abstract_forms(BeamFile),
    Forms.

-spec used_primitives(file:filename(), atom()) -> list(string()).
used_primitives(BeamFile, PrimCategory) ->
    {ok, Forms} = get_abstract_forms(BeamFile),
    Remotes = collect(Forms, fun pred/1, fun remote_fun/1),
    Remotes1 = lists:append(Remotes),
    Set = prim_set(PrimCategory),
    Primitives =
        [
            atom_to_list(F) ++ "/" ++ integer_to_list(A)
            || {erlang, F, A} <- Remotes1, lists:member({F, A}, Set)
        ],
    Primitives.

prim_set(concurrency) ->
    [
        {cancel_timer, 1},
        {cancel_timer, 2},
        {check_process_code, 2},
        {check_process_code, 3},
        {demonitor, 1},
        {demonitor, 2},
        {disconnect_node, 1},
        {halt, 0},
        {halt, 1},
        {halt, 2},
        {is_process_alive, 1},
        {link, 1},
        {list_to_pid, 1},
        {monitor, 2},
        {node, 0},
        {node, 1},
        {nodes, 0},
        {nodes, 1},
        {process_flag, 2},
        {process_flag, 3},
        {process_info, 2},
        {process_info, 3},
        {processes, 0},
        {register, 2},
        {self, 0},
        {send, 2},
        {send, 3},
        {send_after, 3},
        {send_after, 4},
        {spawn, 1},
        {spawn, 2},
        {spawn, 3},
        {spawn, 4},
        {spawn_link, 1},
        {spawn_link, 2},
        {spawn_link, 3},
        {spawn_link, 4},
        {spawn_monitor, 1},
        {spawn_monitor, 2},
        {spawn_monitor, 3},
        {spawn_monitor, 4},
        {start_timer, 3},
        {start_timer, 4},
        {suspend_process, 2}
    ];
prim_set(dynamic) ->
    [
        {apply, 2},
        {apply, 3},
        {is_function, 1},
        {is_function, 2}
    ].

-spec dynamic_calls(file:filename()) -> list(string()).
dynamic_calls(BeamFile) ->
    {ok, Forms} = get_abstract_forms(BeamFile),
    DCalls = collect(Forms, fun pred/1, fun dynamic_call/1),
    DCalls1 = lists:append(DCalls),
    DCalls12 = lists:map(fun dynamic_call_to_str/1, DCalls1),
    DCalls12.

%% Returns a list of functions exported from a given module.
-spec exports(file:filename()) -> list({atom, arity()}).
exports(BeamFile) ->
    {ok, Forms} = get_abstract_forms(BeamFile),
    exports_aux(Forms).

-spec exports_aux(list(erl_parse:abstract_form())) -> list({atom, arity()}).
exports_aux(Forms) ->
    Exports = collect(Forms, fun pred/1, fun export/1),
    Exports1 = lists:append(Exports),
    lists:usort(Exports1).

-spec behaviours(file:filename()) -> list(module()).
behaviours(BeamFile) ->
    {ok, Forms} = get_abstract_forms(BeamFile),
    Behaviors = [Behavior || {'attribute', _, 'behavior', Behavior} <- Forms],
    Behaviours = [Behaviour || {'attribute', _, 'behaviour', Behaviour} <- Forms],
    lists:usort(Behaviors ++ Behaviours).

%% the count of all functions in a module
-spec functions(file:filename()) -> integer().
functions(BeamFile) ->
    {ok, Forms} = get_abstract_forms(BeamFile),
    Functions = [{Name, Arity} || {function, _Line, Name, Arity, _Clauses} <- Forms],
    Count = erlang:length(Functions),
    Count.

-type try_info() :: {NumberOfCatches :: integer()}.

-spec error_handling(file:filename()) ->
    {{'catches', integer()}, {'tries', list(try_info())}}.
error_handling(BeamFile) ->
    {ok, Forms} = get_abstract_forms(BeamFile),
    Catches = erlang:length(catches_aux(Forms)),
    Tries = tries_aux(Forms),
    {{'catches', Catches}, {'tries', Tries}}.

error_handling_verbose(BeamFile) ->
    {ok, Forms} = get_abstract_forms(BeamFile),
    Catches = erlang:length(catches_aux(Forms)),
    Tries = tries_aux_verbose(Forms),
    {{'catches', Catches}, {'tries', Tries}}.

-spec bif_clashes(file:filename()) -> list(mfa()).
bif_clashes(BeamFile) ->
    {ok, Forms} = get_abstract_forms(BeamFile),
    M = get_module(Forms),
    Clashes = [{M, N, A} || {function, _Line, N, A, _Cs} <- Forms, erl_internal:bif(N, A)],
    lists:usort(Clashes).

-spec catches(file:filename()) -> list(erl_anno:anno()).
catches(BeamFile) ->
    {ok, Forms} = get_abstract_forms(BeamFile),
    catches_aux(Forms).

-spec catches_aux(list(erl_parse:abstract_form())) -> list(erl_anno:anno()).
catches_aux(Forms) ->
    collect(Forms, fun pred/1, fun catch_anno/1).

-spec receives(file:filename()) -> integer().
receives(BeamFile) ->
    {ok, Forms} = get_abstract_forms(BeamFile),
    Receives = collect(Forms, fun pred/1, fun receive_anno/1),
    erlang:length(Receives).

-spec functions_with_nonlinear_clauses(file:filename()) -> list(mfa()).
functions_with_nonlinear_clauses(BeamFile) ->
    {ok, Forms} = get_abstract_forms(BeamFile),
    HasNonlinear = fun(Clauses) -> not lists:all(fun is_linear_clause/1, Clauses) end,
    HasCatchAll = fun(Clauses) -> lists:any(fun is_catch_all_clause/1, Clauses) end,
    Nonlinear = [{N, A, HasCatchAll(Clauses)} || {function, _Line, N, A, Clauses} <- Forms, HasNonlinear(Clauses)],
    lists:usort(Nonlinear).

is_linear_clause({clause, _LINE, Patterns, _Guards, _Body}) ->
    CollectVariables =
        fun
            ({var, _Line, '_'})  -> false;
            ({var, _Line, Name}) -> Name;
            (_)                  -> false
        end,
    Variables = collect(Patterns, fun pred/1, CollectVariables),
    not has_duplicates(Variables).

%% @doc Returns true if the clause matches all values.
is_catch_all_clause({clause, _LINE, Patterns, Guards, _Body} = Clause) ->
    (Guards == []) and is_linear_clause(Clause) and lists:all(fun is_catch_all_pattern/1, Patterns).

is_catch_all_pattern({var, _LINE, _Name}) ->
    true;
is_catch_all_pattern({tuple, _LINE, Elements}) ->
    lists:all(fun is_catch_all_pattern/1, Elements);
is_catch_all_pattern(_) ->
    false.

-spec has_duplicates(list()) -> boolean().
has_duplicates(L) ->
    erlang:length(L) /= erlang:length(lists:usort(L)).

-spec tries(file:filename()) -> list(erl_anno:anno()).
tries(BeamFile) ->
    {ok, Forms} = get_abstract_forms(BeamFile),
    tries_aux(Forms).

-spec tries_aux(list(erl_parse:abstract_form())) -> list(try_info()).
tries_aux(Forms) ->
    collect(Forms, fun pred/1, fun try_info/1).

tries_aux_verbose(Forms) ->
    collect(Forms, fun pred/1, fun try_info_verbose/1).

collect(Forms, Pred, Collect) ->
    do_collect(Forms, Pred, Collect, []).

do_collect([], _Pred, _Collect, Acc) ->
    Acc;
do_collect([H | T], Pred, Collect, Acc) ->
    Acc1 = do_collect(T, Pred, Collect, Acc),
    do_collect(H, Pred, Collect, Acc1);
do_collect(X, Pred, Collect, Acc) when is_tuple(X) ->
    case Pred(X) of
        true ->
            Acc1 = do_collect(tuple_to_list(X), Pred, Collect, Acc),
            case Collect(X) of
                false -> Acc1;
                Delta -> [Delta | Acc1]
            end;
        false ->
            Acc
    end;
do_collect(_X, _Pred, _Collect, Acc) ->
    Acc.

dynamic_call({call, _, {remote, _Line, {var, _, M}, {atom, _, F}}, As}) ->
    [{M, F, length(As)}];
dynamic_call({call, _, {remote, _Line, {atom, _, M}, {var, _, F}}, As}) ->
    [{M, F, length(As)}];
dynamic_call({call, _, {remote, _Line, {var, _, M}, {var, _, F}}, As}) ->
    [{M, F, length(As)}];
dynamic_call({call, _, {var, _, F}, As}) ->
    [{F, length(As)}];
dynamic_call(_) ->
    false.

dynamic_call_to_str({M, F, A}) ->
    atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A);
dynamic_call_to_str({F, A}) ->
    atom_to_list(F) ++ "/" ++ integer_to_list(A).

remote_fun({attribute, _, import, {Mod, Funs}}) ->
    [{Mod, F, A} || {F, A} <- Funs];
remote_fun({call, _, {remote, _Line, {atom, _, M}, {atom, _, F}}, As}) ->
    [{M, F, length(As)}];
% we don't complicate the analysis and assume that there are no collisions
% between BIFs and functions defined in a given module
remote_fun({call, _, {atom, _, F}, As}) ->
    A = length(As),
    case erl_internal:bif(F, A) of
        true -> [{erlang, F, A}];
        false -> false
    end;
%% fn mod:f/n
remote_fun({'fun', _, {function, {atom, _, Mod}, {atom, _, F}, {integer, _, A}}}) when
    is_atom(Mod), is_atom(F), is_integer(A)
->
    [{Mod, F, A}];
remote_fun(_) ->
    false.

export({attribute, _, export, Es}) ->
    Es;
export(_) ->
    false.

receive_anno({'receive', Anno, _Cs0}) ->
    Anno;
receive_anno({'receive', Anno, _Cs0, _To0, _ToEs0}) ->
    Anno;
receive_anno(_) ->
    false.

catch_anno({'catch', Anno, _}) -> Anno;
catch_anno(_) -> false.

try_info({'try', _, _, _, CatchSeq, _}) -> {erlang:length(CatchSeq)};
try_info(_) -> false.

try_info_verbose({'try', Line, _, _, CatchSeq, _}) ->
    {Line, erlang:length(CatchSeq), lists:map(fun clause_head/1, CatchSeq)};
try_info_verbose(_) ->
    false.

clause_head({clause, _Line, Pat, Guard, _Body}) ->
    {Pat, Guard}.

pred(_) ->
    true.

-spec get_module(list(erl_parse:abstract_form())) -> module().
get_module(Forms) ->
    erlang:hd([M || {attribute, _, module, M} <- Forms]).

-spec get_abstract_forms(file:filename()) -> {ok, [erl_parse:abstract_form()]} | {error}.
get_abstract_forms(BeamFile) ->
    case beam_lib:chunks(BeamFile, [abstract_code]) of
        {ok, {_Module, [{abstract_code, {_, Forms}}]}} ->
            {ok, Forms};
        _ ->
            io:format("loading ~p~n", [BeamFile]),
            io:format("error~n", []),
            {error}
    end.
