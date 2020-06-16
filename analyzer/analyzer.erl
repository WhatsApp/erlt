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
    catches/1,
    error_handling/1,
    exports/1,
    tries/1,
    used_funs/1
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

-spec error_handling(file:filename()) ->
    {{'catches', integer()}, {'tries', integer()}}.
error_handling(BeamFile) ->
    {ok, Forms} = get_abstract_forms(BeamFile),
    Catches = erlang:length(catches_aux(Forms)),
    Tries = erlang:length(tries_aux(Forms)),
    {{'catches', Catches}, {'tries', Tries}}.

-spec catches(file:filename()) -> list(erl_anno:anno()).
catches(BeamFile) ->
    {ok, Forms} = get_abstract_forms(BeamFile),
    catches_aux(Forms).

-spec catches_aux(list(erl_parse:abstract_form())) -> list(erl_anno:anno()).
catches_aux(Forms) ->
    collect(Forms, fun pred/1, fun catch_anno/1).

-spec tries(file:filename()) -> list(erl_anno:anno()).
tries(BeamFile) ->
    {ok, Forms} = get_abstract_forms(BeamFile),
    tries_aux(Forms).

-spec tries_aux(list(erl_parse:abstract_form())) -> list(erl_anno:anno()).
tries_aux(Forms) ->
    collect(Forms, fun pred/1, fun try_anno/1).

collect(Forms, Pred, Collect) ->
    do_collect(Forms, Pred, Collect, []).

do_collect([], _Pred, _Collect, Acc) ->
    Acc;
do_collect([H|T], Pred, Collect, Acc) ->
    Acc1 = do_collect(T, Pred, Collect, Acc),
    do_collect(H, Pred, Collect, Acc1);
do_collect(X, Pred, Collect, Acc) when is_tuple(X) ->
    case Pred(X) of
        true ->
            Acc1 = do_collect(tuple_to_list(X), Pred, Collect, Acc),
            case Collect(X) of
                false -> Acc1;
                Delta -> [Delta|Acc1]
            end;
        false ->
            Acc
    end;
do_collect(_X, _Pred, _Collect, Acc) ->
    Acc.

remote_fun({attribute,_,import,{Mod,Funs}}) ->
    [{Mod, F, A} || {F, A} <- Funs];
remote_fun({call,_,{remote,_Line,{atom,_,M},{atom,_,F}},As}) ->
    [{M,F, length(As)}];
%% fn mod:f/n
remote_fun({'fun',_,{function,{atom,_,Mod},{atom,_,F},{integer,_,A}}}) when is_atom(Mod),is_atom(F),is_integer(A) ->
    [{Mod, F, A}];
remote_fun(_) ->
    false.

export({attribute,_,export,Es}) ->
    Es;
export(_) ->
    false.

catch_anno({'catch', Anno, _}) -> Anno;
catch_anno(_) -> false.

try_anno({'try', Anno, _, _, _, _}) -> Anno;
try_anno(_) -> false.

pred(_) ->
    true.

-spec get_module(list(erl_parse:abstract_form())) -> module().
get_module(Forms) ->
    erlang:hd([M || {attribute,_,module,M} <- Forms]).

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
