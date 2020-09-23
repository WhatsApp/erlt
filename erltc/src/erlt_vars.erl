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

-module(erlt_vars).

-record(var_state, {set :: gb_sets:set(atom()), counter = 0 :: integer()}).

-define(VARNAME(Number), list_to_atom("_Var_" ++ integer_to_list(Number) ++ "_Gen")).

-export([initialize_vars/1, new_variable/1]).

-export_type([var_state/0]).

-opaque var_state() :: #var_state{}.

-spec initialize_vars(Forms :: erl_parse:abstract_forms()) -> #var_state{}.
initialize_vars(Forms) ->
    {_NewForms, VarSet} = erlt_ast:prewalk(Forms, gb_sets:empty(), fun gather_vars/3),
    #var_state{set = VarSet}.

gather_vars({var, _, Name} = V, Set, _) ->
    %% restore the import as the simplest solution for now
    %% (our linter has prevented all other variable imports)
    {V, gb_sets:add_element(Name, Set)};
gather_vars(Other, Set, _Ctx) ->
    {Other, Set}.

-spec new_variable(State :: #var_state{}) -> {Name :: atom(), NextState :: #var_state{}}.
new_variable(State) ->
    Counter = State#var_state.counter,
    NextState = State#var_state{counter = Counter + 1},
    Var = ?VARNAME(Counter),
    case gb_sets:is_element(Var, State#var_state.set) of
        true ->
            new_variable(NextState);
        false ->
            {Var, NextState}
    end.
