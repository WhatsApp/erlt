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

-module(erlt_modifiers).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    erlt_ast:prewalk(Forms, fun rewrite_erlt/2).

rewrite_erlt({attribute, Anno, unchecked_opaque, {N, Def, Args}}, _) ->
    {attribute, Anno, type, {N, Def, rewrite_args(Args)}};
rewrite_erlt({unchecked_function, Anno, N, A, Cs}, _) ->
    {function, Anno, N, A, Cs};
rewrite_erlt({type, Anno, exception, []}, type) ->
    {type, Anno, any, []};
rewrite_erlt({type, Anno, message, []}, type) ->
    {type, Anno, any, []};
rewrite_erlt(X, _) ->
    X.

rewrite_args([{var, Anno, Name} | Rest]) ->
    [{var, Anno, list_to_atom("_" ++ atom_to_list(Name))} | rewrite_args(Rest)];
rewrite_args([]) ->
    [].
