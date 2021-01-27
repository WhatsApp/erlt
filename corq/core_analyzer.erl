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
-module(core_analyzer).

-export([
    get_core_forms/1,
    get_core_forms_pretty/1
]).

-spec get_core_forms_pretty(file:filename()) -> {ok, cerl:cerl(), string()} | {error, term()}.
get_core_forms_pretty(BeamFile) ->
    case get_core_forms(BeamFile) of
        {ok, Core} ->
                    Pretty = lists:flatten(core_pp:format_all(Core)),
                    {ok, Core, Pretty};
        Other -> Other
    end.

-spec get_core_forms(file:filename()) -> {ok, cerl:cerl()} | {error, term()}.
get_core_forms(BeamFile) ->
    case beam_lib:chunks(BeamFile, [debug_info]) of
        {ok, {Module, [{debug_info, {debug_info_v1, Backend, Metadata}}]}} ->
            case Backend:debug_info(core_v1, Module, Metadata, []) of
                {ok, Core} -> {ok, Core}
            end
    end.
