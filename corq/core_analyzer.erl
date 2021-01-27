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
    get_core_forms/1
]).

-define(DEBUG, ).

-ifdef(DEBUG).
-define(DEBUG_CORE(Core), log_core(Core)).
log_core(Core) ->
    io:format("~s~n", [lists:flatten(core_pp:format(Core))]).
-else.
-define(?DEBUG_CORE(Core), ok).
-endif.


-spec get_core_forms(file:filename()) -> {ok, cerl:cerl()} | error.
get_core_forms(BeamFile) ->
    io:format("Got request for ~p~n", [BeamFile]),
    case beam_lib:chunks(BeamFile, [debug_info]) of
        {ok, {Module, [{debug_info, {debug_info_v1, Backend, Metadata}}]}} ->
            case Backend:debug_info(core_v1, Module, Metadata, []) of
                {ok, Core} ->
                    ?DEBUG_CORE(Core),
                    {ok, Core}
            end
    end.

