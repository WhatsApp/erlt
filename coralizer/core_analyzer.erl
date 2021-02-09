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

% -define(DEBUG, true).

-ifdef(DEBUG).
-define(DEBUG_CORE(Core), log_core(Core)).
log_core(Core) ->
    io:format("~s~n", [lists:flatten(core_pp:format(Core))]).
-else.
-define(DEBUG_CORE(Core), ok).
-endif.

% copied from dialyzer_utils.erl 84adefa331
src_compiler_opts() ->
  [
    no_copt,
    to_core,
    binary,
    return_errors,
    no_inline,
    strict_record_tests,
    strict_record_updates,
    dialyzer,
    no_spawn_compiler_process
  ].

% adapted from dialyzer_utils.erl 84adefa331
get_core_forms(File) ->
  case beam_lib:chunks(File, [debug_info]) of
    {ok, {Module, [{debug_info, {debug_info_v1, Backend, Metadata}}]}} ->
      case Backend:debug_info(core_v1, Module, Metadata, src_compiler_opts()) of
        {ok, Core} ->
          Cleaned = dialyzer_clean_core:clean(Core),
          ?DEBUG_CORE(Cleaned),
          {ok, Cleaned};
        {error, _} ->
          {error, "  Could not get Core Erlang code for: " ++ File ++ "\n"}
      end;
    _ ->
      {error, "  Could not get Core Erlang code for: " ++ File ++ "\n" ++
        "  Recompile with +debug_info or analyze starting from source code"}
  end.
