%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
%%
%% %CopyrightEnd%
%% Purpose: Run the ErlT compiler.

-module(erlt_util).

%% copy of parts of eunit_lib.
%% the only change - adjusting for handling binaries as "THEY ARE"
%% from stdio
-export([command/1]).

command(Cmd) ->
    command(Cmd, "").

command(Cmd, Dir) ->
    command(Cmd, Dir, []).

command(Cmd, Dir, Env) ->
    CD =
        if
            Dir =:= "" -> [];
            true -> [{cd, Dir}]
        end,
    SetEnv =
        if
            Env =:= [] -> [];
            true -> [{env, Env}]
        end,
    Opt = CD ++ SetEnv ++ [stream, exit_status, use_stdio, in, eof],
    P = open_port({spawn, Cmd}, Opt),
    get_data(P, []).

% in contrast to eunit_lib, it gets data "AS IS"
% without normalising "\r\n" stuff
get_data(P, D) ->
    receive
        {P, {data, D1}} ->
            get_data(P, [D1 | D]);
        {P, eof} ->
            port_close(P),
            receive
                {P, {exit_status, N}} ->
                    {N, lists:flatten(lists:reverse(D))}
            end
    end.
