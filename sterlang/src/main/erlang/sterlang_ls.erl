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

-module(sterlang_ls).

-export([type_check/1, type_check_debug/1]).

type_check(AbsFile) ->
    type_check(AbsFile, false).

type_check_debug(AbsFile) ->
    type_check(AbsFile, true),
    ok.

type_check(AbsFile, Debug) ->
    {ok, [[HomeDir]]} = init:get_argument(home),
    SterlangLsDir = filename:join(HomeDir, ".sterlang_ls"),
    SterlangBin = filename:join(SterlangLsDir, "sterlang"),
    ErlFileName = filename:basename(AbsFile),
    EtfFileName = ErlFileName ++ ".etf",
    SterlangResponseFileName = ErlFileName ++ ".sterlang",
    AbsEftFileName = filename:join(SterlangLsDir, EtfFileName),
    AbsSterlangResponseFileName = filename:join(SterlangLsDir, SterlangResponseFileName),
    ok = erl2etf:main(["-erl", AbsFile, "-etf", AbsEftFileName]),
    CheckCmd =
        lists:append([
            SterlangBin,
            " ",
            AbsEftFileName,
            " ",
            AbsSterlangResponseFileName,
            " ",
            "--lsp"
        ]),
    case Debug of
        true ->
            io:format("Command: ~p\n", [CheckCmd]);
        false ->
            ok
    end,
    {0, _} = eunit_lib:command(CheckCmd),
    {ok, Content} = file:read_file(AbsSterlangResponseFileName),
    Response = erlang:binary_to_term(Content),
    case Debug of
        true ->
            io:format("Sterlang Response:~n~p~n", [Response]);
        false ->
            ok
    end,
    Response.
