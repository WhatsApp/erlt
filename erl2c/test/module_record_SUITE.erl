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

%%%-------------------------------------------------------------------
%%% @author Michał Muskała <micmus@whatsapp.com>
%%% @doc
%%%     Tests module records and module aliasing
%%% @end
%%% -------------------------------------------------------------------

-module(module_record_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Test server callbacks
-export([
    suite/0,
    all/0,
    groups/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2, end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
]).

%% Test cases
-export([
    smoke_test/1
]).

suite() ->
    [{timetrap, {seconds, 15}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [
        {smoke_tests, [patallel, shuffle, auto_per_tc], [smoke_test]}
    ].

all() ->
    [{group, smoke_tests}].

%%--------------------------------------------------------------------
%% TEST CASES

smoke_test(Config) ->
    SmokeTestModules = [
        ma_mod01, ma_mod02, ma_mod03, ma_mod04, ma_mod05, ma_mod06,
        ma_mod07, ma_mod08, ma_mod09, ma_mod10, ma_mod11, ma_mod12,
        ma_mod13
    ],
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    [erl2_compile(Module, DataDir, PrivDir) || Module <- SmokeTestModules].

erl2_compile(Module, Source, Out) ->
    Command = io_lib:format("erl2c -pa ~s -o ~s ~s/~s.erl", [Out, Out, Source, Module]),
    ?assertEqual({0, ""}, eunit_lib:command(Command)).
