%% ------------------------------------------------------------------------
%% @copyright (c) WhatsApp Inc. and its affiliates. All rights reserved.
%% @doc Tests for dotted names

-module(dots_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Test server callbacks
-export([ suite/0,
          all/0,
          groups/0,
          init_per_suite/1, end_per_suite/1,
          init_per_group/2, end_per_group/2,
          init_per_testcase/2, end_per_testcase/2
        ]).

%% Test cases
-export([ smoke_test/1
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
     {smoke_tests, [parallel, shuffle, auto_per_tc],
      [smoke_test]}
    ].

all() ->
    [{group, smoke_tests}].


%% ------------------------------------------------------------------------
%% Test Cases

smoke_test(Config) ->
    SmokeTestModules = [ dots_mod01
                       ],
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    [erl2_compile(Module, DataDir, PrivDir)
     || Module <- SmokeTestModules].

erl2_compile(Module, Source, Out) ->
    Command = io_lib:format("erl2c -pa ~s -o ~s ~s/~s.erl",
                            [Out, Out, Source, Module]),
    ?assertEqual({0, ""}, eunit_lib:command(Command)).
