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

%% kitty_SUITE tests kitty, an example from
%% https://learnyousomeerlang.com/clients-and-servers#callback-to-the-future
-module(kitty_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Test server callbacks
-export([
    suite/0,
    all/0,
    groups/0,
    group/1,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    kitty_test/1
]).

suite() ->
    [{timetrap, {seconds, 10}}].

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
        {kitty_group, [parallel], [
            kitty_test
        ]}
    ].

group(_) -> [].

all() ->
    [{group, kitty_group}].

kitty_test(_Config) ->
    {ok, Pid} = kitty:start_link(),
    CatStevens = kitty:order_cat(Pid, "Cat Stevens"),
    ok = kitty:return_cat(Pid, CatStevens),
    CatStevens2 = kitty:order_cat(Pid, "Kitten Mittens"),
    ?assertEqual(CatStevens, CatStevens2),
    Mittens = kitty:order_cat(Pid, "Kitten Mittens"),
    ?assertNotEqual(CatStevens, Mittens),
    ok = kitty:return_cat(Pid, CatStevens2),
    ok = kitty:close_shop(Pid).
