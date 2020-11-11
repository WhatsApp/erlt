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

-module(count_server_SUITE).

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
    count_test/1,
    singleton_test/1,
    confusing_routing/1
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
        {count_group, [parallel], [
            count_test,
            singleton_test,
            confusing_routing
        ]}
    ].

group(_) -> [].

all() ->
    [{group, count_group}].

count_test(_Config) ->
    count_server:start(myname, 10),
    ?assertEqual(true, count_server:equal(myname, 10)),
    count_server:inc(myname, 5),
    ?assertEqual(false, count_server:equal(myname, 10)),
    ?assertEqual(true, count_server:equal(myname, 15)),
    count_server:dec(myname, 3),
    ?assertEqual(false, count_server:equal(myname, 15)),
    ?assertEqual(true, count_server:equal(myname, 12)),
    ?assertEqual(11, count_server:closer(myname, 11, 17)),
    count_server:stop(myname).

singleton_test(_Config) ->
    count_singleton:start(),
    ?assertEqual(true, count_singleton:equal(0)),
    count_singleton:inc(5),
    ?assertEqual(false, count_singleton:equal(0)),
    ?assertEqual(true, count_singleton:equal(5)),
    count_singleton:dec(3),
    ?assertEqual(false, count_singleton:equal(5)),
    ?assertEqual(true, count_singleton:equal(2)),
    count_singleton:stop().

confusing_routing(_Config) ->
    count_server:start(ints, 10),
    %% We can even have mocks, by importing the same header.
    string_server:start(strings, {}),
    true = string_server:equal(strings, "0"),
    %% TODO: This shouldn't work, but it is perfectly legal
    true = count_server:equal(strings, "0"),
    count_server:stop(ints),
    string_server:stop(strings).
