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
-module(rpctc_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("../src/service.hrl").

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
    print_type/1,
    parse_and_print_kitty/1,
    parse_and_print_count/1,
    gen_test/1
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
        {example_group, [parallel], [
            print_type,
            parse_and_print_kitty,
            parse_and_print_count,
            gen_test
        ]}
    ].

group(_) -> [].

all() ->
    [{group, example_group}].

print_type(_Config) ->
    StringToInt = rpctc:fun_str(
        {call, "string_to_int", ["string()"], ["integer()"]}
    ),
    ?assertEqual("string_to_int(string()) -> integer()", StringToInt),
    Ok = rpctc:fun_str(
        {call, "to_ok", [], "ok"}
    ),
    ?assertEqual("to_ok() -> ok", Ok).

parse_and_print_kitty(Config) ->
    DataDir = ?config(data_dir, Config),
    ErlFile = filename:join(DataDir, "kitty_rpct.erlt"),
    {ok, Service} = rpctc:parse_service(ErlFile),
    [
        "order_cat(string()) -> string()",
        "close_shop() -> ok",
        "return_cat(string()) -> no_return()"
    ] = lists:map(fun rpctc:fun_str/1, Service#service.calls ++ Service#service.casts).

parse_and_print_count(Config) ->
    DataDir = ?config(data_dir, Config),
    ErlFile = filename:join(DataDir, "count_rpct.erlt"),
    {ok, Service} = rpctc:parse_service(ErlFile),
    ?assertEqual(
        [
            "equal(integer()) -> boolean()",
            "inc(integer()) -> no_return()",
            "dec(integer()) -> no_return()"
        ],
        lists:map(fun rpctc:fun_str/1, Service#service.calls ++ Service#service.casts)
    ).

gen_test(_Config) ->
    Service = #service{
        calls = [#call{name = "equal", params = ["integer()"], return = "boolean()"}],
        casts = [#cast{name = "inc", params = ["integer()"]}]
    },
    io:format(user, "~n~s~n", [header:gen(Service)]).
