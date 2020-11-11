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

-behaviour(gen_server).

-export([start/2, stop/1, equal/2, closer/3, inc/2, dec/2]).

-export([handle_equal/2, handle_closer/3, handle_inc/2, handle_dec/2]).

-export([handle_call/3, handle_cast/2, init/1]).

-callback handle_init(input()) -> state().
-callback handle_equal(integer(), state()) -> {boolean(), state()}.
-callback handle_closer(integer(), integer(), state()) -> {integer(), state()}.
-callback handle_inc(integer(), state()) -> state().
-callback handle_dec(integer(), state()) -> state().

-spec start(gen_server:name(), input()) -> {ok, pid()} | ignore | {error, term()}.
start(Name, InitArgs) ->
    gen_server:start_link({local, Name}, ?MODULE, InitArgs, []).

-spec stop(gen_server:name()) -> ok.
stop(Name) ->
    gen_server:stop(Name).

-spec equal(gen_server:name(), integer()) -> boolean().
equal(Name, Arg) ->
    gen_server:call(Name, {handle_equal, Arg}).

-spec closer(gen_server:name(), integer(), integer()) -> integer().
closer(Name, Arg1, Arg2) ->
    gen_server:call(Name, {handle_closer, Arg1, Arg2}).

-spec inc(gen_server:name(), integer()) -> ok.
inc(Name, Arg) ->
    gen_server:cast(Name, {handle_inc, Arg}).

-spec dec(gen_server:name(), integer()) -> ok.
dec(Name, Arg) ->
    gen_server:cast(Name, {handle_dec, Arg}).

-spec init(input()) -> {ok, state()}.
init(InitState) ->
    State = handle_init(InitState),
    {ok, State}.

-spec handle_cast
    ({handle_inc, integer()}, state()) -> {noreply, state()};
    ({handle_dec, integer()}, state()) -> {noreply, state()}.
handle_cast({handle_inc, Arg0}, State) ->
    {noreply, (?MODULE:handle_inc)(Arg0, State)};
handle_cast({handle_dec, Arg0}, State) ->
    {noreply, (?MODULE:handle_dec)(Arg0, State)}.

-spec handle_call
    ({handle_equal, integer()}, pid(), state()) -> {reply, boolean(), state()};
    ({handle_closer, integer(), integer()}, pid(), state()) -> {reply, integer(), state()}.
handle_call({handle_equal, Arg0}, _From, State) ->
    {Result, NewState} = (?MODULE:handle_equal)(Arg0, State),
    {reply, Result, NewState};
handle_call({handle_closer, Arg0, Arg1}, _From, State) ->
    {Result, NewState} = (?MODULE:handle_closer)(Arg0, Arg1, State),
    {reply, Result, NewState}.
