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

-module(count_server).

-behavior(gen_server).

-export([handle_call/3, handle_cast/2, init/1]).

-export([start/3, stop/1, equal/2, closer/3, inc/2, dec/2]).

-callback init(count_server_impl:input()) -> count_server_impl:state().
-callback equal(integer(), count_server_impl:state()) -> {boolean(), count_server_impl:state()}.
-callback inc(integer(), count_server_impl:state()) -> count_server_impl:state().
-callback dec(integer(), count_server_impl:state()) -> count_server_impl:state().

-spec start(gen_server:name(), count_server_impl:input(), module()) -> {ok, pid()}.
start(Name, InitArgs, ImplModule) ->
    gen_server:start_link({local, Name}, ?MODULE, {ImplModule, InitArgs}, []).

-spec stop(gen_server:name()) -> ok.
stop(Name) ->
    gen_server:stop(Name).

-spec equal(gen_server:name(), integer()) -> boolean().
equal(Name, Arg) ->
    gen_server:call(Name, {equal, Arg}).

-spec closer(gen_server:name(), integer(), integer()) -> integer().
closer(Name, Arg0, Arg1) ->
    gen_server:call(Name, {closer, Arg0, Arg1}).

-spec inc(gen_server:name(), integer()) -> ok.
inc(Name, Arg) ->
    gen_server:cast(Name, {inc, Arg}).

-spec dec(gen_server:name(), integer()) -> ok.
dec(Name, Arg) ->
    gen_server:cast(Name, {dec, Arg}).

-spec init({module(), count_server_impl:input()}) -> {ok, {module(), count_server_impl:state()}}.
init({ImplModule, InitState}) ->
    State = ImplModule:init(InitState),
    {ok, {ImplModule, State}}.

-spec handle_cast({atom(), term()}, {module(), count_server_impl:state()}) ->
    {noreply, {module(), count_server_impl:state()}}.
handle_cast({inc, Arg0}, {ImplModule, State}) ->
    {noreply, {ImplModule, (ImplModule:inc)(Arg0, State)}};
handle_cast({dec, Arg0}, {ImplModule, State}) ->
    {noreply, {ImplModule, (ImplModule:dec)(Arg0, State)}}.

-spec handle_call({atom(), term()}, _, {module(), count_server_impl:state()}) ->
    {reply, term(), {module(), count_server_impl:state()}}.
handle_call({equal, Arg0}, _From, {ImplModule, State}) ->
    {Result, NewState} = (ImplModule:equal)(Arg0, State),
    {reply, Result, {ImplModule, NewState}};
handle_call({closer, Arg0, Arg1}, _From, {ImplModule, State}) ->
    {Result, NewState} = (ImplModule:closer)(Arg0, Arg1, State),
    {reply, Result, {ImplModule, NewState}}.
