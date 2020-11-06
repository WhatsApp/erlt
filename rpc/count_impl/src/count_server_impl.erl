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

-module(count_server_impl).

-behaviour(count_server).

-export([
    init/1,
    equal/2,
    closer/3,
    inc/2,
    dec/2
]).

-export_type([state/0, input/0]).

-type input() :: integer().
-type state() :: integer().

-spec init(count_server:input()) -> count_server:state().
init(I) -> I.

-spec equal(integer(), count_server:state()) -> {boolean(), count_server:state()}.
equal(I, I) -> {true, I};
equal(_, I) -> {false, I}.

-spec inc(integer(), count_server:state()) -> count_server:state().
inc(N, I) -> I + N.

-spec dec(integer(), count_server:state()) -> count_server:state().
dec(N, I) -> I - N.

-spec closer(integer(), integer(), state()) -> {integer(), state()}.
closer(I, J, S) ->
    case abs(I - S) < abs(J - S) of
        true -> {I, S};
        false -> {J, S}
    end.
