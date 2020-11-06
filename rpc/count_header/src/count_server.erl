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

-include("count_boilerplate.hrl").

-type input() :: integer().
-type state() :: integer().

-spec handle_init(input()) -> state().
handle_init(I) -> I.

-spec handle_equal(integer(), state()) -> {boolean(), state()}.
handle_equal(I, I) -> {true, I};
handle_equal(_, I) -> {false, I}.

-spec handle_inc(integer(), state()) -> state().
handle_inc(N, I) -> I + N.

-spec handle_dec(integer(), state()) -> state().
handle_dec(N, I) -> I - N.

-spec handle_closer(integer(), integer(), state()) -> {integer(), state()}.
handle_closer(I, J, S) ->
    case abs(I - S) < abs(J - S) of
        true -> {I, S};
        false -> {J, S}
    end.
