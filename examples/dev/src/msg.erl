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

-lang([erl2]).
-module(msg).

-export([mk_reply/1, mk_ping/0]).

-message ping :: (from :: pid()).
-message pong :: (from :: pid(), in_reply_to :: wrapped_message()).

-enum wrapped_message() :: wrapped_message{message()}.

-spec mk_ping() ->
    message().
mk_ping() ->
    #ping{from = self()}.

-spec mk_reply(message()) ->
    message().
mk_reply(Ping) ->
    #pong{from = self(), in_reply_to = wrapped_message.wrapped_message{Ping}}.
