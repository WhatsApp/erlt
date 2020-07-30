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
-module(exn).

-export([]).

-exception(error1, {}).
-exception(error2, {name :: string()}).

-enum my_error() :: my_error{exception()}.

mk_error1() ->
    #error1{}.

-spec mk_error2() -> exception().
mk_error2() ->
    #error2{name = "bad error"}.

