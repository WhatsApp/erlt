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

-module(literals).
-lang(st).

-export([
    my_binary/1,
    my_boolean/1,
    my_char/1,
    my_integer/1,
    my_list/1,
    my_string/1
]).

-spec my_binary(binary()) -> boolean().
my_binary(<<>>) -> true.

-spec my_boolean(boolean()) -> boolean().
my_boolean(true) -> true.

-spec my_char(char()) -> boolean().
my_char($a) -> true.

-spec my_integer(integer()) -> boolean().
my_integer(0) -> true.

-spec my_list(list(_)) -> boolean().
my_list([]) -> true.

-spec my_string(string()) -> boolean().
my_string("") -> true.
