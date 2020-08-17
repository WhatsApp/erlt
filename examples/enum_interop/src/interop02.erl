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
-module(interop02).

-enum default() :: int{integer()} | bool{boolean()}.

-enum custom() :: int{integer()} | bool{boolean()}.

-spec erl1_type_custom({int, integer()} | {bool, boolean()}) -> custom().
erl1_type_custom({int, X}) -> custom.int{X};
erl1_type_custom({bool, X}) -> custom.bool{X}.

test() ->
    Custom = custom.int{100},
    case Custom of
        (custom.int{X}) -> custom.bool{true};
        (custom.bool{X}) -> custom.int{0}
    end.
