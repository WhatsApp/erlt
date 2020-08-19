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
-module(interop03).

-enum bare() :: int{integer()} | bool{boolean()}.

-spec erl1_type_bare(integer() | boolean()) -> bare().
erl1_type_bare(X) when is_integer(X) -> bare.int{X};
erl1_type_bare(X) when is_boolean(X) -> bare.bool{X}.

test() ->
    case bare.int{100} of
        (bare.int{X}) -> bare.bool{true};
        (bare.bool{X}) -> bare.int{0}
    end.
