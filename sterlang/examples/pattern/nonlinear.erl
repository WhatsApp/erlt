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

-lang([erl2, st]).
-module(nonlinear).
-export([equal/2, contains/2]).

-spec equal(boolean(), boolean()) -> boolean().
equal(X, X) -> true;
equal(_, _) -> false.

-spec contains(A, list(A)) -> boolean().
contains(_, []) -> false;
contains(X, [X | _]) -> true;
contains(X, [_ | T]) -> contains(X, T).
