%% Copyright (c) 2020 Facebook, Inc. and its affiliates.
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

-module(erlt_atom).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    erlt_ast:prewalk(Forms, fun rewrite/2).

rewrite({atom_expr, Anno, Atom}, _) ->
    {atom, Anno, Atom};
rewrite(Other, _) ->
    Other.
