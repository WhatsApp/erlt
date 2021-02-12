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

-module(erlt_try).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    erlt_ast:prewalk(Forms, fun rewrite/2).

rewrite({'try', Anno, Exprs, OfClauses, CatchClauses, After}, _) ->
    {'try', Anno, Exprs, OfClauses, rewrite_clauses(CatchClauses), After};
rewrite(Other, _) ->
    Other.

rewrite_clauses(Clauses) ->
    [
        {clause, Anno, [rewrite_head(Head)], Guards, Body}
     || {clause, Anno, Head, Guards, Body} <- Clauses
    ].

rewrite_head([Reason]) ->
    {tuple, 0, [{atom, 0, throw}, Reason, {var, 0, '_'}]};
rewrite_head([Kind, Reason]) ->
    {tuple, 0, [Kind, Reason, {var, 0, '_'}]};
rewrite_head([Kind, Reason, Stack]) ->
    {tuple, 0, [Kind, Reason, Stack]}.
