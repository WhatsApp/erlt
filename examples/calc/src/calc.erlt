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
-module(calc).

-export([calculate/2]).
-export_type([env/0]).

-type env() :: [{ atom(), integer() }].

-spec calculate(env(), string()) -> integer().
calculate(Env, Input) ->
    eval(Env, calc_parser_ffi:parse(Input)).

eval(Env, Expr) ->
    case Expr of
        calc_core.expr.number{N} ->
            N;
        calc_core.expr.add{E1, E2} ->
            eval(Env, E1) + eval(Env, E2);
        calc_core.expr.subtr{E1, E2} ->
            eval(Env, E1) - eval(Env, E2);
        calc_core.expr.mult{E1, E2} ->
            eval(Env, E1) * eval(Env, E2);
        calc_core.expr.divd{E1, E2} ->
            eval(Env, E1) / eval(Env, E2);
        calc_core.expr.var{Name} ->
            lookup(Name, Env)
    end.

lookup(A, [{A, V}|_]) ->
    V;
lookup(A, [_|Rest]) ->
    lookup(A, Rest).
