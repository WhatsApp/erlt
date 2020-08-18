%% Copyright Ericsson AB 1996-2020. All Rights Reserved.
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

-module(erl2_util).

%% Utility functions that are useful in multiple modules.

-export([rewrite/2, collect/2]).

%% @doc Applies a rewrite to an AST bottom up and returns the new AST.
-spec rewrite(fun((term()) -> term()), term()) -> term().
rewrite(Rewrite, Node) ->
    WithRewrittenChildren =
        case Node of
            _ when is_list(Node) ->
                lists:map(fun(N) -> rewrite(Rewrite, N) end, Node);
            _ when is_tuple(Node) ->
                list_to_tuple(lists:map(fun(N) -> rewrite(Rewrite, N) end, tuple_to_list(Node)));
            _ ->
                Node
        end,
    Rewrite(WithRewrittenChildren).


%% @doc Applies a function to every node in an AST and collects all returned
%% values into a list (except when the function returns 'undefined').
%% The AST is traversed in postorder (left to right, children before the parent).
%% A "node" in the tree is any tuple value.
-spec collect(fun((term()) -> A | undefined), A) -> list(A).
collect(Collect, Node) ->
    do_collect(Collect, Node, []).

do_collect(_Collect, [], Acc) ->
    Acc;
do_collect(Collect, [H | T], Acc) ->
    Acc1 = do_collect(Collect, T, Acc),
    do_collect(Collect, H, Acc1);
do_collect(Collect, X, Acc) when is_tuple(X) ->
            Acc1 = do_collect(Collect, tuple_to_list(X), Acc),
            case Collect(X) of
                undefined -> Acc1;
                Delta -> [Delta | Acc1]
            end;
do_collect(_Collect, _X, Acc) ->
    Acc.
