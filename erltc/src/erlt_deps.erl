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

-module(erlt_deps).

-export([
    st_deps/1,
    ffi_deps/1,
    dt_deps/1
]).

st_deps(Forms) ->
    collect(Forms, [fun calls/1, fun structs/1, fun enums/1, fun types/1]).

ffi_deps(Forms) ->
    collect(Forms, [fun structs/1, fun enums/1, fun types/1]).

dt_deps(Forms) ->
    collect(Forms, [fun structs/1, fun enums/1]).

calls({call, _, {remote, _, {atom, L, M}, _}, _}) ->
    [{L, M}];
calls({'fun', _, {function, {atom, L, M}, _, _}}) ->
    [{L, M}];
calls(_) ->
    [].

types({remote_type, _, [{atom, L, M} | _]}) ->
    [{L, M}];
types(_) ->
    [].

structs({struct, _, {remote, _, {atom, L, M}, _}, _}) ->
    [{L, M}];
structs({struct, _, _, {remote, _, {atom, L, M}, _}, _}) ->
    [{L, M}];
structs({struct_field, _, _, {remote, _, {atom, L, M}, _}, _}) ->
    [{L, M}];
structs(_) ->
    [].

enums({enum, _, {remote, _, {atom, L, M}, _}, _, _}) ->
    [{L, M}];
enums(_) ->
    [].

collect(Forms, Collectors) ->
    Module = get_module(Forms),
    {_Forms, Results} =
        erlt_ast:prewalk(Forms, [], fun(Node, Acc, _Ctx) ->
            Results = lists:flatmap(fun(Fun) -> Fun(Node) end, Collectors),
            {Node, [{L, M} || {L, M} <- Results, M =/= Module, M =/= ffi] ++ Acc}
        end),
    Results.

get_module(Forms) ->
    erlang:hd([M || {attribute, _, module, M} <- Forms]).
