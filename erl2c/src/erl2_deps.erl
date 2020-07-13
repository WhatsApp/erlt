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

-module(erl2_deps).

-export([
    st_deps/1,
    ffi_deps/1
]).

get_module(Forms) ->
    erlang:hd([M || {attribute, _, module, M} <- Forms]).

st_deps(Forms) ->
    Module = get_module(Forms),
    Remotes = collect(Forms, fun st_pred/1, fun remote/1),
    [{L, M} || {L, M} <- Remotes, M =/= ffi, M =/= Module].

ffi_deps(Forms) ->
    Module = get_module(Forms),
    Remotes = collect(Forms, fun ffi_pred/1, fun remote/1),
    [{L, M} || {L, M} <- Remotes, M =/= ffi, M =/= Module].

%% remote call
remote({remote, _Ln, {atom, Ln, Mod}, _}) ->
    {Ln, Mod};
%% remote type
remote({remote_type, _Ln, [{atom, Ln, Mod} | _]}) ->
    {Ln, Mod};
%% remote enum ctr
remote(
    {enum, _, {remote, _, {atom, Ln, Mod}, {atom, _, _Enum}}, {atom, _, _Ctr}, _Args}
) ->
    {Ln, Mod};
%% fn mod:f/n
remote({'fun', _Ln, {function, {atom, Ln, Mod}, {atom, _, F}, {integer, _, A}}}) when
    is_atom(Mod), is_atom(F), is_integer(A)
->
    {Ln, Mod};
remote(_Form) ->
    false.

st_pred(_Form) ->
    true.

%% ffi is not interested in function bodies
ffi_pred({function, _, _, _, _}) ->
    false;
ffi_pred(_) ->
    true.

collect(Forms, Pred, Collect) ->
    do_collect(Forms, Pred, Collect, []).

do_collect([], _Pred, _Collect, Acc) ->
    Acc;
do_collect([H | T], Pred, Collect, Acc) ->
    Acc1 = do_collect(T, Pred, Collect, Acc),
    do_collect(H, Pred, Collect, Acc1);
do_collect(X, Pred, Collect, Acc) when is_tuple(X) ->
    case Pred(X) of
        true ->
            Acc1 = do_collect(tuple_to_list(X), Pred, Collect, Acc),
            case Collect(X) of
                false -> Acc1;
                Delta -> [Delta | Acc1]
            end;
        false ->
            Acc
    end;
do_collect(_X, _Pred, _Collect, Acc) ->
    Acc.
