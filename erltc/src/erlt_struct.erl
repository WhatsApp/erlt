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

-module(erlt_struct).

%% The skeleton for this module is erl_id_trans.

%% This module only traverses legal Erlang code. This is most noticeable
%% in guards where only a limited number of expressions are allowed.
%% N.B. if this module is to be used as a basis for transforms then
%% all the error cases must be handled otherwise this module just crashes!

-export([parse_transform/2]).

-record(context, {
    module :: atom(),
    struct = [],
    structs = #{}
}).

parse_transform(Forms, _Options) ->
    Context = init_context(Forms),
    forms(Forms, Context).

init_context(Forms) ->
    [Module] = [M || {attribute, _, module, M} <- Forms],
    Structs = [Def || {attribute, _, struct, Def} <- Forms],
    #context{
        module = Module,
        structs = init_structs(Structs, Module)
    }.

init_structs(Defs, Module) ->
    Map = [{Tag, struct_info(Module, Tag, Fields)} || {_Name, {type, _, struct, Tag, Fields}, _Args} <- Defs],
    maps:from_list(Map).

struct_info(Module, {atom, _, Tag}, Fields) ->
    RuntimeTag = list_to_atom("$#" ++ atom_to_list(Module) ++ ":" ++ atom_to_list(Tag)),
    Anno = erl_anno:set_generated(true, erl_anno:new(0)),
    {{atom, Anno, RuntimeTag}, Fields}.


%% forms(Fs,Context) -> lists:map(fun (F) -> form(F) end, Fs).

forms([F0 | Fs0], Context) ->
    F1 =
        try form(F0, Context)
        catch
            {error, Loc, Info} ->
                {error, {Loc, ?MODULE, Info}}
        end,
    Fs1 = forms(Fs0, Context),
    [F1 | Fs1];
forms([], _Context) ->
    [].

form({attribute, Line, struct, {TypeName, StructType, Args}}, Context) ->
    {type, TypeLine, struct, Tag, Fields} = StructType,
    {RuntimeTag, _} = map_get(Tag, Context#context.structs),
    Type = {type, TypeLine, tuple, [RuntimeTag | [Type || {struct_field, _, _Name, Type} <- Fields]]},
    {attribute, Line, type, {TypeName, Type, Args}};
form(Other, _Context) ->
    Other.
