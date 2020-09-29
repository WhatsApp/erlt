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

-module(erlt_anon_struct).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    erlt_ast:postwalk(Forms, fun(Node, Ctx) -> rewrite(Node, Ctx) end).

rewrite({type, Line, closed_anon_struct, Fields}, type) ->
    MapTypeFields = to_typed_map_fields(Fields),
    {type, Line, map, MapTypeFields};
rewrite({type, Line, open_anon_struct, Fields, _Var}, type) ->
    MapTypeFields = to_typed_map_fields(Fields),
    {type, Line, map, MapTypeFields ++ [generic_open_anon_struct_field_type(Line)]};
rewrite({anon_struct, Line, Fields}, pattern) ->
    MapFields = to_map_fields(map_field_exact, Fields),
    {map, Line, MapFields};
rewrite({anon_struct, Line, Fields}, Ctx) when Ctx =:= expr orelse Ctx =:= guard ->
    MapFields = to_map_fields(map_field_assoc, Fields),
    {map, Line, MapFields};
rewrite({anon_struct_update, Line, Expr, Fields}, Ctx) when Ctx =:= expr orelse Ctx =:= guard ->
    MapFields = to_map_fields(map_field_assoc, Fields),
    {map, Line, Expr, MapFields};
rewrite({anon_struct_field, Line, Expr, Field}, Ctx) when Ctx =:= expr orelse Ctx =:= guard ->
    {call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, map_get}}, [Field, Expr]};
rewrite(Other, _) ->
    Other.

generic_open_anon_struct_field_type(Line) ->
    {type, Line, map_field_assoc, [{type, Line, atom, []}, {type, Line, any, []}]}.

to_typed_map_fields(Fields) ->
    [
        {type, Line, map_field_exact, [Key, Value]}
        || {field_definition, Line, Key, undefined, Value} <- Fields
    ].

to_map_fields(TypeOfMapField, Fields) ->
    [{TypeOfMapField, Line, Key, Value} || {field, Line, Key, Value} <- Fields].
