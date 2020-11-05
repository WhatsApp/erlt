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

-module(erlt_shape).

-include("erlt_ast.hrl").

-export([parse_transform/2]).

-record(state, {
    shape_vars = [] :: [atom()],
    all_vars = [] :: [atom()],
    % we collect all_vars while traversing type annotations
    is_collecting = false
}).

-define(IS_EXPR_OR_GUARD(Ctx), Ctx =:= expr orelse Ctx =:= guard).

parse_transform(Forms0, _Options) ->
    {Forms, _St} = erlt_ast:traverse(
        Forms0,
        #state{},
        fun pre_walk/3,
        fun post_walk/3
    ),
    Forms.

pre_walk({attribute, _, Kind, _} = Node, St, _Ctx) when ?IS_TYPE(Kind) ->
    {Node, St#state{is_collecting = true}};
pre_walk({var, _Info, Name} = Node, #state{is_collecting = true} = St, _Ctx) ->
    {Node, St#state{all_vars = [Name | St#state.all_vars]}};
pre_walk({type, Line, closed_shape, Fields}, St, type) ->
    MapTypeFields = to_typed_map_fields(Fields),
    {{type, Line, map, MapTypeFields}, St};
pre_walk({type, Line, open_shape, Fields, {var, _Line, VarName}}, St0, type) ->
    MapTypeFields = to_typed_map_fields(Fields),
    St = St0#state{shape_vars = [VarName | St0#state.shape_vars]},
    {{type, Line, map, MapTypeFields ++ [generic_open_shape_field_type(Line)]}, St};
pre_walk({shape, Line, Fields}, St, pattern) ->
    MapFields = to_map_fields(map_field_exact, Fields),
    {{map, Line, MapFields}, St};
pre_walk({shape, Line, Fields}, St, Ctx) when ?IS_EXPR_OR_GUARD(Ctx) ->
    MapFields = to_map_fields(map_field_assoc, Fields),
    {{map, Line, MapFields}, St};
pre_walk({shape_update, Line, Expr, Fields}, St, Ctx) when ?IS_EXPR_OR_GUARD(Ctx) ->
    MapFields = to_map_fields(map_field_assoc, Fields),
    {{map, Line, Expr, MapFields}, St};
pre_walk({shape_field, Line, Expr, Field}, St, Ctx) when ?IS_EXPR_OR_GUARD(Ctx) ->
    {{call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, map_get}}, [Field, Expr]}, St};
pre_walk(Other, St, _Ctx) ->
    {Other, St}.

post_walk({attribute, Line, Kind, {Name, Def0, Args0}}, St, _Forms) when ?IS_TYPE(Kind) ->
    Args = [
        {var, TheLine, transform_unused_shape_vars(TheName, St)}
        || {var, TheLine, TheName} <- Args0
    ],
    {{attribute, Line, Kind, {Name, Def0, Args}}, #state{}};
post_walk(
    {attribute, Line, spec, {FA, [{type, Loc, 'fun', [{type, _, product, Args0} | _]} = F]}} =
        Forms,
    St,
    _Forms
) ->
    Anno = erl_anno:set_generated(true, Line),
    MapType = {type, Anno, map, [generic_open_shape_field_type(Anno)]},
    % add guards for each row var: `-spec .... when RowVar :: #{atom() -> any()}`
    Guards = [
        {type, Anno, constraint, [{atom, Anno, is_subtype}, [{var, Anno, Name}, MapType]]}
        || {var, _Line, Name} <- Args0, lists:member(Name, St#state.shape_vars)
    ],
    case Guards of
        [] ->
            {Forms, St};
        _ ->
            BoundedFun = {type, Loc, bounded_fun, [F, Guards]},
            Node = {attribute, Line, spec, {FA, [BoundedFun]}},
            {Node, St}
    end;
post_walk(Other, St, _Ctx) ->
    {Other, St}.

transform_unused_shape_vars(VarName, St) ->
    Used = St#state.all_vars -- St#state.shape_vars,
    IsUnusedShapeVar =
        lists:member(VarName, St#state.shape_vars) andalso not lists:member(VarName, Used),
    case IsUnusedShapeVar of
        false ->
            VarName;
        true ->
            NewVarName = atom_prepend("__ERLT_SHAPE_VAR_", VarName),
            no_conflict(St#state.all_vars, NewVarName)
    end.

generic_open_shape_field_type(Line) ->
    {type, Line, map_field_assoc, [{type, Line, atom, []}, {type, Line, any, []}]}.

to_typed_map_fields(Fields) ->
    [
        {type, Line, map_field_exact, [Key, Value]}
        || {field_definition, Line, Key, undefined, Value} <- Fields
    ].

to_map_fields(TypeOfMapField, Fields) ->
    [{TypeOfMapField, Line, Key, Value} || {field, Line, Key, Value} <- Fields].

atom_prepend(Prefix, Atom) ->
    list_to_atom(Prefix ++ atom_to_list(Atom)).

no_conflict(OtherVars, Var) ->
    case lists:member(Var, OtherVars) of
        false -> Var;
        true -> no_conflict(OtherVars, atom_prepend("_", Var))
    end.
