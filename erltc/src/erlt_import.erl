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

-module(erlt_import).

-export([module/1]).

-record(context, {
    functions = #{},
    types = #{}
}).

module(Forms) ->
    Context = init(Forms),
    erlt_ast:prewalk(Forms, fun(Node, Ctx) -> rewrite(Node, Context, Ctx) end).

init(Forms) -> init(Forms, [], []).

init([{function, _, Name, Arity, _} | Rest], Functions, Types) ->
    init(Rest, [{{Name, Arity}, local} | Functions], Types);
init([{unchecked_function, _, Name, Arity, _} | Rest], Functions, Types) ->
    init(Rest, [{{Name, Arity}, local} | Functions], Types);
init([{attribute, _, import, {Mod, Imports}} | Rest], Functions0, Types) ->
    Functions = [{NA, {imported, Mod}} || NA <- Imports] ++ Functions0,
    init(Rest, Functions, Types);
init([{attribute, _, import_type, {Mod, Imports}} | Rest], Functions, Types0) ->
    Types = [{Name, {imported, Mod}} || {Name, _Arity} <- Imports] ++ Types0,
    init(Rest, Functions, Types);
init([{attribute, _, type, {Name, _, _}} | Rest], Functions, Types) ->
    init(Rest, Functions, [{Name, local} | Types]);
init([{attribute, _, opaque, {Name, _, _}} | Rest], Functions, Types) ->
    init(Rest, Functions, [{Name, local} | Types]);
init([{attribute, _, unchecked_opaque, {Name, _, _}} | Rest], Functions, Types) ->
    init(Rest, Functions, [{Name, local} | Types]);
init([{attribute, _, struct, {Name, _, _}} | Rest], Functions, Types) ->
    init(Rest, Functions, [{Name, local} | Types]);
init([{attribute, _, enum, {Name, _, _}} | Rest], Functions, Types) ->
    init(Rest, Functions, [{Name, local} | Types]);
init([_Other | Rest], Functions, Types) ->
    init(Rest, Functions, Types);
init([], Functions, Types) ->
    #context{
        functions = maps:from_list(Functions),
        types = maps:from_list(Types)
    }.

rewrite({struct, Line, Name, Fields}, Context, _Ctx) ->
    {struct, Line, full_type_name(Name, Context), Fields};
rewrite({struct, Line, Expr, Name, Fields}, Context, _Ctx) ->
    {struct, Line, Expr, full_type_name(Name, Context), Fields};
rewrite({struct_field, Line, Expr, Name, Field}, Context, _Ctx) ->
    {struct_field, Line, Expr, full_type_name(Name, Context), Field};
rewrite({struct_index, Line, Name, Field}, Context, _Ctx) ->
    {struct_index, Line, full_type_name(Name, Context), Field};
rewrite({enum, Line, Name, Constr, Fields}, Context, _Ctx) ->
    {enum, Line, full_type_name(Name, Context), Constr, Fields};
rewrite({'fun', Line, {function, Name, Arity}}, Context, _Ctx) ->
    case full_function_name({atom, Line, Name}, Arity, Context) of
        {atom, _, Name} ->
            {'fun', Line, {function, Name, Arity}};
        {remote, _, {atom, _, Mod}, {atom, _, Name}} ->
            {'fun', Line, {function, Mod, Name, Arity}}
    end;
rewrite({call, Line, Name, Args}, Context, _Ctx) ->
    {call, Line, full_function_name(Name, length(Args), Context), Args};
rewrite({user_type, Line, exception, []}, _Context, _Ctx) ->
    {type, Line, any, []};
rewrite({user_type, Line, message, []}, _Context, _Ctx) ->
    {type, Line, any, []};
rewrite({user_type, Line, Name, Args}, Context, _Ctx) ->
    case full_type_name({atom, Line, Name}, Context) of
        {atom, _, Name} ->
            {user_type, Line, Name, Args};
        {remote, _, Mod, NameAtom} ->
            {remote_type, Line, [Mod, NameAtom, Args]}
    end;
rewrite(Other, _Context, _Ctx) ->
    Other.

full_type_name({atom, Line, Name}, Context) ->
    case maps:find(Name, Context#context.types) of
        {ok, {imported, Mod}} ->
            GenLine = erl_anno:set_generated(true, Line),
            {remote, Line, {atom, GenLine, Mod}, {atom, GenLine, Name}};
        {ok, local} ->
            {atom, Line, Name}
    end;
full_type_name({remote, _, _, _} = Name, _Context) ->
    Name.

full_function_name({atom, Line, Name}, Arity, Context) ->
    case maps:find({Name, Arity}, Context#context.functions) of
        {ok, {imported, Mod}} ->
            GenLine = erl_anno:set_generated(true, Line),
            {remote, Line, {atom, GenLine, Mod}, {atom, GenLine, Name}};
        {ok, local} ->
            {atom, Line, Name};
        error ->
            case erl_internal:bif(Name, Arity) of
                true ->
                    GenLine = erl_anno:set_generated(true, Line),
                    {remote, Line, {atom, GenLine, erlang}, {atom, GenLine, Name}};
                false ->
                    %% Call to a generated function like module_info/0
                    {atom, Line, Name}
            end
    end;
full_function_name(RemoteOrExpr, _Arity, _Context) ->
    RemoteOrExpr.
