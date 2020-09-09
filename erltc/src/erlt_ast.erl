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

-module(erlt_ast).

-export([
    prewalk/2, prewalk/3,
    postwalk/2, postwalk/3,
    traverse/4
]).

-type ctx() :: form | expr | guard | pattern | type.

-type t() :: tuple() | [tuple()].

-spec prewalk(t(), fun((t(), ctx()) -> t())) -> t().
prewalk(Ast, Fun) ->
    element(1, prewalk(Ast, undefined, fun(Node, Acc, Ctx) -> {Fun(Node, Ctx), Acc} end)).

-spec prewalk(t(), any(), fun((t(), any(), ctx()) -> {t(), any()})) -> {t(), any()}.
prewalk(Ast, Acc0, Fun) ->
    traverse(Ast, Acc0, Fun, fun(Node, Acc, _Ctx) -> {Node, Acc} end).

-spec postwalk(t(), fun((t(), ctx()) -> t())) -> t().
postwalk(Ast, Fun) ->
    element(1, postwalk(Ast, undefined, fun(Node, Acc, Ctx) -> {Fun(Node, Ctx), Acc} end)).

-spec postwalk(t(), any(), fun((t(), any(), ctx()) -> {t(), any()})) -> {t(), any()}.
postwalk(Ast, Acc0, Fun) ->
    traverse(Ast, Acc0, fun(Node, Acc, _Ctx) -> {Node, Acc} end, Fun).

-define(IS_ATOMIC(Kind),
    Kind =:= integer orelse
        Kind =:= float orelse
        Kind =:= char orelse
        Kind =:= atom orelse
        Kind =:= string orelse
        Kind =:= var
).

-define(IS_TYPE(Kind),
    Kind =:= type orelse
        Kind =:= opaque orelse
        Kind =:= enum
).

-spec traverse(t(), any(), fun((t(), any(), ctx()) -> {t(), any()}), fun((t(), any(), ctx()) -> {t(), any()})) -> {t(), any()}.
traverse(Ast, Acc, Pre, Post) ->
    case Ast of
        %% the module definiton is a list
        List when is_list(List) ->
            Fun = fun(Node, Acc1) -> traverse(Node, Acc1, Pre, Post) end,
            lists:mapfoldl(Fun, Acc, List);
        {attribute, _, _, _} = Node ->
            do_traverse(Node, Acc, Pre, Post, form);
        {function, _, _, _, _} = Node ->
            do_traverse(Node, Acc, Pre, Post, form);
        %% don't traverse special parser forms
        {error, _} = Node ->
            {Node, Acc};
        {warning, _} = Node ->
            {Node, Acc};
        {eof, _} = Node ->
            {Node, Acc}
    end.

do_traverse(Node0, Acc, Pre, Post, Ctx) ->
    {Node, Acc0} = Pre(Node0, Acc, Ctx),
    case Node of
        {attribute, Line, Type, {Name, Def0, Args0}} when ?IS_TYPE(Type) ->
            {Args1, Acc1} = do_traverse_list(Args0, Acc0, Pre, Post, type),
            {Def1, Acc2} = do_traverse(Def0, Acc1, Pre, Post, type),
            Post({attribute, Line, Type, {Name, Def1, Args1}}, Acc2, Ctx);
        %% TODO: traverse other attributes that can have type defintions
        {attribute, _, _, _} ->
            Post(Node, Acc0, Ctx);
        {function, Line, Name, Arity, Clauses} ->
            {Clauses1, Acc1} = do_traverse_list(Clauses, Acc0, Pre, Post, Ctx),
            Post({function, Line, Name, Arity, Clauses1}, Acc1, Ctx);
        {clause, Line, Head0, Guard0, Body0} ->
            {Head1, Acc1} = do_traverse_list(Head0, Acc0, Pre, Post, pattern),
            {Guard1, Acc2} = do_traverse_guards(Guard0, Acc1, Pre, Post),
            {Body1, Acc3} = do_traverse_list(Body0, Acc2, Pre, Post, expr),
            Post({clause, Line, Head1, Guard1, Body1}, Acc3, Ctx);
        {Atomic, _, _} when ?IS_ATOMIC(Atomic) ->
            Post(Node, Acc0, Ctx);
        {nil, _} ->
            Post(Node, Acc0, Ctx);
        {match, Line, Left0, Right0} ->
            {Left1, Acc1} = do_traverse(Left0, Acc0, Pre, Post, pattern),
            {Right1, Acc2} = do_traverse(Right0, Acc1, Pre, Post, Ctx),
            Post({match, Line, Left1, Right1}, Acc2, Ctx);
        {cons, Line, Head0, Tail0} ->
            {Head1, Acc1} = do_traverse(Head0, Acc0, Pre, Post, Ctx),
            {Tail1, Acc2} = do_traverse(Tail0, Acc1, Pre, Post, Ctx),
            Post({cons, Line, Head1, Tail1}, Acc2, Ctx);
        {tuple, Line, Values0} ->
            {Values1, Acc1} = do_traverse_list(Values0, Acc0, Pre, Post, Ctx),
            Post({tuple, Line, Values1}, Acc1, Ctx);
        {enum, Line, Name0, Constr0, Values0} ->
            {Name1, Acc1} = do_traverse(Name0, Acc0, Pre, Post, Ctx),
            {Constr1, Acc2} = do_traverse(Constr0, Acc1, Pre, Post, Ctx),
            {Values1, Acc3} = do_traverse_list(Values0, Acc2, Pre, Post, Ctx),
            Post({enum, Line, Name1, Constr1, Values1}, Acc3, Ctx);
        {map, Line, Values0} ->
            {Values1, Acc1} = do_traverse_list(Values0, Acc0, Pre, Post, Ctx),
            Post({map, Line, Values1}, Acc1, Ctx);
        {map, Line, Expr0, Values0} ->
            {Expr1, Acc1} = do_traverse(Expr0, Acc0, Pre, Post, Ctx),
            {Values1, Acc2} = do_traverse_list(Values0, Acc1, Pre, Post, Ctx),
            Post({map, Line, Expr1, Values1}, Acc2, Ctx);
        {map_field_exact, Line, Key0, Value0} ->
            %% erl_id_trans in patterns traverses key as expr
            {Key1, Acc1} = do_traverse(Key0, Acc0, Pre, Post, pattern_to_expr(Ctx)),
            {Value1, Acc2} = do_traverse(Value0, Acc1, Pre, Post, Ctx),
            Post({map_field_exact, Line, Key1, Value1}, Acc2, Ctx);
        {map_field_assoc, Line, Key0, Value0} ->
            {Key1, Acc1} = do_traverse(Key0, Acc0, Pre, Post, Ctx),
            {Value1, Acc2} = do_traverse(Value0, Acc1, Pre, Post, Ctx),
            Post({map_field_assoc, Line, Key1, Value1}, Acc2, Ctx);
        {struct, Line, Name0, Fields0} ->
            {Name1, Acc1} = do_traverse(Name0, Acc0, Pre, Post, Ctx),
            {Fields1, Acc2} = do_traverse(Fields0, Acc1, Pre, Post, Ctx),
            Post({struct, Line, Name1, Fields1}, Acc2, Ctx);
        {record, Line, Name, Fields0} ->
            {Fields1, Acc1} = do_traverse_list(Fields0, Acc0, Pre, Post, Ctx),
            Post({record, Line, Name, Fields1}, Acc1, Ctx);
        {record, Line, Expr0, Name, Fields0} ->
            {Expr1, Acc1} = do_traverse(Expr0, Acc0, Pre, Post, Ctx),
            {Fields1, Acc2} = do_traverse_list(Fields0, Acc1, Pre, Post, Ctx),
            Post({record, Line, Expr1, Name, Fields1}, Acc2, Ctx);
        {record_index, Line, Name, Field0} ->
            {Field1, Acc1} = do_traverse(Field0, Acc0, Pre, Post, Ctx),
            Post({record_index, Line, Name, Field1}, Acc1, Ctx);
        {record_field, Line, Value0, Name, Field0} ->
            {Value1, Acc1} = do_traverse(Value0, Acc0, Pre, Post, Ctx),
            {Field1, Acc2} = do_traverse(Field0, Acc1, Pre, Post, Ctx),
            Post({record_field, Line, Value1, Name, Field1}, Acc2, Ctx);
        {record_field, Line, Name, Field0} ->
            {Field1, Acc1} = do_traverse(Field0, Acc0, Pre, Post, Ctx),
            Post({record_field, Line, Name, Field1}, Acc1, Ctx);
        {op, Line, Op, Expr0} ->
            {Expr1, Acc1} = do_traverse(Expr0, Acc0, Pre, Post, Ctx),
            Post({op, Line, Op, Expr1}, Acc1, Ctx);
        {op, Line, Op, Left0, Right0} ->
            {Left1, Acc1} = do_traverse(Left0, Acc0, Pre, Post, Ctx),
            {Right1, Acc2} = do_traverse(Right0, Acc1, Pre, Post, Ctx),
            Post({op, Line, Op, Left1, Right1}, Acc2, Ctx);
        {bin, Line, Values0} ->
            {Values1, Acc1} = do_traverse_list(Values0, Acc0, Pre, Post, Ctx),
            Post({bin, Line, Values1}, Acc1, Ctx);
        {bin_element, Line, Expr0, Size0, Type} when Size0 =/= default ->
            %% don't recurse into Type, it's not AST, but special syntax
            {Expr1, Acc1} = do_traverse(Expr0, Acc0, Pre, Post, Ctx),
            {Size1, Acc2} = do_traverse(Size0, Acc1, Pre, Post, Ctx),
            Post({bin_element, Line, Expr1, Size1, Type}, Acc2, Ctx);
        {bin_element, Line, Expr0, default, Type} ->
            %% don't recurse into Type, it's not AST, but special syntax
            {Expr1, Acc1} = do_traverse(Expr0, Acc0, Pre, Post, Ctx),
            Post({bin_element, Line, Expr1, default, Type}, Acc1, Ctx);
        {call, Line, Name0, Args0} ->
            {Name1, Acc1} = do_traverse(Name0, Acc0, Pre, Post, Ctx),
            {Args1, Acc2} = do_traverse_list(Args0, Acc1, Pre, Post, Ctx),
            Post({call, Line, Name1, Args1}, Acc2, Ctx);
        {remote, Line, Mod0, Name0} ->
            {Mod1, Acc1} = do_traverse(Mod0, Acc0, Pre, Post, Ctx),
            {Name1, Acc2} = do_traverse(Name0, Acc1, Pre, Post, Ctx),
            Post({remote, Line, Mod1, Name1}, Acc2, Ctx);
        {lc, Line, Expr0, Compr0} ->
            {Expr1, Acc1} = do_traverse(Expr0, Acc0, Pre, Post, Ctx),
            {Compr1, Acc2} = do_traverse(Compr0, Acc1, Pre, Post, Ctx),
            Post({lc, Line, Expr1, Compr1}, Acc2, Ctx);
        {bc, Line, Expr0, Compr0} ->
            {Expr1, Acc1} = do_traverse(Expr0, Acc0, Pre, Post, Ctx),
            {Compr1, Acc2} = do_traverse(Compr0, Acc1, Pre, Post, Ctx),
            Post({bc, Line, Expr1, Compr1}, Acc2, Ctx);
        {Generate, Line, Pattern0, Expr0} when Generate =:= generate; Generate =:= b_generate ->
            {Pattern1, Acc1} = do_traverse(Pattern0, Acc0, Pre, Post, pattern),
            {Expr1, Acc2} = do_traverse(Expr0, Acc1, Pre, Post, Ctx),
            Post({generate, Line, Pattern1, Expr1}, Acc2, Ctx);
        {block, Line, Exprs0} ->
            {Exprs1, Acc1} = do_traverse_list(Exprs0, Acc0, Pre, Post, Ctx),
            Post({block, Line, Exprs1}, Acc1, Ctx);
        {'if', Line, Clauses0} ->
            {Clauses1, Acc1} = do_traverse_list(Clauses0, Acc0, Pre, Post, Ctx),
            Post({'if', Line, Clauses1}, Acc1, Ctx);
        {'case', Line, Expr0, Clauses0} ->
            {Expr1, Acc1} = do_traverse(Expr0, Acc0, Pre, Post, Ctx),
            {Clauses1, Acc2} = do_traverse_list(Clauses0, Acc1, Pre, Post, Ctx),
            Post({'case', Line, Expr1, Clauses1}, Acc2, Ctx);
        {'receive', Line, Clauses0} ->
            {Clauses1, Acc1} = do_traverse_list(Clauses0, Acc0, Pre, Post, Ctx),
            Post({'receive', Line, Clauses1}, Acc1, Ctx);
        {'receive', Line, Clauses0, Timeout0, After0} ->
            {Clauses1, Acc1} = do_traverse_list(Clauses0, Acc0, Pre, Post, Ctx),
            {Timeout1, Acc2} = do_traverse(Timeout0, Acc1, Pre, Post, Ctx),
            {After1, Acc3} = do_traverse_list(After0, Acc2, Pre, Post, Ctx),
            Post({'receive', Line, Clauses1, Timeout1, After1}, Acc3, Ctx);
        {'fun', Line, {clauses, Clauses0}} ->
            {Clauses1, Acc1} = do_traverse_list(Clauses0, Acc0, Pre, Post, Ctx),
            Post({'fun', Line, {clauses, Clauses1}}, Acc1, Ctx);
        {'fun', _Line, {function, _F, _A}} ->
            Post(Node, Acc0, Ctx);
        {'fun', Line, {function, Module0, Fun0, Arity0}} ->
            {Module1, Acc1} = do_traverse(Module0, Acc0, Pre, Post, Ctx),
            {Fun1, Acc2} = do_traverse(Fun0, Acc1, Pre, Post, Ctx),
            {Arity1, Acc3} = do_traverse(Arity0, Acc2, Pre, Post, Ctx),
            Post({'fun', Line, {function, Module1, Fun1, Arity1}}, Acc3, Ctx);
        {named_fun, Line, Name, Clauses0} ->
            {Clauses1, Acc1} = do_traverse_list(Clauses0, Acc0, Pre, Post, Ctx),
            Post({named_fun, Line, Name, Clauses1}, Acc1, Ctx);
        {'catch', Line, Expr0} ->
            {Expr1, Acc1} = do_traverse(Expr0, Acc0, Pre, Post, Ctx),
            Post({'catch', Line, Expr1}, Acc1, Ctx);
        {'try', Line, Exprs0, OfClauses0, CatchClauses0, After0} ->
            {Exprs1, Acc1} = do_traverse_list(Exprs0, Acc0, Pre, Post, Ctx),
            {OfClauses1, Acc2} = do_traverse_list(OfClauses0, Acc1, Pre, Post, Ctx),
            {CatchClauses1, Acc3} = do_traverse_list(CatchClauses0, Acc2, Pre, Post, Ctx),
            {After1, Acc4} = do_traverse_list(After0, Acc3, Pre, Post, Ctx),
            Post({'try', Line, Exprs1, OfClauses1, CatchClauses1, After1}, Acc4, Ctx);
        {type, _, map, any} ->
            Post(Node, Acc0, Ctx);
        {type, _, tuple, any} ->
            Post(Node, Acc0, Ctx);
        {type, Line, enum, Name0, Constr0, Args0} ->
            {Name1, Acc1} = do_traverse(Name0, Acc0, Pre, Post, Ctx),
            {Constr1, Acc2} = do_traverse(Constr0, Acc1, Pre, Post, Ctx),
            {Args1, Acc3} = do_traverse_list(Args0, Acc2, Pre, Post, Ctx),
            Post({type, Line, enum, Name1, Constr1, Args1}, Acc3, Ctx);
        {type, Line, enum, Constr0, Args0} ->
            {Constr1, Acc1} = do_traverse(Constr0, Acc0, Pre, Post, Ctx),
            {Args1, Acc2} = do_traverse_list(Args0, Acc1, Pre, Post, Ctx),
            Post({type, Line, enum, Constr1, Args1}, Acc2, Ctx);
        {type, Line, Name, Args0} ->
            {Args1, Acc1} = do_traverse_list(Args0, Acc0, Pre, Post, Ctx),
            Post({type, Line, Name, Args1}, Acc1, Ctx);
        {type, _, any} ->
            Post(Node, Acc0, Ctx);
        {qualified_record, M0, N0} ->
            {M1, Acc1} = do_traverse(M0, Acc0, Pre, Post, Ctx),
            {N1, Acc2} = do_traverse(N0, Acc1, Pre, Post, Ctx),
            Post({qualified_record, M1, N1}, Acc2, Ctx);
        {ann_type, Line, Args0} ->
            {Args1, Acc1} = do_traverse_list(Args0, Acc0, Pre, Post, Ctx),
            Post({ann_type, Line, Args1}, Acc1, Ctx);
        {remote_type, Line, [Mod0, Name0, Args0]} ->
            {Mod1, Acc1} = do_traverse(Mod0, Acc0, Pre, Post, Ctx),
            {Name1, Acc2} = do_traverse(Name0, Acc1, Pre, Post, Ctx),
            {Args1, Acc3} = do_traverse_list(Args0, Acc2, Pre, Post, Ctx),
            Post({remote_type, Line, [Mod1, Name1, Args1]}, Acc3, Ctx);
        {user_type, Line, Name, Args0} ->
            {Args1, Acc1} = do_traverse_list(Args0, Acc0, Pre, Post, Ctx),
            Post({user_type, Line, Name, Args1}, Acc1, Ctx)
    end.

do_traverse_list(List, Acc0, Pre, Post, Ctx) ->
    Fun = fun(Node, Acc) ->
        do_traverse(Node, Acc, Pre, Post, Ctx)
    end,
    lists:mapfoldl(Fun, Acc0, List).

do_traverse_guards(List, Acc0, Pre, Post) ->
    Fun = fun(Nodes, Acc) ->
        do_traverse_list(Nodes, Acc, Pre, Post, guard)
    end,
    lists:mapfoldl(Fun, Acc0, List).

pattern_to_expr(pattern) -> expr;
pattern_to_expr(Other) -> Other.