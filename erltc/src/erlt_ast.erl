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

-include("erlt_ast.hrl").

-export([
    prewalk/2, prewalk/3,
    postwalk/2, postwalk/3,
    traverse/4,
    map_anno/2
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

-spec map_anno(t(), fun((erl_anno:anno()) -> erl_anno:anno())) -> t().
map_anno(Ast, Fun) ->
    prewalk(Ast, fun(Node, _Ctx) -> setelement(2, Node, Fun(element(2, Node))) end).

-spec traverse(t(), any(), fun((t(), any(), ctx()) -> {t(), any()}), fun(
    (t(), any(), ctx()) -> {t(), any()}
)) -> {t(), any()}.
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
            {Node, Acc};
        Node when tuple_size(Node) >= 2 ->
            do_traverse(Node, Acc, Pre, Post, expr)
    end.

do_traverse(Node0, Acc, Pre, Post, Ctx) ->
    {Node, Acc0} = Pre(Node0, Acc, Ctx),
    case Node of
        {attribute, Line, Type, {Name, Def0, Args0}} when ?IS_TYPE(Type) ->
            {Args1, Acc1} = do_traverse_list(Args0, Acc0, Pre, Post, type),
            {Def1, Acc2} = do_traverse(Def0, Acc1, Pre, Post, type),
            Post({attribute, Line, Type, {Name, Def1, Args1}}, Acc2, Ctx);
        {attribute, Line, Spec, {MFA, Types0}} when ?IS_SPEC(Spec) ->
            {Types1, Acc1} = do_traverse_list(Types0, Acc0, Pre, Post, type),
            Post({attribute, Line, Spec, {MFA, Types1}}, Acc1, Ctx);
        %% TODO: traverse other attributes that can have type defintions
        {attribute, _, _, _} ->
            Post(Node, Acc0, Ctx);
        {F, Line, Name, Arity, Clauses} when ?IS_FUNCTION(F) ->
            {Clauses1, Acc1} = do_traverse_list(Clauses, Acc0, Pre, Post, Ctx),
            Post({F, Line, Name, Arity, Clauses1}, Acc1, Ctx);
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
            {Values1, Acc3} = do_traverse_atom_or_list(Values0, Acc2, Pre, Post, Ctx),
            Post({enum, Line, Name1, Constr1, Values1}, Acc3, Ctx);
        {map, Line, Values0} ->
            {Values1, Acc1} = do_traverse_list(Values0, Acc0, Pre, Post, Ctx),
            Post({map, Line, Values1}, Acc1, Ctx);
        {map, Line, Expr0, Values0} ->
            {Expr1, Acc1} = do_traverse(Expr0, Acc0, Pre, Post, Ctx),
            {Values1, Acc2} = do_traverse_list(Values0, Acc1, Pre, Post, Ctx),
            Post({map, Line, Expr1, Values1}, Acc2, Ctx);
        {map_field_exact, Line, Key0, Value0} ->
            %% map keys are never pattern, but (limited) expressions
            {Key1, Acc1} = do_traverse(Key0, Acc0, Pre, Post, pattern_to_expr(Ctx)),
            {Value1, Acc2} = do_traverse(Value0, Acc1, Pre, Post, Ctx),
            Post({map_field_exact, Line, Key1, Value1}, Acc2, Ctx);
        {map_field_assoc, Line, Key0, Value0} ->
            {Key1, Acc1} = do_traverse(Key0, Acc0, Pre, Post, Ctx),
            {Value1, Acc2} = do_traverse(Value0, Acc1, Pre, Post, Ctx),
            Post({map_field_assoc, Line, Key1, Value1}, Acc2, Ctx);
        {shape, Line, Fields0} ->
            {Fields1, Acc1} = do_traverse_list(Fields0, Acc0, Pre, Post, Ctx),
            Post({shape, Line, Fields1}, Acc1, Ctx);
        {shape_update, Line, Expr0, Fields0} ->
            {Expr1, Acc1} = do_traverse(Expr0, Acc0, Pre, Post, Ctx),
            {Fields1, Acc2} = do_traverse_list(Fields0, Acc1, Pre, Post, Ctx),
            Post({shape_update, Line, Expr1, Fields1}, Acc2, Ctx);
        {shape_field, Line, Expr0, Field0} ->
            {Expr1, Acc1} = do_traverse(Expr0, Acc0, Pre, Post, Ctx),
            {Field1, Acc2} = do_traverse(Field0, Acc1, Pre, Post, Ctx),
            Post({shape_field, Line, Expr1, Field1}, Acc2, Ctx);
        {struct, Line, Name0, Fields0} ->
            {Name1, Acc1} = do_traverse(Name0, Acc0, Pre, Post, Ctx),
            {Fields1, Acc2} = do_traverse_list(Fields0, Acc1, Pre, Post, Ctx),
            Post({struct, Line, Name1, Fields1}, Acc2, Ctx);
        {struct, Line, Expr0, Name0, Fields0} ->
            {Expr1, Acc1} = do_traverse(Expr0, Acc0, Pre, Post, Ctx),
            {Name1, Acc2} = do_traverse(Name0, Acc1, Pre, Post, Ctx),
            {Fields1, Acc3} = do_traverse_list(Fields0, Acc2, Pre, Post, Ctx),
            Post({struct, Line, Expr1, Name1, Fields1}, Acc3, Ctx);
        {field, Line, Name0, Value0} ->
            {Name1, Acc1} = do_traverse_atom_or_node(Name0, Acc0, Pre, Post, Ctx),
            {Value1, Acc2} = do_traverse(Value0, Acc1, Pre, Post, Ctx),
            Post({field, Line, Name1, Value1}, Acc2, Ctx);
        {struct_field, Line, Expr0, Name0, Value0} ->
            {Expr1, Acc1} = do_traverse(Expr0, Acc0, Pre, Post, Ctx),
            {Name1, Acc2} = do_traverse(Name0, Acc1, Pre, Post, Ctx),
            {Value1, Acc3} = do_traverse(Value0, Acc2, Pre, Post, Ctx),
            Post({struct_field, Line, Expr1, Name1, Value1}, Acc3, Ctx);
        {struct_index, Line, Name0, Field0} ->
            {Name1, Acc1} = do_traverse(Name0, Acc0, Pre, Post, Ctx),
            {Field1, Acc2} = do_traverse(Field0, Acc1, Pre, Post, Ctx),
            Post({struct_index, Line, Name1, Field1}, Acc2, Ctx);
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
        {bin_element, Line, Expr0, Size0, Type} ->
            %% don't recurse into Type, it's not AST, but special syntax
            {Expr1, Acc1} = do_traverse(Expr0, Acc0, Pre, Post, Ctx),
            %% bin field size are never patterns, but (limited) expressions
            {Size1, Acc2} = do_traverse_atom_or_node(Size0, Acc1, Pre, Post, pattern_to_expr(Ctx)),
            Post({bin_element, Line, Expr1, Size1, Type}, Acc2, Ctx);
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
            {Compr1, Acc2} = do_traverse_list(Compr0, Acc1, Pre, Post, Ctx),
            Post({lc, Line, Expr1, Compr1}, Acc2, Ctx);
        {bc, Line, Expr0, Compr0} ->
            {Expr1, Acc1} = do_traverse(Expr0, Acc0, Pre, Post, Ctx),
            {Compr1, Acc2} = do_traverse_list(Compr0, Acc1, Pre, Post, Ctx),
            Post({bc, Line, Expr1, Compr1}, Acc2, Ctx);
        {Generate, Line, Pattern0, Expr0} when Generate =:= generate; Generate =:= b_generate ->
            {Pattern1, Acc1} = do_traverse(Pattern0, Acc0, Pre, Post, pattern),
            {Expr1, Acc2} = do_traverse(Expr0, Acc1, Pre, Post, Ctx),
            Post({Generate, Line, Pattern1, Expr1}, Acc2, Ctx);
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
        {type, Line, enum, Name0, Variants0} ->
            {Name1, Acc1} = do_traverse(Name0, Acc0, Pre, Post, Ctx),
            {Variants1, Acc2} = do_traverse_list(Variants0, Acc1, Pre, Post, Ctx),
            Post({type, Line, enum, Name1, Variants1}, Acc2, Ctx);
        {variant, Line, Name0, Fields0} ->
            {Name1, Acc1} = do_traverse(Name0, Acc0, Pre, Post, Ctx),
            {Fields1, Acc2} = do_traverse_atom_or_list(Fields0, Acc1, Pre, Post, Ctx),
            Post({variant, Line, Name1, Fields1}, Acc2, Ctx);
        {type, Line, struct, Name0, Fields0} ->
            {Name1, Acc1} = do_traverse(Name0, Acc0, Pre, Post, Ctx),
            {Fields1, Acc2} = do_traverse_list(Fields0, Acc1, Pre, Post, Ctx),
            Post({type, Line, struct, Name1, Fields1}, Acc2, Ctx);
        {field_definition, Line, Name0, Default0, Type0} ->
            {Name1, Acc1} = do_traverse_atom_or_node(Name0, Acc0, Pre, Post, Ctx),
            {Default1, Acc2} = do_traverse_atom_or_node(Default0, Acc1, Pre, Post, guard),
            {Type1, Acc3} = do_traverse(Type0, Acc2, Pre, Post, Ctx),
            Post({field_definition, Line, Name1, Default1, Type1}, Acc3, Ctx);
        {type, Line, open_shape, Args0, Var} ->
            {Args1, Acc1} = do_traverse_list(Args0, Acc0, Pre, Post, Ctx),
            {Var1, Acc2} = do_traverse(Var, Acc1, Pre, Post, Ctx),
            Post({type, Line, open_shape, Args1, Var1}, Acc2, Ctx);
        %% The first argument is the normal fun followed by a list of constarints.
        {type, Line, 'bounded_fun', [Fun0, Guards0]} ->
            {Fun1, Acc1} = do_traverse(Fun0, Acc0, Pre, Post, Ctx),
            {Guards1, Acc2} = do_traverse_list(Guards0, Acc1, Pre, Post, Ctx),
            Post({type, Line, 'bounded_fun', [Fun1, Guards1]}, Acc2, Ctx);
        %% The first argument to the constraint is the type of constraint followed by a list.
        {type, Line, constraint, [Constraint, Args0]} ->
            {Args1, Acc1} = do_traverse_list(Args0, Acc0, Pre, Post, Ctx),
            Post({type, Line, constraint, [Constraint, Args1]}, Acc1, Ctx);
        {type, Line, Name, Args0} ->
            {Args1, Acc1} = do_traverse_list(Args0, Acc0, Pre, Post, Ctx),
            Post({type, Line, Name, Args1}, Acc1, Ctx);
        {type, _, any} ->
            Post(Node, Acc0, Ctx);
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

do_traverse_guards(List0, Acc0, Pre, Post) ->
    %% no support for transforming guard_or/and to something else
    {{guard_or, _, List1}, Acc1} = Pre({guard_or, 0, List0}, Acc0, guard),
    Fun = fun(Nodes0, AccInner0) ->
        {{guard_and, _, Nodes1}, AccInner1} = Pre({guard_and, 0, Nodes0}, AccInner0, guard),
        {Nodes2, AccInner2} = do_traverse_list(Nodes1, AccInner1, Pre, Post, guard),
        {{guard_and, _, Nodes3}, AccInner3} = Post({guard_and, 0, Nodes2}, AccInner2, guard),
        {Nodes3, AccInner3}
    end,
    {List2, Acc2} = lists:mapfoldl(Fun, Acc1, List1),
    {{guard_or, _, List3}, Acc3} = Post({guard_or, 0, List2}, Acc2, guard),
    {List3, Acc3}.

do_traverse_atom_or_node(Atom, Acc, _Pre, _Post, _Ctx) when is_atom(Atom) ->
    {Atom, Acc};
do_traverse_atom_or_node(Node, Acc, Pre, Post, Ctx) when is_tuple(Node) ->
    do_traverse(Node, Acc, Pre, Post, Ctx).

do_traverse_atom_or_list(Atom, Acc, _Pre, _Post, _Ctx) when is_atom(Atom) ->
    {Atom, Acc};
do_traverse_atom_or_list(List, Acc, Pre, Post, Ctx) when is_list(List) ->
    do_traverse_list(List, Acc, Pre, Post, Ctx).

pattern_to_expr(pattern) -> expr;
pattern_to_expr(Other) -> Other.
