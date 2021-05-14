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

-module(erlt_pinning).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    St = init_state(),
    {Forms1, _St1} = erlt_ast:traverse(Forms, St, fun pre/3, fun post/3),
    Forms1.

%% This pass eliminates the ^ notation from pattern variables, either
%% leaving the already-bound variable as it is, or replacing it with a
%% fresh variable in those contexts where shadowing is in effect.
%%
%% To ensure that pinned variables can be accessed whether or not they
%% happen to get shadowed in the same pattern, we always introduce fresh
%% names for them, guaranteed not to become shadowed. We also need a fresh
%% variable for the location in the pattern, since if the pattern is
%% shadowing, we cannot use the original name. For example:
%%
%%   X = 42,
%%   F = fun({foo, ^X, X}) -> {bar, X} end
%%
%% Here, the inner name X refers to the third element of the tuple, if
%% there is a match, while the ^X refers to the outer X. This will be
%% translated to:
%%
%%   X = 42,
%%   F = begin
%%         _pin_o1=X,
%%         fun({foo, _pin_i1, X}) when _pin_i1 =:= _pin_o1 -> {bar, X} end
%%       end
%%
%% which ultimately is what the compiler will do anyway with already-bound
%% variables in patterns - the renamings do not cause any runtime overhead.
%%
%% To avoid generating many redundant variables in case the same variable
%% is pinned in many parts of the same construct, we try to reuse the
%% generated outer and inner names where this is possible (inner names
%% cannot be reused in comprehensions, since generators shadow each other).

-record(ctx, {
    keep = true,
    reuse_inner = true
}).

-record(state, {
    %% lift pinnings only for shadowing
    keep_nonshadowing = true,
    %% stack of contexts
    ctx = [] :: #ctx{},
    %% stack of variable sets
    pinned = [],
    var_count = 0
}).

init_state() ->
    #state{}.

pre({op, _, '^', {var, Line, V}}, #state{ctx = [#ctx{keep = true} | _]} = St, pattern) ->
    %% in constructs where we allow use of already bound variables
    %% we just drop the ^ and keep the variable as it is
    {{var, Line, V}, St};
pre({op, _, '^', {var, Line, V}}, #state{ctx = [#ctx{keep = false} | _]} = St, pattern) ->
    %% generate inner & outer names, reusing the outer if already existing,
    %% and replace ^V with inner name; in both cases we ensure that the
    %% mapping is stored in the current clause pinnings so that guard tests
    %% are generated in all clauses where this variable occurs pinned
    case find_pin(V, St) of
        {ok, {Vo, Vi}} ->
            case St#state.ctx of
                [#ctx{reuse_inner = false} | _] ->
                    {Vi1, St1} = mk_var("__pin_i", St),
                    {{var, Line, Vi1}, add_pin(V, Vo, Vi1, St1)};
                _ ->
                    {{var, Line, Vi}, add_pin(V, Vo, Vi, St)}
            end;
        error ->
            {Vo, Vi, St1} = mk_var_pair(St),
            {{var, Line, Vi}, add_pin(V, Vo, Vi, St1)}
    end;
pre({match, _L, _P, _E} = Expr, St, expr) ->
    %% since matches have no separate clause substructure we need to
    %% push two empty sets here in order to make end_construct() work
    St1 = push_empty_pinned(St),
    {Expr, begin_construct(St#state.keep_nonshadowing, St1)};
pre({'case', _L, _E, _Cs} = Expr, St, expr) ->
    {Expr, begin_construct(St#state.keep_nonshadowing, St)};
pre({'receive', _L, _Cs} = Expr, St, expr) ->
    {Expr, begin_construct(St#state.keep_nonshadowing, St)};
pre({'receive', _L, _Cs, _T, _A} = Expr, St, expr) ->
    {Expr, begin_construct(St#state.keep_nonshadowing, St)};
pre({'try', _L, _Es, _OCs, _CCs, _A} = Expr, St, expr) ->
    {Expr, begin_construct(St#state.keep_nonshadowing, St)};
pre({'fun', _L, {clauses, _Cs}} = Expr, St, expr) ->
    {Expr, begin_construct(false, St)};
pre({named_fun, _L, _N, _Cs} = Expr, St, expr) ->
    {Expr, begin_construct(false, St)};
pre({lc, _L, _E, _C} = Expr, St, expr) ->
    {Expr, begin_construct(false, St)};
pre({bc, _L, _E, _C} = Expr, St, expr) ->
    {Expr, begin_construct(false, St)};
pre({clause, _L, _Head, _Guard, _Body} = Clause, #state{ctx = [#ctx{keep = false} | _]} = St, _) ->
    %% push an empty set of pinnings for the clause
    {Clause, push_empty_pinned(St)};
pre({Generate, _L, _Pattern, _Expr} = Generator, St, expr) when
    Generate =:= generate; Generate =:= b_generate
->
    %% push an empty set of pinnings for the generator and ensure we do not
    %% reuse inner variable names because of shadowing between generators
    {Generator, push_empty_pinned(disable_reuse(St))};
pre(Other, St, _) ->
    {Other, St}.

disable_reuse(#state{ctx = [Ctx | Cs]} = St) ->
    St#state{ctx = [Ctx#ctx{reuse_inner = false} | Cs]}.

begin_construct(Keep, St) ->
    %% pushes a new pinning set and context
    push_empty_pinned(St#state{ctx = [#ctx{keep = Keep} | St#state.ctx]}).

%% Since we need to collect all pinned vars for a whole expression (like a
%% case), but generate guard tests per clause, and we also need to handle
%% nested constructs in clause bodies, we keep a stack of sets of
%% pinnings, like this: [ClausePins, ExprPins, ...], where the topmost is
%% the collection of pinned variables found in the current clause, and the
%% second is the collection of pinned variables found in previous clauses
%% of the expression. When entering a new matching expression or a new
%% clause, an empty set is pushed onto the stack, which get popped when we
%% leave the clause or expression again. We use orddicts for the sets, so
%% we know that [] is an empty set and we can traverse the sets as lists.
%%
%% We check for existing pinnings both for the current clause and for the
%% expression as a whole so as not to generate lots of redundant names for
%% the same variable, in case there are many clauses containing the same
%% pinned variable.

%% look up existing pinning for a variable, either in the current clause
%% set or the current expression set (if in both, should have the same
%% outer variable)
find_pin(V, #state{pinned = [Ps0, Ps1 | _]}) ->
    case orddict:find(V, Ps0) of
        {ok, Info} ->
            {ok, Info};
        error ->
            case orddict:find(V, Ps1) of
                {ok, Info} -> {ok, Info};
                error -> error
            end
    end.

%% adds a pinned variable to the set of the current clause
add_pin(V, Vo, Vi, #state{pinned = [Ps | Pss]} = St) ->
    Ps1 = orddict:store(V, {Vo, Vi}, Ps),
    St#state{pinned = [Ps1 | Pss]}.

%% pushes an empty pinned set on the stack
push_empty_pinned(#state{pinned = Pss} = St) ->
    St#state{pinned = [[] | Pss]}.

%% pops the top pinned set
pop_pinned(#state{pinned = [Ps | Pss]} = St) ->
    {Ps, St#state{pinned = Pss}}.

%% merges the two top pinned sets, returning the previously topmost; if the
%% same key exists in both, we assert that they have the same outer name
%% and make a list of corresponding inner variables (mainly for debugging)
merge_pinned(#state{pinned = [Ps0, Ps1 | Pss]} = St) ->
    % assert same Vo and merge Vi to a list
    MergeFun = fun(_K, {Vo, Vi0}, {Vo, Vi1}) ->
        if
            is_list(Vi0) -> {Vo, [Vi1 | Vi0]};
            true -> {Vo, [Vi1, Vi0]}
        end
    end,
    Merged = orddict:merge(MergeFun, Ps0, Ps1),
    {Ps0, St#state{pinned = [Merged | Pss]}}.

%% resets the pinning state
clear_pinned(St) ->
    St#state{pinned = []}.

%% We must set up outer bindings for all pattern matching expressions,
%% and tests for equality to clause guards for all inner variables.
%% Note: forgetting to pop the pinning stack will have strange effects.

post(Form, St, form) ->
    %% ensure pinning info doesn't propagate between forms
    {Form, clear_pinned(St)};
post({match, L, Pat, Expr} = Match, #state{ctx = [#ctx{keep = false} | _]} = St, expr) ->
    %% pop the set of pinnings for this match and add them to the total set
    %% for the enclosing expression
    {Pins, St1} = merge_pinned(St),
    %% matches are equivalent to case expressions with a single clause, but
    %% we must preserve the 'badmatch' error and not get a 'case_clause',
    %% and it is then simplest to put the extra tests in a separate 'if'
    %% following the match, raising 'badmatch' in the catch-all; we need an
    %% extra variable to carry the matched value past the tests
    %% only rewrite matches if they actually contain pinned variables
    case Pins of
        [] ->
            end_construct(Match, St1);
        _ ->
            Tests = mk_tests(Pins),
            {V, St2} = mk_var("__pin_m", St1),
            %% note that we want to always allow begin...end blocks to
            %% export bindings, otherwise it gets much more complicated to
            %% replace a single expression with a code sequence like this
            Expr1 =
                {block, L, [
                    {match, L, {var, L, V}, Expr},
                    {match, L, Pat, {var, L, V}},
                    {'if', L, [
                        {clause, L, [], [Tests], [{var, L, V}]},
                        {clause, L, [], [[{atom, L, true}]], [
                            {call, L, {remote, L, {atom, L, erlang}, {atom, L, error}}, [
                                {tuple, L, [
                                    {atom, L, badmatch},
                                    {var, L, V}
                                ]}
                            ]}
                        ]}
                    ]}
                ]},
            end_construct(Expr1, St2)
    end;
post({match, _L, _P, _E} = Expr, #state{ctx = [#ctx{keep = true} | _]} = St, expr) ->
    end_construct(Expr, St);
post({'case', _L, _E, _Cs} = Expr, St, expr) ->
    end_construct(Expr, St);
post({'receive', _L, _Cs} = Expr, St, expr) ->
    end_construct(Expr, St);
post({'receive', _L, _Cs, _T, _A} = Expr, St, expr) ->
    end_construct(Expr, St);
post({'try', _L, _Es, _OCs, _CCs, _A} = Expr, St, expr) ->
    end_construct(Expr, St);
post({'fun', _L, {clauses, _Cs}} = Expr, St, expr) ->
    end_construct(Expr, St);
post({named_fun, _L, _N, _Cs} = Expr, St, expr) ->
    end_construct(Expr, St);
post({lc, L, E, C}, St, expr) ->
    %% flatten the list of generators-and-tests, see the generator case below
    Expr1 = {lc, L, E, lists:flatten(C)},
    end_construct(Expr1, St);
post({bc, L, E, C}, St, expr) ->
    %% flatten the list of generators-and-tests, see the generator case below
    Expr = {bc, L, E, lists:flatten(C)},
    end_construct(Expr, St);
post({clause, L, Head, Guard, Body}, #state{ctx = [#ctx{keep = false} | _]} = St, _) ->
    %% pop the set of pinnings for this clause and add them to the total
    %% set for the enclosing expression
    {Pins, St1} = merge_pinned(St),
    %% add corresponding guard tests to the clause; note that guards are
    %% lists of lists of tests, but you can't have empty inner lists
    Tests = mk_tests(Pins),
    Guard1 =
        case Guard of
            [] when Tests =/= [] -> [Tests];
            [] -> [];
            [_ | _] -> [Tests ++ Conj || Conj <- Guard]
        end,
    {{clause, L, Head, Guard1, Body}, St1};
post({Generate, _L, _Pattern, _Expr}, St, expr) when
    Generate =:= generate; Generate =:= b_generate
->
    %% pop the set of pinnings for this generator and add them to the total
    %% set for the enclosing expression
    {Pins, St1} = merge_pinned(St),
    %% insert corresponding guard tests right after the generator, but note
    %% that afterwards we need to flatten the list of generators-and-tests
    %% in the post-handling for the comprehension as a whole
    Tests = mk_tests(Pins),
    {[{Generate, _L, _Pattern, _Expr} | Tests], St1};
post(Other, St, _) ->
    {Other, St}.

%% pops the total set of pinnings for an expression and adds corresponding
%% outer bindings to the expression; also pops the context
end_construct(Expr, #state{ctx = [_ | Ctx]} = St) ->
    {Pins, St1} = pop_pinned(St),
    {mk_bind(Pins, Expr), St1#state{ctx = Ctx}}.

mk_bind([], Expr) ->
    Expr;
mk_bind(Pins, Expr) ->
    {block, 0, mk_bind_1(Pins, Expr)}.

mk_bind_1([{V, {Vo, _Vi}} | Pins], Expr) ->
    [{match, 0, {var, 0, Vo}, {var, 0, V}} | mk_bind_1(Pins, Expr)];
mk_bind_1([], Expr) ->
    [Expr].

mk_tests([{_V, {Vo, Vi}} | Pins]) ->
    [{op, 0, '=:=', {var, 0, Vi}, {var, 0, Vo}} | mk_tests(Pins)];
mk_tests([]) ->
    [].

mk_var(Prefix, #state{var_count = N} = St) ->
    V = list_to_atom(Prefix ++ integer_to_list(N)),
    {V, St#state{var_count = N + 1}}.

mk_var_pair(#state{var_count = N} = St) ->
    Vo = list_to_atom("__pin_o" ++ integer_to_list(N)),
    Vi = list_to_atom("__pin_i" ++ integer_to_list(N)),
    {Vo, Vi, St#state{var_count = N + 1}}.
