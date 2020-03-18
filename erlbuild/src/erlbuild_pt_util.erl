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

% some useful parse-transforms
%
% usage:
%
% -compile({parse_transform, erlbuild_pt_util}).
%
%
% 1. precompile a 0-ary function by listing its name in a -const(ListOfNames)
%    attribute. For example, with the following code:
%
%    -const([get_foos]).
%    ...
%    get_foos() ->
%       FooCount = 300,
%       Strings = [lists:concat("foo_", N) || N <- lists:seq(1, FooCount)],
%       list_to_tuple([list_to_atom(S) || S <- Strings]).
%
%    get_foos() will always return the same pre-constructed 300-tuple of atoms,
%    avoiding the list-building and (surprisingly expensive) lists:concat and
%    list_to_atom bifs at runtime. The bodies of -const functions are restricted
%    to only a whitelisted set of library functions and BIFs. More examples are
%    in const_example.erl.
%
%    WARNING: The code compiles and runs on different hosts. They may have
%    different number of cores (schedulers), RAM, etc. Therefore if your
%    pre-compiled code relies on any of host-specific parameters, please
%    either pad them up to a safe(r) limit, or come up with another solution
%    to this issue.

-module(erlbuild_pt_util).

-export([parse_transform/2]).

-type const_names() :: ordsets:ordset(atom()).
-type form()        :: erl_parse:abstract_form().

%-define(DEBUG, 1).

-ifdef(DEBUG).
-compile(export_all).
-define(PRINT(Fmt, Args), io:format(standard_error, Fmt "~n", Args)).
-define(PRINT(Fmt), ?PRINT(Fmt, [])).
-else.
-define(PRINT(Fmt, Args), ok).
-define(PRINT(Fmt), ok).
-endif.


parse_transform(Forms, _Options) ->
    ?PRINT("erlbuild_pt_util: start"),
    rewrite(Forms).


% "Abstract Format" chapter from ERTS User Guide:
% http://www.erlang.org/doc/apps/erts/absform.html

rewrite(Forms) ->
    % This function call returns the same AST but with the record definitions replaced with tuples (this is the same way
    % the normal Erlang compiler does this). We need the expanded form for rewriting const functions, since the eval
    % function doesn't work when there are records inside the expression. We need the unexpanded form, because if we
    % substitute record definitions at this stage, the Erlang compiler (erlc) will throw warnings saying that record
    % definitions are unused. So, we zip the two forms together, and only use the record-expanded form when we are
    % rewriting a const function.
    ExpandedForms = erl_expand_records:module(Forms, []),
    Forms2 = lists:zip(Forms, ExpandedForms),
    rewrite(Forms2, _Consts=ordsets:new(), _Accu = []).

-spec rewrite([{form(), form()}], const_names(), [form()]) -> [form()].

rewrite([{F, E}|T], Consts, Accu) ->
    {NewForm, NewConsts} = rewrite_maybe_const_form(F, E, Consts),
    rewrite(T, NewConsts, [NewForm | Accu]);

rewrite([], Consts=[_|_], _Accu) ->
    % end of input when we haven't seen one or more consts
    MissingFuns = [list_to_atom(lists:concat([N, "/0"])) || N <- Consts],
    exit({missing_const_funs, MissingFuns});

rewrite([], [], Accu) ->
    % end of input
    lists:reverse(Accu).


-spec rewrite_maybe_const_form(form(), form(), const_names()) -> {form(), const_names()}.
%% rewrite a single form that might add or remove a const name
rewrite_maybe_const_form({attribute, _LINE, const, Names} = Form, _ExpandedForm, Consts) ->
    case is_list(Names) andalso lists:all(fun erlang:is_atom/1, Names) of
        false -> exit({bad_const_param, Names});
        true -> {Form, ordsets:union(ordsets:from_list(Names), Consts)}
    end;
% Here we are actually rewriting a const function, so we substitute the record-expanded form that we zipped together
% earlier, into rewrite_const_thunk().
rewrite_maybe_const_form(Form,
   {function, LINE, Name, 0, [{clause, _, [], [], Exprs}]} = _ExpandedForm, Consts) ->
    case ordsets:is_element(Name, Consts) of
        true ->
            {rewrite_const_thunk(LINE, {Name, Exprs}),
             ordsets:del_element(Name, Consts)};
        false ->
            {rewrite_form(Form), Consts}
    end;
rewrite_maybe_const_form(Form, _ExpandedForm, Consts) -> {rewrite_form(Form), Consts}.

rewrite_form(Form) -> Form.


%% Convert a name and list of expressions into a zero-ary function returning
%% the precompiled value of the expressions.
%% May exit abruptly with {unsafe_in_const_fun, X} if one of the expressions
%% uses a side-effecting function.
rewrite_const_thunk(LINE, {Name, Exprs}) ->
    {value, Term, _} = limited_eval(Exprs),
    Expr = term_to_ast(LINE, Term),
    {function, LINE, Name, _Arity = 0,
     [{clause, LINE, _Args = [], _Guards = [], [Expr]}]}.


term_to_ast(LINE, X) when is_atom(X) -> {atom, LINE, X};
term_to_ast(LINE, X) when is_integer(X) -> {integer, LINE, X};
term_to_ast(LINE, X) when is_float(X) -> {float, LINE, X};
term_to_ast(LINE, T) when is_tuple(T) ->
    {tuple, LINE, [term_to_ast(LINE, X) || X <- tuple_to_list(T)]};
term_to_ast(LINE, []) -> {nil, LINE};
term_to_ast(LINE, L = [H|T]) when is_list(L) ->
    {cons, LINE, term_to_ast(LINE, H), term_to_ast(LINE, T)};
term_to_ast(LINE, M) when is_map(M) ->
    Folded = maps:fold(fun(K, V, Acc) ->
                               [{map_field_assoc, LINE, term_to_ast(LINE, K), term_to_ast(LINE, V)} | Acc]
                       end,
                       [],
                       M),
    {map, LINE, Folded};
term_to_ast(LINE, B) when is_binary(B) ->
    BinElements = [
        {bin_element, LINE, {integer, LINE, X}, default, default}
        ||
        X <- binary_to_list(B)
    ],
    {bin, LINE, BinElements}.


%% Find the value of a sequence of expressions, limiting the functions that may
%% be called to known-safe (side-effect-free) functions.
limited_eval(Exprs) ->
    erl_eval:exprs(
      Exprs,
      _Bindings=[],
      _LocalHandler=none,
      _NonLocalHandler={value, fun limited_apply/2}).

%% Apply a known-safe function to a list of args, or exit abruptly if
%% the function is not known-safe.
limited_apply({Module, Name}, Args) ->
    Arity = length(Args),
    case is_constant_safe(Module, Name, Arity) of
        false -> exit({unsafe_in_const_fun, {Module, Name, Arity}});
        true -> apply(Module, Name, Args)
    end.

%% Determine whether a function defined by module/name/arity is safe for use
%% in constants.
%% We hand-pick a few library functions we know are safe and the BIFs that the
%% compiler treats as pure for its own optimizations.
is_constant_safe(_Module, _Name, _Arity) -> true.
