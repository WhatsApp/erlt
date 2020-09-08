%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2020. All Rights Reserved.
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
%%
%% %CopyrightEnd%
%%
-module(erlt_pp).

%%% Pretty printer for Erlang code in the same format as returned from
%%% the parser. It does not always produce pretty code.
-export([
    form/1,
    form/2,
    attribute/1,
    attribute/2,
    function/1,
    function/2,
    guard/1,
    guard/2,
    exprs/1,
    exprs/2,
    exprs/3,
    expr/1,
    expr/2,
    expr/3,
    expr/4
]).

-import(lists, [append/1, foldr/3, map/2, mapfoldl/3, reverse/1, reverse/2]).

-import(io_lib, [write/1, format/2]).

-import(erlt_parse, [
    inop_prec/1,
    preop_prec/1,
    func_prec/0,
    max_prec/0,
    type_inop_prec/1,
    type_preop_prec/1
]).

-define(MAXLINE, 72).

-type hook_function() ::
    none |
    fun(
        (
            Expr :: erlt_parse:abstract_expr(),
            CurrentIndentation :: integer(),
            CurrentPrecedence :: non_neg_integer(),
            Options :: options()
        ) -> io_lib:chars()
    ).

-type option() ::
    {hook, hook_function()} |
    {encoding, latin1 | unicode | utf8} |
    {quote_singleton_atom_types, boolean()}.

-type options() :: hook_function() | [option()].

-record(pp, {value_fun, singleton_atom_type_fun, string_fun, char_fun}).

-record(options, {hook, encoding, opts}).

%-define(DEBUG, true).
-ifdef(DEBUG).

-define(FORM_TEST(T),
    _ =
        case T of
            {eof, _Line} -> ok;
            {warning, _W} -> ok;
            {error, _E} -> ok;
            _ -> ?TEST(T)
        end
).

-define(EXPRS_TEST(L), _ = [?TEST(E) || E <- L]).

-define(TEST(T),
    %% Assumes that erl_anno has been compiled with DEBUG=true.
    %% erl_pp does not use the annoations, but test it anyway.
    %% Note: hooks are not handled.
    _ =
        try
            erlt_parse:map_anno(fun(A) when is_list(A) -> A end, T)
        catch
            _:_ ->
                erlang:error(badarg, [T])
        end
).

-else.

-define(FORM_TEST(T), ok).

-define(EXPRS_TEST(T), ok).

-define(TEST(T), ok).

-endif.

%%%
%%% Exported functions
%%%
-spec form(Form) -> io_lib:chars() when Form :: erlt_parse:abstract_form() | erlt_parse:form_info().
form(Thing) ->
    form(Thing, none).

-spec form(Form, Options) -> io_lib:chars() when
    Form :: erlt_parse:abstract_form() | erlt_parse:form_info(), Options :: options().
form(Thing, Options) ->
    ?FORM_TEST(Thing),
    State = state(Options),
    frmt(lform(Thing, options(Options)), State).

-spec attribute(Attribute) -> io_lib:chars() when Attribute :: erlt_parse:abstract_form().
attribute(Thing) ->
    attribute(Thing, none).

-spec attribute(Attribute, Options) -> io_lib:chars() when
    Attribute :: erlt_parse:abstract_form(), Options :: options().
attribute(Thing, Options) ->
    ?TEST(Thing),
    State = state(Options),
    frmt(lattribute(Thing, options(Options)), State).

-spec function(Function) -> io_lib:chars() when Function :: erlt_parse:abstract_form().
function(F) ->
    function(F, none).

-spec function(Function, Options) -> io_lib:chars() when
    Function :: erlt_parse:abstract_form(), Options :: options().
function(F, Options) ->
    ?TEST(F),
    frmt(lfunction(F, options(Options)), state(Options)).

-spec guard(Guard) -> io_lib:chars() when Guard :: [erlt_parse:abstract_expr()].
guard(Gs) ->
    guard(Gs, none).

-spec guard(Guard, Options) -> io_lib:chars() when
    Guard :: [erlt_parse:abstract_expr()], Options :: options().
guard(Gs, Options) ->
    ?EXPRS_TEST(Gs),
    frmt(lguard(Gs, options(Options)), state(Options)).

-spec exprs(Expressions) -> io_lib:chars() when Expressions :: [erlt_parse:abstract_expr()].
exprs(Es) ->
    exprs(Es, 0, none).

-spec exprs(Expressions, Options) -> io_lib:chars() when
    Expressions :: [erlt_parse:abstract_expr()], Options :: options().
exprs(Es, Options) ->
    exprs(Es, 0, Options).

-spec exprs(Expressions, Indent, Options) -> io_lib:chars() when
    Expressions :: [erlt_parse:abstract_expr()], Indent :: integer(), Options :: options().
exprs(Es, I, Options) ->
    ?EXPRS_TEST(Es),
    frmt({seq, [], [], [$,], lexprs(Es, options(Options))}, I, state(Options)).

-spec expr(Expression) -> io_lib:chars() when Expression :: erlt_parse:abstract_expr().
expr(E) ->
    ?TEST(E),
    frmt(lexpr(E, 0, options(none)), state(none)).

-spec expr(Expression, Options) -> io_lib:chars() when
    Expression :: erlt_parse:abstract_expr(), Options :: options().
expr(E, Options) ->
    ?TEST(E),
    frmt(lexpr(E, 0, options(Options)), state(Options)).

-spec expr(Expression, Indent, Options) -> io_lib:chars() when
    Expression :: erlt_parse:abstract_expr(), Indent :: integer(), Options :: options().
expr(E, I, Options) ->
    ?TEST(E),
    frmt(lexpr(E, 0, options(Options)), I, state(Options)).

-spec expr(Expression, Indent, Precedence, Options) -> io_lib:chars() when
    Expression :: erlt_parse:abstract_expr(),
    Indent :: integer(),
    Precedence :: non_neg_integer(),
    Options :: options().
expr(E, I, P, Options) ->
    ?TEST(E),
    frmt(lexpr(E, P, options(Options)), I, state(Options)).

%%%
%%% Local functions
%%%
options(Options) when is_list(Options) ->
    Hook = proplists:get_value(hook, Options, none),
    Encoding = encoding(Options),
    #options{hook = Hook, encoding = Encoding, opts = Options};
options(Hook) ->
    #options{hook = Hook, encoding = encoding([]), opts = Hook}.

state(Options) when is_list(Options) ->
    Quote = proplists:get_bool(quote_singleton_atom_types, Options),
    case encoding(Options) of
        latin1 -> latin1_state(Quote);
        unicode -> unicode_state(Quote)
    end;
state(_Hook) ->
    latin1_state(false).

latin1_state(Quote) ->
    Options = [{encoding, latin1}],
    ValueFun = fun(V) -> io_lib_pretty:print(V, Options) end,
    SingletonFun =
        case Quote of
            true ->
                fun(A) ->
                    io_lib:write_string_as_latin1(atom_to_list(A), $')
                %'
                end;
            false ->
                ValueFun
        end,
    #pp{
        value_fun = ValueFun,
        singleton_atom_type_fun = SingletonFun,
        string_fun = fun io_lib:write_string_as_latin1/1,
        char_fun = fun io_lib:write_char_as_latin1/1
    }.

unicode_state(Quote) ->
    Options = [{encoding, unicode}],
    ValueFun = fun(V) -> io_lib_pretty:print(V, Options) end,
    SingletonFun =
        case Quote of
            true ->
                %'
                fun(A) -> io_lib:write_string(atom_to_list(A), $') end;
            false ->
                ValueFun
        end,
    #pp{
        value_fun = ValueFun,
        singleton_atom_type_fun = SingletonFun,
        string_fun = fun io_lib:write_string/1,
        char_fun = fun io_lib:write_char/1
    }.

encoding(Options) ->
    case proplists:get_value(encoding, Options, epp:default_encoding()) of
        latin1 -> latin1;
        utf8 -> unicode;
        unicode -> unicode
    end.

lform({attribute, Line, Name, Arg}, Opts) ->
    lattribute({attribute, Line, Name, Arg}, Opts);
lform({function, Line, Name, Arity, Clauses}, Opts) ->
    lfunction({function, Line, Name, Arity, Clauses}, Opts);
%% These are specials to make it easier for the compiler.
lform({error, _} = E, Opts) ->
    message(E, Opts);
lform({warning, _} = W, Opts) ->
    message(W, Opts);
lform({eof, _Line}, _Opts) ->
    $\n.

message(M, #options{encoding = Encoding}) ->
    F =
        case Encoding of
            latin1 -> "~p\n";
            unicode -> "~tp\n"
        end,
    leaf(format(F, [M])).

lattribute({attribute, _Line, type, Type}, Opts) ->
    [typeattr(type, Type, Opts), leaf(".\n")];
lattribute({attribute, _Line, opaque, Type}, Opts) ->
    [typeattr(opaque, Type, Opts), leaf(".\n")];
lattribute({attribute, _Line, enum, Type}, Opts) ->
    [typeattr(enum, Type, Opts), leaf(".\n")];
lattribute({attribute, _Line, struct, Type}, Opts) ->
    [typeattr(struct, Type, Opts), leaf(".\n")];
lattribute({attribute, _Line, spec, Arg}, _Opts) ->
    [specattr(spec, Arg), leaf(".\n")];
lattribute({attribute, _Line, callback, Arg}, _Opts) ->
    [specattr(callback, Arg), leaf(".\n")];
lattribute({attribute, _Line, Name, Arg}, Opts) ->
    [lattribute(Name, Arg, Opts), leaf(".\n")].

lattribute(module, {M, Vs}, _Opts) ->
    A = a0(),
    attr(module, [
        {var, A, pname(M)},
        foldr(fun(V, C) -> {cons, A, {var, A, V}, C} end, {nil, A}, Vs)
    ]);
lattribute(module, M, _Opts) ->
    attr(module, [{var, a0(), pname(M)}]);
lattribute(export, Falist, _Opts) ->
    attrib(export, falist(Falist));
lattribute(import, Name, _Opts) when is_list(Name) ->
    attr(import, [{var, a0(), pname(Name)}]);
lattribute(import, {From, Falist}, _Opts) ->
    attrib(import, [leaf(pname(From)), falist(Falist)]);
lattribute(export_type, Talist, _Opts) ->
    attrib(export_type, falist(Talist));
lattribute(optional_callbacks, Falist, Opts) ->
    try
        attrib(optional_callbacks, falist(Falist))
    catch
        _:_ -> attr(optional_callbacks, [abstract(Falist, Opts)])
    end;
lattribute(file, {Name, Line}, _Opts) ->
    attr(file, [{string, a0(), Name}, {integer, a0(), Line}]);
lattribute(record, {Name, Is}, Opts) ->
    Nl = [leaf("-record("), {atom, Name}, $,],
    [{first, Nl, record_fields(Is, Opts)}, $)];
lattribute(Name, Arg, Options) ->
    attr(Name, [abstract(Arg, Options)]).

abstract(Arg, #options{encoding = Encoding}) ->
    erlt_parse:abstract(Arg, [{encoding, Encoding}]).

typeattr(struct, {TypeName, Type, []}, _Opts) ->
    {first, leaf("-struct "), typed(atom_to_list(TypeName), Type)};
typeattr(Tag, {TypeName, Type, Args}, _Opts) ->
    {first, leaf("-" ++ atom_to_list(Tag) ++ " "),
        typed(call({atom, a0(), TypeName}, Args, 0, options(none)), Type)}.

ltype(T) ->
    ltype(T, 0).

ltype({ann_type, _Line, [V, T]}, Prec) ->
    {L, P, R} = type_inop_prec('::'),
    Vl = ltype(V, L),
    Tr = ltype(T, R),
    El = {list, [{cstep, [Vl, ' ::'], Tr}]},
    maybe_paren(P, Prec, El);
ltype({paren_type, _Line, [T]}, P) ->
    %% Generated before Erlang/OTP 18.
    ltype(T, P);
ltype({type, _Line, union, Ts}, Prec) ->
    {_L, P, R} = type_inop_prec('|'),
    E = {seq, [], [], [' |'], ltypes(Ts, R)},
    maybe_paren(P, Prec, E);
ltype({type, _Line, list, [T]}, _) ->
    {seq, $[, $], $,, [ltype(T)]};
ltype({type, _Line, nonempty_list, [T]}, _) ->
    {seq, $[, $], [$,], [ltype(T), leaf("...")]};
ltype({type, Line, nil, []}, _) ->
    lexpr({nil, Line}, options(none));
ltype({type, Line, map, any}, _) ->
    simple_type({atom, Line, map}, []);
ltype({type, _Line, map, Pairs}, Prec) ->
    {P, _R} = type_preop_prec('#'),
    E = map_type(Pairs),
    maybe_paren(P, Prec, E);
ltype({type, Line, tuple, any}, _) ->
    simple_type({atom, Line, tuple}, []);
ltype({type, _Line, tuple, Ts}, _) ->
    tuple_type(Ts, fun ltype/2);
ltype({type, _Line, record, [{atom, _, N} | Fs]}, Prec) ->
    {P, _R} = type_preop_prec('#'),
    E = record_type(N, Fs),
    maybe_paren(P, Prec, E);
ltype({type, _Line, range, [_I1, _I2] = Es}, Prec) ->
    {_L, P, R} = type_inop_prec('..'),
    F = fun(E, Opts) -> lexpr(E, R, Opts) end,
    E = expr_list(Es, '..', F, options(none)),
    maybe_paren(P, Prec, E);
ltype({type, _Line, binary, [I1, I2]}, _) ->
    % except binary()
    binary_type(I1, I2);
ltype({type, _Line, 'fun', []}, _) ->
    leaf("fun()");
ltype({type, _, 'fun', [{type, _, any}, _]} = FunType, _) ->
    [fun_type(['fun', $(], FunType), $)];
ltype({type, _Line, 'fun', [{type, _, product, _}, _]} = FunType, _) ->
    [fun_type(['fun', $(], FunType), $)];
ltype({type, _Line, enum, Tag, Vars}, _) ->
    {first, lexpr(Tag, options(none)), tuple_type(Vars, fun ltype/2)};
ltype({type, _Line, struct, _Name, Fields}, _) ->
    {seq, $(, $), [$,], struct_fields(Fields, "::", fun ltype/2, 0)};
ltype({type, Line, T, Ts}, _) ->
    simple_type({atom, Line, T}, Ts);
ltype({user_type, Line, T, Ts}, _) ->
    simple_type({atom, Line, T}, Ts);
ltype({remote_type, Line, [M, F, Ts]}, _) ->
    simple_type({remote, Line, M, F}, Ts);
ltype({atom, _, T}, _) ->
    {singleton_atom_type, T};
ltype(E, P) ->
    lexpr(E, P, options(none)).

binary_type(I1, I2) ->
    B = [[] || {integer, _, 0} <- [I1]] =:= [],
    U = [[] || {integer, _, 0} <- [I2]] =:= [],
    P = max_prec(),
    E1 = [[leaf("_:"), lexpr(I1, P, options(none))] || B],
    E2 = [[leaf("_:_*"), lexpr(I2, P, options(none))] || U],
    case E1 ++ E2 of
        [] ->
            leaf("<<>>");
        Es ->
            {seq, '<<', '>>', [$,], Es}
    end.

map_type(Fs) ->
    {first, [$#], map_pair_types(Fs)}.

map_pair_types(Fs) ->
    tuple_type(Fs, fun map_pair_type/2).

map_pair_type({type, _Line, map_field_assoc, [KType, VType]}, Prec) ->
    {list, [{cstep, [ltype(KType, Prec), leaf(" =>")], ltype(VType, Prec)}]};
map_pair_type({type, _Line, map_field_exact, [KType, VType]}, Prec) ->
    {list, [{cstep, [ltype(KType, Prec), leaf(" :=")], ltype(VType, Prec)}]}.

record_type(Name, Fields) ->
    {first, [record_name(Name)], field_types(Fields)}.

field_types(Fs) ->
    tuple_type(Fs, fun field_type/2).

field_type({type, _Line, field_type, [Name, Type]}, _Prec) ->
    typed(lexpr(Name, options(none)), Type).

typed(B, Type) ->
    {list, [{cstep, [B, ' ::'], ltype(Type)}]}.

tuple_type([], _) ->
    leaf("{}");
tuple_type(Ts, F) ->
    {seq, ${, $}, [$,], ltypes(Ts, F, 0)}.

specattr(SpecKind, {FuncSpec, TypeSpecs}) ->
    Func =
        case FuncSpec of
            {F, _A} ->
                {atom, F};
            {M, F, _A} ->
                [{atom, M}, $:, {atom, F}]
        end,
    {first, leaf(lists:concat(["-", SpecKind, " "])),
        {list, [{first, Func, spec_clauses(TypeSpecs)}]}}.

spec_clauses(TypeSpecs) ->
    {prefer_nl, [$;], [sig_type(T) || T <- TypeSpecs]}.

sig_type({type, _Line, bounded_fun, [T, Gs]}) ->
    guard_type(fun_type([], T), Gs);
sig_type(FunType) ->
    fun_type([], FunType).

guard_type(Before, Gs) ->
    Opts = options(none),
    Gl = {list, [{step, 'when', expr_list(Gs, [$,], fun constraint/2, Opts)}]},
    {list, [{step, Before, Gl}]}.

constraint(
    {type, _Line, constraint, [{atom, _, is_subtype}, [{var, _, _} = V, Type]]},
    _Opts
) ->
    typed(lexpr(V, options(none)), Type);
constraint({type, _Line, constraint, [Tag, As]}, _Opts) ->
    simple_type(Tag, As).

fun_type(Before, {type, _, 'fun', [FType, Ret]}) ->
    {first, Before, {step, [type_args(FType), ' ->'], ltype(Ret)}}.

type_args({type, _Line, any}) ->
    leaf("(...)");
type_args({type, _line, product, Ts}) ->
    targs(Ts).

simple_type(Tag, Types) ->
    {first, lexpr(Tag, options(none)), targs(Types)}.

targs(Ts) ->
    {seq, $(, $), [$,], ltypes(Ts, 0)}.

ltypes(Ts, Prec) ->
    ltypes(Ts, fun ltype/2, Prec).

ltypes(Ts, F, Prec) ->
    [F(T, Prec) || T <- Ts].

struct_fields(Fields, Join, F, Prec) ->
    [
        {first, [F(Name, Prec), " " ++ Join ++ " "], F(Value, Prec)}
        || {struct_field, _, Name, Value} <- Fields
    ].

attr(Name, Args) ->
    {first, [$-, {atom, Name}], args(Args, options(none))}.

attrib(Name, Args) ->
    {first, [$-, {atom, Name}], [{seq, $(, $), [$,], Args}]}.

pname(['' | As]) ->
    [$. | pname(As)];
pname([A]) ->
    write(A);
pname([A | As]) ->
    [write(A), $. | pname(As)];
pname(A) when is_atom(A) ->
    write(A).

falist([]) ->
    ['[]'];
falist(Falist) ->
    L = [
        begin
            {Name, Arity} = Fa,
            [{atom, Name}, leaf(format("/~w", [Arity]))]
        end
        || Fa <- Falist
    ],
    [{seq, $[, $], $,, L}].

lfunction({function, _Line, Name, _Arity, Cs}, Opts) ->
    Cll = nl_clauses(fun(C, H) -> func_clause(Name, C, H) end, $;, Opts, Cs),
    [Cll, leaf(".\n")].

func_clause(Name, {clause, Line, Head, Guard, Body}, Opts) ->
    Hl = call({atom, Line, Name}, Head, 0, Opts),
    Gl = guard_when(Hl, Guard, Opts),
    Bl = body(Body, Opts),
    {step, Gl, Bl}.

guard_when(Before, Guard, Opts) ->
    guard_when(Before, Guard, Opts, ' ->').

guard_when(Before, Guard, Opts, After) ->
    Gl = lguard(Guard, Opts),
    [{list, [{step, Before, Gl}]}, After].

lguard([E | Es], Opts) when is_list(E) ->
    {list, [{step, 'when', expr_list([E | Es], [$;], fun guard0/2, Opts)}]};
% before R6
lguard([E | Es], Opts) ->
    lguard([[E | Es]], Opts);
lguard([], _) ->
    [].

guard0(Es, Opts) ->
    expr_list(Es, [$,], fun lexpr/2, Opts).

%% body(Before, Es, Opts) -> [Char].
body([E], Opts) ->
    lexpr(E, Opts);
body(Es, Opts) ->
    {prefer_nl, [$,], lexprs(Es, Opts)}.

lexpr(E, Opts) ->
    lexpr(E, 0, Opts).

%Special hack for Robert
lexpr({var, _, V}, _, _) when is_integer(V) ->
    leaf(format("_~w", [V]));
lexpr({var, _, V}, _, _) ->
    leaf(format("~ts", [V]));
lexpr({char, _, C}, _, _) ->
    {char, C};
lexpr({integer, _, N}, _, _) ->
    leaf(write(N));
lexpr({float, _, F}, _, _) ->
    leaf(write(F));
lexpr({atom, _, A}, _, _) ->
    {atom, A};
lexpr({string, _, S}, _, _) ->
    {string, S};
lexpr({nil, _}, _, _) ->
    '[]';
lexpr({cons, _, H, T}, _, Opts) ->
    list(T, [H], Opts);
lexpr({lc, _, E, Qs}, _Prec, Opts) ->
    P = max_prec(),
    Lcl = {list, [{step, [lexpr(E, P, Opts), leaf(" ||")], lc_quals(Qs, Opts)}]},
    {list, [{seq, $[, [], [[]], [{force_nl, leaf(" "), [Lcl]}]}, $]]};
%% {list,[{step,$[,Lcl},$]]};
lexpr({bc, _, E, Qs}, _Prec, Opts) ->
    P = max_prec(),
    Lcl = {list, [{step, [lexpr(E, P, Opts), leaf(" ||")], lc_quals(Qs, Opts)}]},
    {list, [{seq, '<<', [], [[]], [{force_nl, leaf(" "), [Lcl]}]}, '>>']};
%% {list,[{step,'<<',Lcl},'>>']};
lexpr({tuple, _, Elts}, _, Opts) ->
    tuple(Elts, Opts);
lexpr({enum, _, C, Elts}, _, Opts) ->
    {first, lexpr(C, Opts), tuple(Elts, Opts)};
%%lexpr({struct,_,Tag,Elts}, _, Opts) ->
%%  {first,format("~w", [Tag]),tuple(Elts, Opts)};
lexpr({record_index, _, Name, F}, Prec, Opts) ->
    {P, R} = preop_prec('#'),
    Nl = record_name(Name),
    El = [Nl, $., lexpr(F, R, Opts)],
    maybe_paren(P, Prec, El);
lexpr({record, _, Name, Fs}, Prec, Opts) ->
    {P, _R} = preop_prec('#'),
    Nl = record_name(Name),
    El = {first, Nl, record_fields(Fs, Opts)},
    maybe_paren(P, Prec, El);
lexpr({record_field, _, Rec, Name, F}, Prec, Opts) ->
    {L, P, R} = inop_prec('#'),
    Rl = lexpr(Rec, L, Opts),
    Sep = hash_after_integer(Rec, [$#]),
    Nl = [Sep, {atom, Name}, $.],
    El = [Rl, Nl, lexpr(F, R, Opts)],
    maybe_paren(P, Prec, El);
lexpr({record, _, Rec, Name, Fs}, Prec, Opts) ->
    {L, P, _R} = inop_prec('#'),
    Rl = lexpr(Rec, L, Opts),
    Sep = hash_after_integer(Rec, []),
    Nl = record_name(Name),
    El = {first, [Rl, Sep, Nl], record_fields(Fs, Opts)},
    maybe_paren(P, Prec, El);
lexpr({record_field, _, {atom, _, ''}, F}, Prec, Opts) ->
    {_L, P, R} = inop_prec('.'),
    El = [$., lexpr(F, R, Opts)],
    maybe_paren(P, Prec, El);
lexpr({record_field, _, Rec, F}, Prec, Opts) ->
    {L, P, R} = inop_prec('.'),
    El = [lexpr(Rec, L, Opts), $., lexpr(F, R, Opts)],
    maybe_paren(P, Prec, El);
lexpr({map, _, Fs}, Prec, Opts) ->
    {P, _R} = preop_prec('#'),
    El = {first, $#, map_fields(Fs, Opts)},
    maybe_paren(P, Prec, El);
lexpr({map, _, Map, Fs}, Prec, Opts) ->
    {L, P, _R} = inop_prec('#'),
    Rl = lexpr(Map, L, Opts),
    Sep = hash_after_integer(Map, [$#]),
    El = {first, [Rl | Sep], map_fields(Fs, Opts)},
    maybe_paren(P, Prec, El);
lexpr({block, _, Es}, _, Opts) ->
    {list, [{step, 'begin', body(Es, Opts)}, {reserved, 'end'}]};
lexpr({'if', _, Cs}, _, Opts) ->
    {list, [{step, 'if', if_clauses(Cs, Opts)}, {reserved, 'end'}]};
lexpr({'case', _, Expr, Cs}, _, Opts) ->
    {list, [
        {step, {list, [{step, 'case', lexpr(Expr, Opts)}, {reserved, 'of'}]}, cr_clauses(Cs, Opts)},
        {reserved, 'end'}
    ]};
lexpr({'cond', _, Cs}, _, Opts) ->
    {list, [{step, leaf("cond"), cond_clauses(Cs, Opts)}, {reserved, 'end'}]};
lexpr({'receive', _, Cs}, _, Opts) ->
    {list, [{step, 'receive', cr_clauses(Cs, Opts)}, {reserved, 'end'}]};
lexpr({'receive', _, Cs, To, ToOpt}, _, Opts) ->
    Al = {list, [{step, [lexpr(To, Opts), ' ->'], body(ToOpt, Opts)}]},
    {list, [
        {step, 'receive', cr_clauses(Cs, Opts)},
        {step, 'after', Al},
        {reserved, 'end'}
    ]};
lexpr({'fun', _, {function, F, A}}, _Prec, _Opts) ->
    [leaf("fun "), {atom, F}, leaf(format("/~w", [A]))];
lexpr({'fun', L, {function, _, _} = Func, Extra}, Prec, Opts) ->
    {force_nl, fun_info(Extra), lexpr({'fun', L, Func}, Prec, Opts)};
lexpr({'fun', L, {function, M, F, A}}, Prec, Opts) when is_atom(M), is_atom(F), is_integer(A) ->
    %% For backward compatibility with pre-R15 abstract format.
    Mod = erlt_parse:abstract(M),
    Fun = erlt_parse:abstract(F),
    Arity = erlt_parse:abstract(A),
    lexpr({'fun', L, {function, Mod, Fun, Arity}}, Prec, Opts);
lexpr({'fun', _, {function, M, F, A}}, _Prec, Opts) ->
    %% New format in R15.
    NameItem = lexpr(M, Opts),
    CallItem = lexpr(F, Opts),
    ArityItem = lexpr(A, Opts),
    ["fun ", NameItem, $:, CallItem, $/, ArityItem];
lexpr({'fun', _, {clauses, Cs}}, _Prec, Opts) ->
    {list, [{first, 'fun', fun_clauses(Cs, Opts, unnamed)}, {reserved, 'end'}]};
lexpr({named_fun, _, Name, Cs}, _Prec, Opts) ->
    {list, [{first, ['fun', " "], fun_clauses(Cs, Opts, {named, Name})}, {reserved, 'end'}]};
lexpr({'fun', _, {clauses, Cs}, Extra}, _Prec, Opts) ->
    {force_nl, fun_info(Extra),
        {list, [{first, 'fun', fun_clauses(Cs, Opts, unnamed)}, {reserved, 'end'}]}};
lexpr({named_fun, _, Name, Cs, Extra}, _Prec, Opts) ->
    {force_nl, fun_info(Extra),
        {list, [
            {first, ['fun', " "], fun_clauses(Cs, Opts, {named, Name})},
            {reserved, 'end'}
        ]}};
lexpr({call, _, {remote, _, {atom, _, M}, {atom, _, F} = N} = Name, Args}, Prec, Opts) ->
    case erl_internal:bif(M, F, length(Args)) of
        true ->
            call(N, Args, Prec, Opts);
        false ->
            call(Name, Args, Prec, Opts)
    end;
lexpr({call, _, Name, Args}, Prec, Opts) ->
    call(Name, Args, Prec, Opts);
lexpr({'try', _, Es, Scs, Ccs, As}, _, Opts) ->
    {list,
        [
            if
                Scs =:= [] ->
                    {step, 'try', body(Es, Opts)};
                true ->
                    {step, {list, [{step, 'try', body(Es, Opts)}, {reserved, 'of'}]},
                        cr_clauses(Scs, Opts)}
            end
        ] ++
            if
                Ccs =:= [] ->
                    [];
                true ->
                    [{step, 'catch', try_clauses(Ccs, Opts)}]
            end ++
            if
                As =:= [] ->
                    [];
                true ->
                    [{step, 'after', body(As, Opts)}]
            end ++
            [{reserved, 'end'}]};
lexpr({'catch', _, Expr}, Prec, Opts) ->
    {P, R} = preop_prec('catch'),
    El = {list, [{step, 'catch', lexpr(Expr, R, Opts)}]},
    maybe_paren(P, Prec, El);
lexpr({match, _, Lhs, Rhs}, Prec, Opts) ->
    {L, P, R} = inop_prec('='),
    Pl = lexpr(Lhs, L, Opts),
    Rl = lexpr(Rhs, R, Opts),
    El = {list, [{cstep, [Pl, ' ='], Rl}]},
    maybe_paren(P, Prec, El);
lexpr({op, _, Op, Arg}, Prec, Opts) when Op =:= '^' ->
    %% no space after caret
    {P, R} = preop_prec(Op),
    Ol = {reserved, leaf(format("~s", [Op]))},
    El = [Ol, lexpr(Arg, R, Opts)],
    maybe_paren(P, Prec, El);
lexpr({op, _, Op, Arg}, Prec, Opts) ->
    {P, R} = preop_prec(Op),
    Ol = {reserved, leaf(format("~s ", [Op]))},
    El = [Ol, lexpr(Arg, R, Opts)],
    maybe_paren(P, Prec, El);
lexpr({op, _, '.', Larg, Rarg}, Prec, Opts) ->
    %% no spaces around dots
    {L, P, R} = inop_prec('.'),
    Ll = lexpr(Larg, L, Opts),
    Lr = lexpr(Rarg, R, Opts),
    maybe_paren(P, Prec, [Ll, $., Lr]);
lexpr({op, _, Op, Larg, Rarg}, Prec, Opts) when Op =:= 'orelse'; Op =:= 'andalso' ->
    %% Breaks lines since R12B.
    {L, P, R} = inop_prec(Op),
    Ll = lexpr(Larg, L, Opts),
    Ol = {reserved, leaf(format("~s", [Op]))},
    Lr = lexpr(Rarg, R, Opts),
    El = {prefer_nl, [[]], [Ll, Ol, Lr]},
    maybe_paren(P, Prec, El);
lexpr({op, _, Op, Larg, Rarg}, Prec, Opts) ->
    {L, P, R} = inop_prec(Op),
    Ll = lexpr(Larg, L, Opts),
    Ol = {reserved, leaf(format("~s", [Op]))},
    Lr = lexpr(Rarg, R, Opts),
    El = {list, [Ll, Ol, Lr]},
    maybe_paren(P, Prec, El);
%% Special expressions which are not really legal everywhere.
lexpr({remote, _, M, F}, Prec, Opts) ->
    {L, P, R} = inop_prec(':'),
    NameItem = lexpr(M, L, Opts),
    CallItem = lexpr(F, R, Opts),
    maybe_paren(P, Prec, [NameItem, $:, CallItem]);
%% BIT SYNTAX:
lexpr({bin, _, Fs}, _, Opts) ->
    bit_grp(Fs, Opts);
%% Special case for straight values.
lexpr({value, _, Val}, _, _) ->
    {value, Val};
%% Now do the hook.
lexpr(Other, _Precedence, #options{hook = none}) ->
    leaf(format("INVALID-FORM:~w:", [Other]));
lexpr(HookExpr, Precedence, #options{hook = {Mod, Func, Eas}}) when Mod =/= 'fun' ->
    {ehook, HookExpr, Precedence, {Mod, Func, Eas}};
lexpr(HookExpr, Precedence, #options{hook = Func, opts = Options}) ->
    {hook, HookExpr, Precedence, Func, Options}.

%% An integer is separated from the following '#' by a space, which
%% erl_scan can handle.
hash_after_integer({integer, _, _}, C) ->
    [$\s | C];
hash_after_integer({'fun', _, {function, _, _}}, C) ->
    [$\s | C];
hash_after_integer({'fun', _, {function, _, _, _}}, C) ->
    [$\s | C];
hash_after_integer(_, C) ->
    C.

call(Name, Args, Prec, Opts) ->
    {F, P} = func_prec(),
    Item = {first, lexpr(Name, F, Opts), args(Args, Opts)},
    maybe_paren(P, Prec, Item).

fun_info(Extra) ->
    [leaf("% fun-info: "), {value, Extra}].

%% BITS:
bit_grp([], _Opts) ->
    leaf("<<>>");
bit_grp(Fs, Opts) ->
    append([['<<'], [bit_elems(Fs, Opts)], ['>>']]).

bit_elems(Es, Opts) ->
    expr_list(Es, $,, fun bit_elem/2, Opts).

bit_elem({bin_element, _, Expr, Sz, Types}, Opts) ->
    P = max_prec(),
    VChars = lexpr(Expr, P, Opts),
    SChars =
        if
            Sz =/= default ->
                [VChars, $:, lexpr(Sz, P, Opts)];
            true ->
                VChars
        end,
    if
        Types =/= default ->
            [SChars, $/ | bit_elem_types(Types)];
        true ->
            SChars
    end.

bit_elem_types([T]) ->
    [bit_elem_type(T)];
bit_elem_types([T | Rest]) ->
    [bit_elem_type(T), $- | bit_elem_types(Rest)].

bit_elem_type({A, B}) ->
    [
        lexpr(erlt_parse:abstract(A), options(none)),
        $:,
        lexpr(erlt_parse:abstract(B), options(none))
    ];
bit_elem_type(T) ->
    lexpr(erlt_parse:abstract(T), options(none)).

%% end of BITS
record_name(Name) ->
    [$#, {atom, Name}].

record_fields(Fs, Opts) ->
    tuple(Fs, fun record_field/2, Opts).

record_field({record_field, _, F, Val}, Opts) ->
    {L, _P, R} = inop_prec('='),
    Fl = lexpr(F, L, Opts),
    Vl = lexpr(Val, R, Opts),
    {list, [{cstep, [Fl, ' ='], Vl}]};
record_field({typed_record_field, {record_field, _, F, Val}, Type}, Opts) ->
    {L, _P, R} = inop_prec('='),
    Fl = lexpr(F, L, Opts),
    Vl = typed(lexpr(Val, R, Opts), Type),
    {list, [{cstep, [Fl, ' ='], Vl}]};
record_field({typed_record_field, Field, Type}, Opts) ->
    typed(record_field(Field, Opts), Type);
record_field({record_field, _, F}, Opts) ->
    lexpr(F, 0, Opts).

map_fields(Fs, Opts) ->
    tuple(Fs, fun map_field/2, Opts).

map_field({map_field_assoc, _, K, V}, Opts) ->
    Pl = lexpr(K, 0, Opts),
    {list, [{step, [Pl, leaf(" =>")], lexpr(V, 0, Opts)}]};
map_field({map_field_exact, _, K, V}, Opts) ->
    Pl = lexpr(K, 0, Opts),
    {list, [{step, [Pl, leaf(" :=")], lexpr(V, 0, Opts)}]}.

list({cons, _, H, T}, Es, Opts) ->
    list(T, [H | Es], Opts);
list({nil, _}, Es, Opts) ->
    proper_list(reverse(Es), Opts);
list(Other, Es, Opts) ->
    improper_list(reverse(Es, [Other]), Opts).

%% if_clauses(Clauses, Opts) -> [Char].
%%  Print 'if' clauses.
if_clauses(Cs, Opts) ->
    clauses(fun if_clause/2, Opts, Cs).

if_clause({clause, _, [], G, B}, Opts) ->
    Gl = [guard_no_when(G, Opts), ' ->'],
    {step, Gl, body(B, Opts)}.

guard_no_when([E | Es], Opts) when is_list(E) ->
    expr_list([E | Es], $;, fun guard0/2, Opts);
% before R6
guard_no_when([E | Es], Opts) ->
    guard_no_when([[E | Es]], Opts);
% cannot happen
guard_no_when([], _) ->
    leaf("true").

%% cr_clauses(Clauses, Opts) -> [Char].
%%  Print 'case'/'receive' clauses.
cr_clauses(Cs, Opts) ->
    clauses(fun cr_clause/2, Opts, Cs).

cr_clause({clause, _, [T], G, B}, Opts) ->
    El = lexpr(T, 0, Opts),
    Gl = guard_when(El, G, Opts),
    Bl = body(B, Opts),
    {step, Gl, Bl}.

%% try_clauses(Clauses, Opts) -> [Char].
%%  Print 'try' clauses.
try_clauses(Cs, Opts) ->
    clauses(fun try_clause/2, Opts, Cs).

try_clause({clause, _, [{tuple, _, [C, V, S]}], G, B}, Opts) ->
    Cs = lexpr(C, 0, Opts),
    El = lexpr(V, 0, Opts),
    CsEl = [Cs, $:, El],
    Sl = stack_backtrace(S, CsEl, Opts),
    Gl = guard_when(Sl, G, Opts),
    Bl = body(B, Opts),
    {step, Gl, Bl}.

stack_backtrace({var, _, '_'}, El, _Opts) ->
    El;
stack_backtrace(S, El, Opts) ->
    El ++ [$:, lexpr(S, 0, Opts)].

%% fun_clauses(Clauses, Opts) -> [Char].
%%  Print 'fun' clauses.
fun_clauses(Cs, Opts, unnamed) ->
    nl_clauses(fun fun_clause/2, [$;], Opts, Cs);
fun_clauses(Cs, Opts, {named, Name}) ->
    nl_clauses(
        fun(C, H) ->
            {step, Gl, Bl} = fun_clause(C, H),
            {step, [atom_to_list(Name), Gl], Bl}
        end,
        [$;],
        Opts,
        Cs
    ).

fun_clause({clause, _, A, G, B}, Opts) ->
    El = args(A, Opts),
    Gl = guard_when(El, G, Opts),
    Bl = body(B, Opts),
    {step, Gl, Bl}.

%% cond_clauses(Clauses, Opts) -> [Char].
%%  Print 'cond' clauses.
cond_clauses(Cs, Opts) ->
    clauses(fun cond_clause/2, Opts, Cs).

cond_clause({clause, _, [], [[E]], B}, Opts) ->
    {step, [lexpr(E, Opts), ' ->'], body(B, Opts)}.

%% nl_clauses(Type, Opts, Clauses) -> [Char].
%%  Generic clause printing function (always breaks lines).
nl_clauses(Type, Sep, Opts, Cs) ->
    {prefer_nl, Sep, lexprs(Cs, Type, Opts)}.

%% clauses(Type, Opts, Clauses) -> [Char].
%%  Generic clause printing function (breaks lines since R12B).
clauses(Type, Opts, Cs) ->
    {prefer_nl, [$;], lexprs(Cs, Type, Opts)}.

%% lc_quals(Qualifiers, After, Opts)
%% List comprehension qualifiers (breaks lines since R12B).
lc_quals(Qs, Opts) ->
    {prefer_nl, [$,], lexprs(Qs, fun lc_qual/2, Opts)}.

lc_qual({b_generate, _, Pat, E}, Opts) ->
    Pl = lexpr(Pat, 0, Opts),
    {list, [{step, [Pl, leaf(" <=")], lexpr(E, 0, Opts)}]};
lc_qual({generate, _, Pat, E}, Opts) ->
    Pl = lexpr(Pat, 0, Opts),
    {list, [{step, [Pl, leaf(" <-")], lexpr(E, 0, Opts)}]};
lc_qual(Q, Opts) ->
    lexpr(Q, 0, Opts).

proper_list(Es, Opts) ->
    {seq, $[, $], [$,], lexprs(Es, Opts)}.

improper_list(Es, Opts) ->
    {seq, $[, $], [{$,, ' |'}], lexprs(Es, Opts)}.

tuple(L, Opts) ->
    tuple(L, fun lexpr/2, Opts).

tuple([], _F, _Opts) ->
    leaf("{}");
tuple(Es, F, Opts) ->
    {seq, ${, $}, [$,], lexprs(Es, F, Opts)}.

args(As, Opts) ->
    {seq, $(, $), [$,], lexprs(As, Opts)}.

expr_list(Es, Sep, F, Opts) ->
    {seq, [], [], Sep, lexprs(Es, F, Opts)}.

lexprs(Es, Opts) ->
    lexprs(Es, fun lexpr/2, Opts).

lexprs(Es, F, Opts) ->
    [F(E, Opts) || E <- Es].

maybe_paren(P, Prec, Expr) when P < Prec ->
    [$(, Expr, $)];
maybe_paren(_P, _Prec, Expr) ->
    Expr.

leaf(S) ->
    {leaf, string:length(S), S}.

%%% Do the formatting. Currently nothing fancy. Could probably have
%%% done it in one single pass.
frmt(Item, PP) ->
    frmt(Item, 0, PP).

frmt(Item, I, PP) ->
    ST = spacetab(),
    WT = wordtable(),
    {Chars, _Length} = f(Item, I, ST, WT, PP),
    [Chars].

%%% What the tags mean:
%%% - C: a character
%%% - [I|Is]: Is follow after I without newline or space
%%% - {list,IPs}: try to put all IPs on one line, if that fails newlines
%%%   and indentation are inserted between IPs.
%%% - {first,I,IP2}: IP2 follows after I, and is output with an indentation
%%%   updated with the width of I.
%%% - {seq,Before,After,Separator,IPs}: a sequence of Is separated by
%%%   Separator. Before is output before IPs, and the indentation of IPs
%%%   is updated with the width of Before. After follows after IPs.
%%% - {force_nl,ExtraInfo,I}: fun-info (a comment) forces linebreak before I.
%%% - {prefer_nl,Sep,IPs}: forces linebreak between Is unlesss negative
%%%   indentation.
%%% - {atom,A}: an atom
%%% - {singleton_atom_type,A}: a singleton atom type
%%% - {char,C}: a character
%%% - {string,S}: a string.
%%% - {value,T}: a term.
%%% - {hook,...}, {ehook,...}: hook expressions.
%%%
%%% list, first, seq, force_nl, and prefer_nl all accept IPs, where each
%%% element is either an item or a tuple {step|cstep,I1,I2}. step means
%%% that I2 is output after linebreak and an incremented indentation.
%%% cstep works similarly, but no linebreak if the width of I1 is less
%%% than the indentation (this is for "A = <expression over several lines>).
f([] = Nil, _I0, _ST, _WT, _PP) ->
    {Nil, 0};
f(C, _I0, _ST, _WT, _PP) when is_integer(C) ->
    {C, 1};
f({leaf, Length, Chars}, _I0, _ST, _WT, _PP) ->
    {Chars, Length};
f([Item | Items], I0, ST, WT, PP) ->
    consecutive(Items, f(Item, I0, ST, WT, PP), I0, ST, WT, PP);
f({list, Items}, I0, ST, WT, PP) ->
    f({seq, [], [], [[]], Items}, I0, ST, WT, PP);
f({first, E, Item}, I0, ST, WT, PP) ->
    f({seq, E, [], [[]], [Item]}, I0, ST, WT, PP);
f({seq, Before, After, Sep, LItems}, I0, ST, WT, PP) ->
    BCharsSize = f(Before, I0, ST, WT, PP),
    I = indent(BCharsSize, I0),
    CharsSizeL = fl(LItems, Sep, I, After, ST, WT, PP),
    {CharsL, SizeL} = unz(CharsSizeL),
    {BCharsL, BSizeL} = unz1([BCharsSize]),
    Sizes = BSizeL ++ SizeL,
    NSepChars =
        if
            is_list(Sep), Sep =/= [] ->
                % not string:length
                erlang:max(0, length(CharsL) - 1);
            true ->
                0
        end,
    case same_line(I0, Sizes, NSepChars) of
        {yes, Size} ->
            Chars =
                if
                    NSepChars > 0 -> insert_sep(CharsL, $\s);
                    true -> CharsL
                end,
            {BCharsL ++ Chars, Size};
        no ->
            CharsList = handle_step(CharsSizeL, I, ST),
            {LChars, LSize} = maybe_newlines(CharsList, LItems, I, NSepChars, ST),
            {[BCharsL, LChars], nsz(LSize, I0)}
    end;
f({force_nl, _ExtraInfoItem, Item}, I, ST, WT, PP) when I < 0 ->
    %% Extra info is a comment; cannot have that on the same line
    f(Item, I, ST, WT, PP);
f({force_nl, ExtraInfoItem, Item}, I, ST, WT, PP) ->
    f({prefer_nl, [], [ExtraInfoItem, Item]}, I, ST, WT, PP);
f({prefer_nl, Sep, LItems}, I, ST, WT, PP) when I < 0 ->
    f({seq, [], [], Sep, LItems}, I, ST, WT, PP);
f({prefer_nl, Sep, LItems}, I0, ST, WT, PP) ->
    CharsSize2L = fl(LItems, Sep, I0, [], ST, WT, PP),
    {_CharsL, Sizes} = unz(CharsSize2L),
    if
        Sizes =:= [] ->
            {[], 0};
        true ->
            {insert_newlines(CharsSize2L, I0, ST), nsz(lists:last(Sizes), I0)}
    end;
f({value, V}, I, ST, WT, PP) ->
    f(write_a_value(V, PP), I, ST, WT, PP);
f({atom, A}, I, ST, WT, PP) ->
    f(write_an_atom(A, PP), I, ST, WT, PP);
f({singleton_atom_type, A}, I, ST, WT, PP) ->
    f(write_a_singleton_atom_type(A, PP), I, ST, WT, PP);
f({char, C}, I, ST, WT, PP) ->
    f(write_a_char(C, PP), I, ST, WT, PP);
f({string, S}, I, ST, WT, PP) ->
    f(write_a_string(S, I, PP), I, ST, WT, PP);
f({reserved, R}, I, ST, WT, PP) ->
    f(R, I, ST, WT, PP);
f({hook, HookExpr, Precedence, Func, Options}, I, _ST, _WT, _PP) ->
    Chars = Func(HookExpr, I, Precedence, Options),
    {Chars, indentation(Chars, I)};
f({ehook, HookExpr, Precedence, {Mod, Func, Eas} = ModFuncEas}, I, _ST, _WT, _PP) ->
    Chars = apply(Mod, Func, [HookExpr, I, Precedence, ModFuncEas | Eas]),
    {Chars, indentation(Chars, I)};
f(WordName, _I, _ST, WT, _PP) when is_atom(WordName) ->
    word(WordName, WT).

-define(IND, 4).

%% fl(ListItems, I0, ST, WT) -> [[CharsSize1,CharsSize2]]
%% ListItems = [{Item,Items}|Item]
fl([], _Sep, I0, After, ST, WT, PP) ->
    [[f(After, I0, ST, WT, PP), {[], 0}]];
fl(CItems, Sep0, I0, After, ST, WT, PP) ->
    F = fun
        ({step, Item1, Item2}, S) ->
            [f(Item1, I0, ST, WT, PP), f([Item2, S], incr(I0, ?IND), ST, WT, PP)];
        ({cstep, Item1, Item2}, S) ->
            {_, Sz1} = CharSize1 = f(Item1, I0, ST, WT, PP),
            if
                is_integer(Sz1), Sz1 < ?IND ->
                    Item2p = [leaf("\s"), Item2, S],
                    [consecutive(Item2p, CharSize1, I0, ST, WT, PP), {[], 0}];
                true ->
                    [CharSize1, f([Item2, S], incr(I0, ?IND), ST, WT, PP)]
            end;
        ({reserved, Word}, S) ->
            [f([Word, S], I0, ST, WT, PP), {[], 0}];
        (Item, S) ->
            [f([Item, S], I0, ST, WT, PP), {[], 0}]
    end,
    {Sep, LastSep} = sep(Sep0),
    fl1(CItems, F, Sep, LastSep, After).

sep([{S, LS}]) -> {[S], [LS]};
sep({_, _} = Sep) -> Sep;
sep(S) -> {S, S}.

fl1([CItem], F, _Sep, _LastSep, After) ->
    [F(CItem, After)];
fl1([CItem1, CItem2], F, _Sep, LastSep, After) ->
    [F(CItem1, LastSep), F(CItem2, After)];
fl1([CItem | CItems], F, Sep, LastSep, After) ->
    [F(CItem, Sep) | fl1(CItems, F, Sep, LastSep, After)].

consecutive(Items, CharSize1, I0, ST, WT, PP) ->
    {CharsSizes, _Length} = mapfoldl(
        fun(Item, Len) ->
            CharsSize = f(Item, Len, ST, WT, PP),
            {CharsSize, indent(CharsSize, Len)}
        end,
        indent(CharSize1, I0),
        Items
    ),
    {CharsL, SizeL} = unz1([CharSize1 | CharsSizes]),
    {CharsL, line_size(SizeL)}.

unz(CharsSizesL) ->
    unz1(append(CharsSizesL)).

unz1(CharSizes) ->
    lists:unzip(nonzero(CharSizes)).

nonzero(CharSizes) ->
    lists:filter(fun({_, Sz}) -> Sz =/= 0 end, CharSizes).

maybe_newlines([{Chars, Size}], [], _I, _NSepChars, _ST) ->
    {Chars, Size};
maybe_newlines(CharsSizeList, Items, I, NSepChars, ST) when I >= 0 ->
    maybe_sep(CharsSizeList, Items, I, NSepChars, nl_indent(I, ST)).

maybe_sep([{Chars1, Size1} | CharsSizeL], [Item | Items], I0, NSepChars, Sep) ->
    I1 =
        case classify_item(Item) of
            atomic ->
                I0 + Size1;
            _ ->
                ?MAXLINE + 1
        end,
    maybe_sep1(CharsSizeL, Items, I0, I1, Sep, NSepChars, Size1, [Chars1]).

maybe_sep1([{Chars, Size} | CharsSizeL], [Item | Items], I0, I, Sep, NSepChars, Sz0, A) ->
    case classify_item(Item) of
        atomic when is_integer(Size) ->
            Size1 = Size + 1,
            I1 = I + Size1,
            if
                I1 =< ?MAXLINE ->
                    A1 =
                        if
                            NSepChars > 0 -> [Chars, $\s | A];
                            true -> [Chars | A]
                        end,
                    maybe_sep1(CharsSizeL, Items, I0, I1, Sep, NSepChars, Sz0 + Size1, A1);
                true ->
                    A1 = [Chars, Sep | A],
                    maybe_sep1(CharsSizeL, Items, I0, I0 + Size, Sep, NSepChars, Size1, A1)
            end;
        _ ->
            A1 = [Chars, Sep | A],
            maybe_sep1(CharsSizeL, Items, I0, ?MAXLINE + 1, Sep, NSepChars, 0, A1)
    end;
maybe_sep1(_CharsSizeL, _Items, _Io, _I, _Sep, _NSepChars, Sz, A) ->
    {lists:reverse(A), Sz}.

insert_newlines(CharsSizesL, I, ST) when I >= 0 ->
    {CharsL, _} = unz1(handle_step(CharsSizesL, I, ST)),
    insert_nl(CharsL, I, ST).

handle_step(CharsSizesL, I, ST) ->
    map(
        fun
            ([{_C1, 0}, {_C2, 0}]) ->
                {[], 0};
            ([{C1, Sz1}, {_C2, 0}]) ->
                {C1, Sz1};
            ([{C1, Sz1}, {C2, Sz2}]) when Sz2 > 0 ->
                {insert_nl([C1, C2], I + ?IND, ST), line_size([Sz1, Sz2])}
        end,
        CharsSizesL
    ).

insert_nl(CharsL, I, ST) ->
    insert_sep(CharsL, nl_indent(I, ST)).

insert_sep([Chars1 | CharsL], Sep) ->
    [Chars1 | [[Sep, Chars] || Chars <- CharsL]].

nl_indent(0, _T) ->
    $\n;
nl_indent(I, T) when I > 0 ->
    [$\n | spaces(I, T)].

classify_item({atom, _}) -> atomic;
classify_item({singleton_atom_type, _}) -> atomic;
classify_item(Atom) when is_atom(Atom) -> atomic;
classify_item({leaf, _, _}) -> atomic;
classify_item(_) -> complex.

same_line(I0, SizeL, NSepChars) ->
    try
        Size = lists:sum(SizeL) + NSepChars,
        true = incr(I0, Size) =< ?MAXLINE,
        {yes, Size}
    catch
        _:_ ->
            no
    end.

line_size(SizeL) ->
    line_size(SizeL, 0, false).

line_size([], Size, false) ->
    Size;
line_size([], Size, true) ->
    {line, Size};
line_size([{line, Len} | SizeL], _, _) ->
    line_size(SizeL, Len, true);
line_size([Sz | SizeL], SizeSoFar, LF) ->
    line_size(SizeL, SizeSoFar + Sz, LF).

nsz({line, _Len} = Sz, _I) ->
    Sz;
nsz(Size, I) when I >= 0 ->
    {line, Size + I}.

indent({_Chars, {line, Len}}, _I) ->
    Len;
indent({_Chars, Size}, I) ->
    incr(I, Size).

incr(I, _Incr) when I < 0 ->
    I;
incr(I, Incr) ->
    I + Incr.

indentation(E, I) when I < 0 ->
    string:length(E);
indentation(E, I0) ->
    I = io_lib_format:indentation(E, I0),
    case has_nl(E) of
        true -> {line, I};
        false -> I
    end.

has_nl([$\n | _]) ->
    true;
has_nl([C | Cs]) when is_integer(C) ->
    has_nl(Cs);
has_nl([C | Cs]) ->
    has_nl(C) orelse has_nl(Cs);
has_nl([]) ->
    false.

write_a_value(V, PP) ->
    flat_leaf(write_value(V, PP)).

write_an_atom(A, PP) ->
    flat_leaf(write_atom(A, PP)).

write_a_singleton_atom_type(A, PP) ->
    flat_leaf(write_singleton_atom_type(A, PP)).

write_a_char(C, PP) ->
    flat_leaf(write_char(C, PP)).

-define(MIN_SUBSTRING, 5).

write_a_string(S, I, PP) when I < 0; S =:= [] ->
    flat_leaf(write_string(S, PP));
write_a_string(S, I, PP) ->
    Len = erlang:max(?MAXLINE - I, ?MIN_SUBSTRING),
    {list, write_a_string(S, Len, Len, PP)}.

write_a_string([], _N, _Len, _PP) ->
    [];
write_a_string(S, N, Len, PP) ->
    SS = string:slice(S, 0, N),
    Sl = write_string(SS, PP),
    case (string:length(Sl) > Len) and (N > ?MIN_SUBSTRING) of
        true ->
            write_a_string(S, N - 1, Len, PP);
        false ->
            [
                flat_leaf(Sl)
                | write_a_string(string:slice(S, string:length(SS)), Len, Len, PP)
            ]
    end.

flat_leaf(S) ->
    L = lists:flatten(S),
    {leaf, string:length(L), L}.

write_value(V, PP) ->
    (PP#pp.value_fun)(V).

write_atom(A, PP) ->
    (PP#pp.value_fun)(A).

write_singleton_atom_type(A, PP) ->
    (PP#pp.singleton_atom_type_fun)(A).

write_string(S, PP) ->
    (PP#pp.string_fun)(S).

write_char(C, PP) ->
    (PP#pp.char_fun)(C).

%%
%% Utilities
%%
a0() ->
    erl_anno:new(0).

-define(N_SPACES, 30).

spacetab() ->
    {[_ | L], _} = mapfoldl(fun(_, A) -> {A, [$\s | A]} end, [], lists:seq(0, ?N_SPACES)),
    list_to_tuple(L).

spaces(N, T) when N =< ?N_SPACES ->
    element(N, T);
spaces(N, T) ->
    [element(?N_SPACES, T) | spaces(N - ?N_SPACES, T)].

wordtable() ->
    L = [
        begin
            {leaf, Sz, S} = leaf(W),
            {S, Sz}
        end
        || W <- [
               " ->",
               " =",
               "<<",
               ">>",
               "[]",
               "after",
               "begin",
               "case",
               "catch",
               "end",
               "fun",
               "if",
               "of",
               "receive",
               "try",
               "when",
               " ::",
               "..",
               " |"
           ]
    ],
    list_to_tuple(L).

word(' ->', WT) -> element(1, WT);
word(' =', WT) -> element(2, WT);
word('<<', WT) -> element(3, WT);
word('>>', WT) -> element(4, WT);
word('[]', WT) -> element(5, WT);
word('after', WT) -> element(6, WT);
word('begin', WT) -> element(7, WT);
word('case', WT) -> element(8, WT);
word('catch', WT) -> element(9, WT);
word('end', WT) -> element(10, WT);
word('fun', WT) -> element(11, WT);
word('if', WT) -> element(12, WT);
word('of', WT) -> element(13, WT);
word('receive', WT) -> element(14, WT);
word('try', WT) -> element(15, WT);
word('when', WT) -> element(16, WT);
word(' ::', WT) -> element(17, WT);
word('..', WT) -> element(18, WT);
word(' |', WT) -> element(19, WT).
