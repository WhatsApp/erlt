-module(erl2ocaml).

-export([erl2ocaml_ffi/1, erl2ocaml_st/1, main/1, ffi/0, st_deps/1, ffi_deps/1, format_error/1]).

-record(context, {
    module :: atom(),
    mode :: 'priv' | 'pub',
    export_types :: [{atom, integer()}]
}).

ffi() ->
    FfiLines = [
        "type 'a tuple1'1 = Tuple1 of 'a\n",
        "type any'0\n",
        "type atom'0\n",
        "type binary'0\n",
        "type bitstring'0\n",
        "type byte'0\n",
        "type identifier'0\n",
        "type iodata'0\n",
        "type iolist'0\n",
        "type ('k, 'v) map'2\n",
        "type neg_integer'0\n",
        "type none'0\n",
        "type non_neg_integer'0\n",
        "type number'0\n",
        "type pid'0\n",
        "type port'0\n",
        "type pos_integer'0\n",
        "type reference'0\n",
        "type term'0\n",
        "type timeout'0\n",
        "type node'0 = atom'0\n",
        "type no_return'0 = none'0\n",
        "val same'2 : 'a * 'a -> unit\n",
        "val to_string'1 : 'a -> string\n",
        "val list_diff'2 : 'a list * 'a list -> 'a list\n"
    ],
    iolist_to_binary(FfiLines).

main(["-erl", InFile, "-ml", MlFile, "-mli", MliFile]) ->
    {ok, Forms} = erl2_epp:parse_file(InFile, []),
    {ok, MliCode, MlCode} = erl2ocaml_st(Forms),
    file:write_file(MliFile, MliCode),
    file:write_file(MlFile, MlCode);
main(["-ast", File]) ->
    {ok, Forms} = erl2_epp:parse_file(File, []),
    io:format("Forms:\n~p\n", [Forms]),
    Funs = get_fns(Forms),
    SortedFuns = mk_sccs(Funs),
    io:format("SortedFuns:\n~p\n", [SortedFuns]),
    Specs = get_specs(Forms),
    io:format("Specs:\n~p\n", [Specs]),
    TypeDefs = get_type_defs(Forms),
    io:format("Types:\n~p\n", [TypeDefs]);
main(_) ->
    usage().

usage() ->
    io:format("usage:\n"),
    io:format("  erl2ocaml -erl mod.erl -ml mod.ml -mli mod.mli:\n").

erl2ocaml_ffi(Forms) ->
    try
        do_erl2ocaml_ffi(Forms)
    catch
        throw:{erl2ocaml_error, {Line, ErrorBody}} ->
            {error, {Line, ErrorBody}}
    end.

do_erl2ocaml_ffi(Forms) ->
    erlang:put('rec_tv_gen', 1),
    Exports = get_exports(Forms),
    Ctx = #context{
        module = get_module(Forms),
        export_types = get_export_types(Forms)
    },
    Funs = get_fns(Forms),
    AllSpecs = get_specs(Forms),
    {PubSpecs, SpecIds} =
        lists:unzip([{PubSpec,FId} || PubSpec = {_,_,_,{FId,_}} <- AllSpecs, lists:member(FId,Exports)]),
    UnSpecedFunIds = Exports -- SpecIds,
    check_export_specs(Funs, UnSpecedFunIds),

    PrivOTypes = [erl2ocaml_spec(S, Ctx) || S <- AllSpecs],
    PubOTypes =  [erl2ocaml_spec(S, Ctx) || S <- PubSpecs],
    PubSpecLines = ["val " ++ N ++ " : " ++ T ++ "\n" || {N, T} <- PubOTypes],
    PrivSpecLines = ["val " ++ N ++ " : " ++ T ++ "\n" || {N, T} <- PrivOTypes],
    TypeDefs = get_type_defs(Forms),

    PubTypeDefLines = type_def_scc(TypeDefs, Ctx#context{mode = 'pub'}),
    PrivTypeDefLines = type_def_scc(TypeDefs, Ctx#context{mode = 'priv'}),

    PubInterfaceLines = PubTypeDefLines ++ PubSpecLines,
    PrivInterfaceLines = PrivTypeDefLines ++ PrivSpecLines,

    PubMliCode = iolist_to_binary(PubInterfaceLines),
    PrivMliCode = iolist_to_binary(PrivInterfaceLines),

    {ok, PubMliCode, PrivMliCode}.

erl2ocaml_st(Forms) ->
    try
        do_erl2ocaml_st(Forms)
    catch
        throw:{erl2ocaml_error, {Line, ErrorBody}} ->
            {error, {Line, ErrorBody}}
    end.

do_erl2ocaml_st(Forms) ->
    erlang:put('rec_tv_gen', 1),
    Exports = get_exports(Forms),
    Ctx = #context{
        module = get_module(Forms),
        export_types = get_export_types(Forms)
    },
    Funs = get_fns(Forms),
    SortedFuns = mk_sccs(Funs),

    AllSpecs = get_specs(Forms),
    {PubSpecs, SpecIds} =
        lists:unzip([{PubSpec,FId} || PubSpec = {_,_,_,{FId,_}} <- AllSpecs, lists:member(FId,Exports)]),
    UnSpecedFunIds = Exports -- SpecIds,
    check_export_specs(Funs, UnSpecedFunIds),

    PrivOTypes = [erl2ocaml_spec(S, Ctx) || S <- AllSpecs],
    PubOTypes = [erl2ocaml_spec(S, Ctx) || S <- PubSpecs],
    PubSpecLines = ["val " ++ N ++ " : " ++ T ++ "\n" || {N, T} <- PubOTypes],
    PrivSpecLines = ["val " ++ N ++ " : " ++ T ++ "\n" || {N, T} <- PrivOTypes],
    TypeDefs = get_type_defs(Forms),

    PubTypeDefLines = type_def_scc(TypeDefs, Ctx#context{mode = 'pub'}),
    PrivTypeDefLines = type_def_scc(TypeDefs, Ctx#context{mode = 'priv'}),

    RawDocs = erl2ocaml_sccs(SortedFuns, PrivOTypes, Ctx),
    FunLines = indent_docs(RawDocs),

    PubInterfaceLines = PubTypeDefLines ++ PubSpecLines,
    PrivInterfaceLines = PrivTypeDefLines ++ PrivSpecLines,
    ImplementationLines = PrivTypeDefLines ++ FunLines,

    OcamlModule = first_upper(atom_to_list(Ctx#context.module)),
    OcamlPrivModule = OcamlModule ++ "_priv",

    PubMliCode = iolist_to_binary(PubInterfaceLines),
    MlCode = iolist_to_binary(ImplementationLines),
    PrivMlCode =
        "module "
            ++ OcamlPrivModule
            ++ " : sig\n"
            ++ PrivInterfaceLines
            ++ "\n" ++ "end = struct\n"
            ++ ImplementationLines
            ++ "\n" ++ "end",

    {ok, PubMliCode, MlCode, PrivMlCode}.

check_export_specs(Funs, UnSpecedFunIds) ->
    case UnSpecedFunIds of
        [] ->
            ok;
        _ ->
            [{L,N,A}|_] = [{L,N,A} || {_,L,N,A,_} <- Funs, lists:member({N, A}, UnSpecedFunIds)],
            throw({erl2ocaml_error, {L, {'unspeced_fun', N, A}}})
    end.

erl2ocaml_sccs(SCCs, OTypes, Ctx) ->
    lists:flatmap(fun(SCC) -> erl2ocaml_scc1(SCC, Ctx, OTypes) end, SCCs).

erl2ocaml_scc1([], _, _) ->
    [];
erl2ocaml_scc1([F|Fs], Ctx, OTypes) ->
    [function(F, Ctx, "let rec ", OTypes) | [function(F1, Ctx, "and ", OTypes) || F1 <- Fs]].

function({function, _Line, Name, Arity, Clauses}, Ctx, Prefix, OTypes) ->
    NameStr = atom_to_list(Name) ++ "'" ++ integer_to_list(Arity),
    TypeSpec = case lists:keyfind(NameStr, 1, OTypes) of
                   {_, Ts} -> " : " ++ Ts;
                   _ -> ""
               end,
    [{Prefix ++ NameStr ++ TypeSpec ++ " = function"}, clauses(Clauses, Ctx), {""}].

clauses([EC|Cs], Ctx) ->
    OC = clause(EC, Ctx),
    OC ++ clauses(Cs, Ctx);
clauses([], _) -> [].

clause({clause, Line, Head, Guard, Body}, Ctx) ->
    case Guard of
        [] ->
            OHead = {"| " ++ head(Head, Ctx) ++ " ->" },
            [OHead, expr_seq(Body, Ctx)];
        Guards ->
            [Guard1|Guards1] =
                lists:map(
                    fun([E0|Es]) ->
                        lists:foldl(fun(H,Acc) -> {op, Line, 'and', Acc, H} end, E0, Es)
                    end,
                    Guards),
            GuardExp = lists:foldl(fun(H,Acc) -> {op, Line, 'or', Acc, H} end, Guard1, Guards1),
            [{"| " ++ head(Head, Ctx) ++ " when" }, [expr(GuardExp, Ctx)], {"->"}, expr_seq(Body, Ctx)]
    end.

head(Ps, Ctx) ->
    OPs = patterns(Ps, Ctx),
    Ops1 = interleave(false, ", ", OPs),
    Docs = ["(", Ops1, ")"],
    OHead = binary_to_list(iolist_to_binary(Docs)),
    OHead.

patterns([P0|Ps], Ctx) ->
    P1 = pattern(P0, Ctx),
    [P1|patterns(Ps, Ctx)];
patterns([], _Ctx) -> [].

pattern({var,_Line,'_'}, _Ctx) ->
    "_";
pattern({var,_Line,V}, _Ctx) ->
    "v_" ++ atom_to_list(V);
pattern({match,_Line,P,{var,_Line,V}}, Ctx) ->
    pattern(P, Ctx) ++ " as " ++ "v_" ++ atom_to_list(V);
pattern({match,Line,_,MP}, _Ctx) ->
    throw({erl2ocaml_error, {Line, {'not_supported_syntax', MP, "complex match pattern"}}});
pattern({integer,_Line,I}, _Ctx) ->
    integer_to_list(I);
pattern({char,_Line,C}, _Ctx) ->
    "'" ++ io_lib:format("~c", [C]) ++ "'";
pattern({float,_Line,F}, _Ctx) ->
    float_to_list(F, [{decimals, 0}]);
pattern({atom,_Line,true}, _Ctx) ->
    "true";
pattern({atom,_Line,false}, _Ctx) ->
    "false";
pattern({atom,Line,Atom}, _Ctx) ->
    throw({erl2ocaml_error, {Line, {'not_supported_syntax', Atom, "atom in pattern"}}});
pattern({enum,_,{op,_,'.',{atom,_,Enum},{atom,_,Ctr}},[]}, _Ctx) ->
    enum_ctr_name(Enum, Ctr);
pattern({enum,_,{op,_,'.',{op,_,'.',{atom,_,Mod},{atom,_,Enum}},{atom,_,Ctr}},[]}, Ctx) ->
    enum_ctr_name(Mod, Enum, Ctr, Ctx);
pattern({enum,_,{op,_,'.',{atom,_,Enum},{atom,_,Ctr}},Args}, Ctx) ->
    enum_ctr_name(Enum, Ctr)
        ++ "("
        ++ binary_to_list(iolist_to_binary(interleave(false, ", ", patterns(Args, Ctx))))
        ++ ")";
pattern({enum,_,{op,_,'.',{op,_,'.',{atom,_,Mod},{atom,_,Enum}},{atom,_,Ctr}},Args}, Ctx) ->
    enum_ctr_name(Mod, Enum, Ctr, Ctx)
        ++ "("
        ++ binary_to_list(iolist_to_binary(interleave(false, ", ", patterns(Args, Ctx))))
        ++ ")";
pattern({string,_Line,S}, _Ctx) ->
    "\"" ++ S ++ "\"";
pattern({nil,_Line}, _Ctx) ->
    "[]";
pattern({cons,_Line,H,T}, Ctx) ->
    "(" ++ pattern(H, Ctx) ++  "::" ++ pattern(T, Ctx) ++ ")";
pattern({tuple,_Line,[P]}, Ctx) ->
    Docs = ["Ffi.Tuple1(", [pattern(P, Ctx)] , ")"],
    binary_to_list(iolist_to_binary(Docs));
pattern({tuple,_Line,Ps}, Ctx) ->
    OPs = patterns(Ps, Ctx),
    Ops1 = interleave(false, ", ", OPs),
    Docs = ["(", Ops1, ")"],
    binary_to_list(iolist_to_binary(Docs));
pattern(Pat, _Ctx) ->
    Line = erlang:element(2, Pat),
    throw({erl2ocaml_error, {Line, {'not_supported_syntax', Pat}}}).

pattern_to_expr({match,_Line,P,_AsP}) ->
    pattern_to_expr(P);
pattern_to_expr(Form) ->
    Form.

expr_seq(ES, Ctx)->
    expr1([], ES, Ctx).

expr1(Acc, [E={match,_Line,P,_}], Ctx) ->
    expr1(Acc, [E, pattern_to_expr(P)], Ctx);
expr1(Acc, [E], Ctx)->
    Delta = [expr(E, Ctx)],
    Acc ++ Delta;
expr1(Acc, [{match,_Line,P,E}|Es], Ctx)->
    PatString = pattern(P, Ctx),
    ExprDoc = expr(E, Ctx),
    Delta = [{"let " ++ PatString ++ " = "}] ++ [ExprDoc] ++ [{"in"}],
    Acc1 = Acc ++ Delta,
    expr1(Acc1, Es, Ctx);
expr1(Acc, [{match_rec,_Line,P,E}|Es], Ctx)->
    PatString = pattern(P, Ctx),
    ExprDoc = expr(E, Ctx),
    Delta = [{"let rec " ++ PatString ++ " = "}] ++ [ExprDoc] ++ [{"in"}],
    Acc1 = Acc ++ Delta,
    expr1(Acc1, Es, Ctx);
expr1(Acc, [E|Es], Ctx)->
    ExprDoc = expr(E, Ctx),
    Delta = [{"let _ = "}] ++ [ExprDoc] ++ [{"in"}],
    Acc1 = Acc ++ Delta,
    expr1(Acc1, Es, Ctx).

expr({var,_Line,V}, _Ctx) ->
    {"v_" ++ atom_to_list(V)};
expr({integer,_Line,I}, _Ctx) ->
    {integer_to_list(I)};
expr({float,_Line,F}, _Ctx) ->
    {float_to_list(F, [{decimals, 0}])};
expr({atom,_Line,true}, _Ctx) ->
    {"true"};
expr({atom,_Line,false}, _Ctx) ->
    {"false"};
expr(Atom={atom,Line,_A}, _Ctx) ->
    throw({erl2ocaml_error, {Line, {'not_supported_syntax', Atom, "atom"}}});
expr({string,_Line,S}, _Ctx) ->
    {"\"" ++ S ++ "\""};
expr({char,_Line,C}, _Ctx) ->
    {"'" ++ io_lib:format("~c", [C]) ++ "'"};
expr({nil,_Line}, _Ctx) ->
    {"[]"};
expr({cons,_Line,H,T}, Ctx) ->
    [{"(("}, expr(H, Ctx), {") :: ("}, expr(T, Ctx), {"))"}];
expr(E={lc,Line,_,_}, _Ctx) ->
    throw({erl2ocaml_error, {Line, {'not_supported_syntax', E, "list comprehension"}}});
expr(E={bc,Line,_,_}, _Ctx) ->
    throw({erl2ocaml_error, {Line, {'not_supported_syntax', E, "binary comprehension"}}});
expr({tuple,_Line,[E]}, Ctx) ->
    [{"Ffi.Tuple1("}, [expr(E, Ctx)], {")"}];
expr({tuple,_Line,Es}, Ctx) ->
    expr({tuple1,_Line,Es},Ctx);
expr({tuple1,_Line,Es}, Ctx) ->
    Docs0 = [expr(E, Ctx) || E <- Es],
    Docs1 = interleave1(false, Docs0),
    Docs2 = [{"("}, Docs1, {")"}],
    Docs2;
expr({map,_Line, InitAssocs}, Ctx) ->
    Fields =  [init_assoc(A, Ctx) || A <- InitAssocs],
    [{"object"}, Fields, {"end"}];
expr({map,Line,Map,UpdateAssocs}, Ctx) ->
    FfiSame = {remote, Line, {atom, Line, 'ffi'}, {atom, Line, 'same'}},
    SameFields =
        [{'call',Line,FfiSame,[V,{'op',Line,'.',Map,K}]} || {map_field_exact,_,K={atom,_,_},V} <- UpdateAssocs],
    SameMaps =
        [{'call',Line,FfiSame,[Map,{map1,Line, Map, [UA]}]} || UA <- UpdateAssocs],
    Exprs = SameMaps ++ SameFields ++ [{map1,Line, Map, UpdateAssocs}],
    expr({block, Line, Exprs}, Ctx);
expr({map1,_Line, Map, UpdateAssocs}, Ctx) ->
    OMap = expr(Map, Ctx),
    update_assocs(OMap, UpdateAssocs, Ctx);
expr(E={match,_Line,_Pat,_}, Ctx) ->
    expr_seq([E], Ctx);
expr({block,_Line,Es}, Ctx) ->
    expr_seq(Es, Ctx);
expr(E={'if',Line,_Clauses}, _Ctx) ->
    throw({erl2ocaml_error, {Line, {'not_supported_syntax', E, "if expression"}}});
expr({'case',_Line,Expr,Clauses}, Ctx) ->
    [{"("}, {"match"}] ++ [expr(Expr, Ctx)] ++ [{"with"}] ++ [clauses(Clauses, Ctx)] ++ [{")"}];
expr(E={'fun',Line,Body}, Ctx) ->
    case Body of
        {clauses,Clauses} ->
            [{"("}, {"function"}, clauses(Clauses, Ctx), {")"}];
        {function,F,A} when is_atom(F), is_integer(A) ->
            Fn = atom_to_list(F) ++ "'" ++ integer_to_list(A),
            [{Fn}];
        {function,{atom,_,M},{atom,_,F},{integer,_,A}} when is_atom(M), is_atom(F), is_integer(A) ->
            FQFn = "(" ++ remote_fun(M,F,A,Ctx) ++ ")",
            {FQFn};
        {function,_M,_F,_A} ->
            throw({erl2ocaml_error, {Line, {'not_supported_syntax', E}}})
    end;
expr({named_fun,Line,Name,Clauses}, Ctx) ->
    Exprs = [{match_rec, Line, {var, Line, Name}, {'fun',Line, {clauses, Clauses}}}, {var, Line, Name}],
    expr({block, Line, Exprs}, Ctx);
expr({call,Line,{atom, _, F},As}, Ctx) ->
    Fn = atom_to_list(F) ++ "'" ++ integer_to_list(length(As)),
    [{"("}, {Fn}, expr({tuple1, Line, As}, Ctx), {")"}];
expr({op,_,'.',Rec,{atom,_,K}}, Ctx) ->
    [{"("}, expr(Rec, Ctx), {")"}, {"#" ++ "get_" ++ atom_to_list(K)}];
expr({call,Line,{remote,_Line,{atom,_,M},{atom,_,F}},As}, Ctx) ->
    Arity = length(As),
    FQFn = remote_fun(M,F,Arity,Ctx),
    [{"("}, {FQFn}, expr({tuple1, Line, As}, Ctx), {")"}];
expr({call,Line,F,As}, Ctx) ->
    [{"("}, expr(F, Ctx), {")"}, expr({tuple1, Line, As}, Ctx)];
expr({op,_Line,Op,A}, Ctx) ->
    [{uop(Op)}, {"("}, expr(A, Ctx), {")"}];
expr({op,_Line,'xor',L,R}, Ctx) ->
    [{"("}, {"not"}, {"("}, expr(L, Ctx), {")"}, {")"}, {"<>"}, {"("}, {"not"}, {"("}, expr(R, Ctx), {")"}, {")"}];
expr({op,Line,'--',L,R}, Ctx) ->
    [{"("}, {"Ffi.list_diff'2"}, expr({tuple1, Line, [L, R]}, Ctx), {")"}];
expr({op,_Line,Op,L,R}, Ctx) ->
    [{"("}, expr(L, Ctx), {")"}, {bop(Op)}, {"("}, expr(R, Ctx), {")"}];
expr({enum,_,{op,_,'.',{atom,_,Enum},{atom,_,Ctr}},[]}, _Ctx) ->
    {enum_ctr_name(Enum, Ctr)};
expr({enum,_,{op,_,'.',{op,_,'.',{atom,_,Mod},{atom,_,Enum}},{atom,_,Ctr}},[]}, Ctx) ->
    {enum_ctr_name(Mod, Enum, Ctr, Ctx)};
expr({enum,_,{op,_,'.',{atom,_,Enum},{atom,_,Ctr}},Args}, Ctx) ->
    [{enum_ctr_name(Enum, Ctr)},
        {"("}, interleave1(false, [expr(A, Ctx) || A <- Args]), {")"}];
expr({enum,_,{op,_,'.',{op,_,'.',{atom,_,Mod},{atom,_,Enum}},{atom,_,Ctr}},Args}, Ctx) ->
    [{enum_ctr_name(Mod, Enum, Ctr, Ctx)},
        {"("}, interleave1(false, [expr(A, Ctx) || A <- Args]), {")"}];
expr(E={remote,Line,_M,_F}, _Ctx) ->
    throw({erl2ocaml_error, {Line, {'not_supported_syntax', E}}});
expr(E, _Ctx) ->
    Line = erlang:element(2, E),
    throw({erl2ocaml_error, {Line, {'not_supported_syntax', E}}}).

uop('+') -> "+";
uop('-') -> "-";
uop('not') -> "not";
uop('bnot') -> "lnot".

bop('*') -> "*";
bop('div') -> "/";
bop('rem') -> "mod";
bop('band') -> "land";
bop('and') -> "&&";
bop('+') -> "+";
bop('-') -> "-";
bop('bor') -> "lor";
bop('bxor') -> "lxor";
bop('bsl') -> "lsl";
bop('bsr') -> "lsr";
bop('or') -> "||";
bop('orelse') -> "||";
bop('andalso') -> "&&";
bop('++') -> "@";
bop('==') -> "=";
bop('/=') -> "<>";
bop('=<') -> "<=";
bop('<') -> "<";
bop('>=') -> ">=";
bop('>') -> ">";
bop('=:=') -> "=";
bop('=/=') -> "<>".

remote_fun(M,F,Arity,Ctx) when M == Ctx#context.module ->
    atom_to_list(F) ++ "'" ++ integer_to_list(Arity);
remote_fun(M,F,Arity,_Ctx) ->
    M1 = first_upper(atom_to_list(M)),
    F1 = atom_to_list(F) ++ "'" ++ integer_to_list(Arity),
    M1 ++ "." ++ F1.

init_assoc({map_field_assoc,_Line,{atom,_ALine,A},V}, Ctx) ->
    A1 = atom_to_list(A),
    [
        {"val val_" ++ A1 ++ " = "},
        [expr(V, Ctx)],
        {"method get_" ++ A1 ++ " = val_" ++ A1},
        {"method set_" ++ A1 ++ " new_val_" ++ A1 ++ " = {< " ++ "val_" ++ A1 ++ " =  new_val_" ++ A1 ++ " >}"}
    ];
init_assoc({map_field_assoc,Line,E,_V}, _Ctx) ->
    throw({erl2ocaml_error, {Line, {'not_supported_syntax', E, "non-atom key in map_field_assoc"}}}).

update_assocs(Acc, [A|As], Ctx) ->
    update_assocs(update_assoc(Acc, A, Ctx), As, Ctx);
update_assocs(Acc, [], _Ctx) ->
    Acc.

update_assoc(Acc, {map_field_exact,_Line,{atom,_ALine,A},V}, Ctx) ->
    [{"("}, Acc, {")"}, {"#" ++ "set_" ++ atom_to_list(A)}, expr(V, Ctx)];
update_assoc(_Acc, {map_field_exact,Line,E,_V}, _Ctx) ->
    throw({erl2ocaml_error, {Line, {'not_supported_syntax', E, "non-atom key in map_field_exact"}}});
update_assoc(_Acc, E={map_field_assoc,Line,_K,_V}, _Ctx) ->
    throw({erl2ocaml_error, {Line, {'not_supported_syntax', E, "wrong map_field_assoc"}}}).

type_def_scc(TypeDefs, Ctx) ->
    type_def_scc(true, TypeDefs, Ctx).

type_def_scc(_, [], _Ctx) ->
    "";
type_def_scc(true, [TypeDef|TypeDefs], Ctx) ->
    case api_type_def(TypeDef, Ctx) of
        true ->
            "type " ++ type_def(TypeDef, Ctx) ++ type_def_scc(false, TypeDefs, Ctx);
        false ->
            type_def_scc(true, TypeDefs, Ctx)
    end;
type_def_scc(false, [TypeDef|TypeDefs], Ctx) ->
    case api_type_def(TypeDef, Ctx) of
        true -> "and " ++ type_def(TypeDef, Ctx) ++ type_def_scc(false, TypeDefs, Ctx);
        false -> type_def_scc(false, TypeDefs, Ctx)
    end.

api_type_def({_, {N,_T,TVs}}, Ctx) ->
    case Ctx#context.mode of
        'priv' ->
            true;
        'pub'  ->
            TypeKey = {N, erlang:length(TVs)},
            lists:member(TypeKey, Ctx#context.export_types)
    end.

type_def({enum, {N,T,TVs}}, Ctx) ->
    EnumAlias = enum_alias(N, TVs, Ctx),
    EnumDef = enum_def_lhs(N, TVs, Ctx) ++ " = " ++ enum_ctr_defs(N, T, Ctx) ++ "\n",
    EnumAlias ++ "\n" ++ "and " ++ EnumDef;
type_def({Kind, {N,T,TVs}}, Ctx) ->
    type_def_lhs(N, TVs, Ctx) ++ type_def_rhs(Kind, T, Ctx) ++ "\n".

enum_def_lhs(N, [], _Ctx) ->
    atom_to_list(N);
enum_def_lhs(N, [TV], Ctx) ->
    type(TV, Ctx) ++ " " ++ atom_to_list(N);
enum_def_lhs(N, TVs, Ctx) ->
    "(" ++ interleave(false, ", ", [type(TV, Ctx) || TV <- TVs])  ++ ") " ++ atom_to_list(N).

enum_alias(N, TVs=[], _Ctx) ->
    type_name(N, TVs) ++ " = " ++ atom_to_list(N);
enum_alias(N, TVs=[TV], Ctx) ->
    type(TV, Ctx) ++ " " ++ type_name(N, TVs) ++ " = " ++ type(TV, Ctx) ++ " " ++ atom_to_list(N);
enum_alias(N, TVs, Ctx) ->
    Vars = "(" ++ interleave(false, ", ", [type(TV, Ctx) || TV <- TVs])  ++ ") ",
    Vars ++ type_name(N, TVs) ++ " = " ++ Vars ++ atom_to_list(N).

type_def_lhs(N, TVs=[], _Ctx) ->
    type_name(N, TVs);
type_def_lhs(N, TVs=[TV], Ctx) ->
    type(TV, Ctx) ++ " " ++ type_name(N, TVs);
type_def_lhs(N, TVs, Ctx) ->
    "(" ++ interleave(false, ", ", [type(TV, Ctx) || TV <- TVs])  ++ ") " ++ type_name(N, TVs).

type_def_rhs(alias, T, Ctx) ->
    " = " ++ type(T, Ctx);
type_def_rhs('opaque', T, Ctx) ->
    case Ctx#context.mode of
        'pub' -> "";
        'priv' -> " = " ++ type(T, Ctx)
    end.

enum_ctr_defs(EnumN, {type,_Ln,union, CtrDefs}, Ctx) ->
    interleave(false, " | ", [enum_ctr_def(EnumN, CtrDef, Ctx) || CtrDef <- CtrDefs]);
enum_ctr_defs(EnumN, CtrDef, Ctx) ->
    enum_ctr_def(EnumN, CtrDef, Ctx).

enum_ctr_def(EnumN, {type,_,enum,[{atom,_,CtrN}]}, _Ctx) ->
    enum_ctr_name(EnumN, CtrN);
enum_ctr_def(EnumN, {type,_,enum,[{atom,_,CtrN}| Ts]}, Ctx) ->
    enum_ctr_name(EnumN, CtrN)
        ++ " of "
        ++ interleave(false, " * ", ["(" ++ type(T, Ctx) ++ ")" || T <- Ts]).

enum_ctr_name(EnumN, CtrN) ->
    first_upper(atom_to_list(EnumN)) ++ "'" ++ first_upper(atom_to_list(CtrN)).

enum_ctr_name(ModN, EnumN, CtrN, Ctx) when ModN == Ctx#context.module ->
    enum_ctr_name(EnumN, CtrN);
enum_ctr_name(ModN, EnumN, CtrN, _Ctx) ->
    first_upper(atom_to_list(ModN)) ++ "." ++
        first_upper(atom_to_list(EnumN)) ++ "'" ++
        first_upper(atom_to_list(CtrN)).

erl2ocaml_spec({attribute,_Line,spec,{{Name,Arity},[FT]}}, Ctx) ->
    {atom_to_list(Name) ++ "'" ++ integer_to_list(Arity), type(FT, Ctx)};
erl2ocaml_spec({attribute,Line,spec,Spec}, _) ->
    throw({erl2ocaml_error, {Line, {'not_supported_syntax', Spec, "spec in not supported format"}}}).

type_name(Name, Args) ->
    atom_to_list(Name) ++ "'" ++ integer_to_list(length(Args)).

type({ann_type,_Ln,[_Var,Tp]}, Ctx) ->
    type(Tp, Ctx);
type({type,Ln,any,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,any},[]]}, Ctx);
type({type,Ln,atom,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,atom},[]]}, Ctx);
type({type,Ln,binary,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,binary},[]]}, Ctx);
type({type,Ln,bitstring,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,bitstring},[]]}, Ctx);
type({type,Ln,byte,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,byte},[]]}, Ctx);
type({type,Ln,identifier,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,identifier},[]]}, Ctx);
type({type,Ln,iodata,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,iodata},[]]}, Ctx);
type({type,Ln,iolist,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,iolist},[]]}, Ctx);
type({type,Ln,neg_integer,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,neg_integer},[]]}, Ctx);
type({type,Ln,node,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,node},[]]}, Ctx);
type({type,Ln,non_neg_integer,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,non_neg_integer},[]]}, Ctx);
type({type,Ln,none,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,none},[]]}, Ctx);
type({type,Ln,no_return,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,no_return},[]]}, Ctx);
type({type,Ln,number,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,number},[]]}, Ctx);
type({type,Ln,pid,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,pid},[]]}, Ctx);
type({type,Ln,port,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,port},[]]}, Ctx);
type({type,Ln,pos_integer,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,pos_integer},[]]}, Ctx);
type({type,Ln,reference,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,reference},[]]}, Ctx);
type({type,Ln,term,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,term},[]]}, Ctx);
type({type,Ln,timeout,[]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,timeout},[]]}, Ctx);
type({type,_Line,'fun',[{type,_,product,[A]},B]}, Ctx) ->
    T1 = "(" ++ type(A, Ctx) ++ ")",
    T2 = "(" ++ type(B, Ctx) ++ ")",
    T1 ++ " ->" ++ T2;
type({type,_Line,'fun',[{type,Lt,product,As},B]}, Ctx) ->
    T1 = "(" ++ type({type,Lt,tuple,As}, Ctx) ++ ")",
    T2 = "(" ++ type(B, Ctx) ++ ")",
    T1 ++ " ->" ++ T2;
type({type, _,integer,[]}, _Ctx) ->
    "int";
type({type, _,float,[]}, _Ctx) ->
    "float";
type({type, _,string,[]}, _Ctx) ->
    "string";
type({type, _,char,[]}, _Ctx) ->
    "char";
type({type, _,boolean,[]}, _Ctx) ->
    "bool";
type({type, _,list,[T]}, Ctx) ->
    "(" ++ type(T, Ctx) ++ ") list";
type({type,Ln,map,[{type,_,map_field_assoc,[KT,VT]}]}, Ctx) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,map},[KT,VT]]}, Ctx);
type({type,_,map,Assocs}, Ctx) ->
    {Suffix,Assocs1} =
        case lists:reverse(Assocs) of
            [{type,_,map_field_exact,[{var,_,'_'},{var,_,'_'}]}|T] ->
                {[".."],lists:reverse(T)};
            _ ->
                {[], Assocs}
        end,
    Counter = erlang:get('rec_tv_gen'),
    erlang:put('rec_tv_gen', Counter + 1),
    RecTV = "'rec_tv__" ++ integer_to_list(Counter),
    AssocsTypes = lists:map(fun(A) -> map_field_type(A, RecTV, Ctx) end, Assocs1),
    "< " ++ interleave(false, " ; ", AssocsTypes ++ Suffix) ++ " > as " ++ RecTV;
type({var,_Line,'_'}, _Ctx) ->
    "_";
type({var,_Line,V}, _Ctx) ->
    "'t" ++ erlang:atom_to_list(V);
type({type,_Line,tuple,[]}, _Ctx) ->
    "unit";
type({type,_Line,tuple,[T]}, Ctx) ->
    "(" ++ type(T, Ctx) ++ ") Ffi.tuple1'1";
type({type,_Line,tuple,TS}, Ctx) ->
    TSStrings = lists:map(fun(T) -> "(" ++ type(T, Ctx) ++ ")" end, TS),
    interleave(false, " * ", TSStrings);
type({user_type,_,N,Ts=[]}, _Ctx) ->
    type_name(N, Ts);
type({user_type,_,N,Ts}, Ctx) ->
    Ts1 = lists:map(fun(T) -> "(" ++ type(T, Ctx) ++ ")" end, Ts),
    "(" ++ interleave(false, " , ", Ts1) ++ ") " ++ type_name(N, Ts);
type({remote_type,Line,[{atom,_,M},{atom,_,N},Ts]}, Ctx) when M == Ctx#context.module ->
    type({user_type,Line,N,Ts}, Ctx);
type({remote_type,_,[{atom,_,M},{atom,_,N},Ts=[]]}, _Ctx) ->
    first_upper(atom_to_list(M)) ++ "." ++ type_name(N, Ts);
type({remote_type,_,[{atom,_,M},{atom,_,N},Ts]}, Ctx) ->
    Ts1 = lists:map(fun(T) -> "(" ++ type(T, Ctx) ++ ")" end, Ts),
    "(" ++ interleave(false, " , ", Ts1) ++ ") " ++ first_upper(atom_to_list(M)) ++ "." ++ type_name(N, Ts);
type(T, _Ctx) ->
    Line = erlang:element(2, T),
    throw({erl2ocaml_error, {Line, {'not_supported_syntax', T}}}).

map_field_type({type,_,map_field_exact,[{atom,_,K},VT]}, SelfType, Ctx) ->
    VType = type(VT, Ctx),
    "get_" ++ atom_to_list(K) ++ " : " ++ VType ++ "; set_" ++ atom_to_list(K) ++ " : (" ++ VType ++ ") ->"  ++ SelfType;
map_field_type(Type={type,Line,_,_}, _SelfType, _Ctx) ->
    throw({erl2ocaml_error, {Line, {'not_supported_syntax', Type}}}).

%% naive indentation of documents
indent_docs([Doc|Docs]) ->
    IDoc = indent_doc("", Doc),
    [IDoc|indent_docs(Docs)];
indent_docs([]) ->
    [].

indent_doc(Indent, [{DocStr}|Docs]) ->
    IDoc = Indent ++ DocStr ++ "\n",
    [IDoc|indent_doc(Indent, Docs)];
indent_doc(Indent, [Doc|Docs]) ->
    IDoc = indent_doc(Indent ++ "    ", Doc),
    [IDoc|indent_doc(Indent, Docs)];
indent_doc(_Indent, []) ->
    [].

%% interleaves a list of strings using sep
interleave(_, _, []) ->
    [];
interleave(false, Sep, [E|ES]) ->
    [E | interleave(true, Sep, ES)];
interleave(true, Sep, [E|ES]) ->
    [Sep | [E | interleave(true, Sep, ES)]].

%% interleaves a list of documents using `{", "}` as a separator
interleave1(_, []) ->
    [];
interleave1(false, [E|ES]) ->
    [E | interleave1(true, ES)];
interleave1(true, [E|ES]) ->
    [{", "} | [E | interleave1(true, ES)]].

%% identifier -> Identifier
first_upper([]) ->
    [];
first_upper([H|T]) ->
    string:uppercase([H]) ++ T.

get_fns([]) ->
    [];
get_fns([{attribute,_,etc,skip}|[_|Forms]]) ->
    get_fns(Forms);
get_fns([F={function,_,_,_,_}|Forms]) ->
    [F| get_fns(Forms)];
get_fns([_|Forms]) ->
    get_fns(Forms).

get_specs([]) ->
    [];
get_specs([{attribute,_,spec,_}=Spec|Forms]) ->
    [Spec|get_specs(Forms)];
get_specs([_|Forms]) ->
    get_specs(Forms).

get_module(Forms) ->
     erlang:hd([M || {attribute,_,module,M} <- Forms]).

get_type_defs([]) ->
    [];
get_type_defs([{attribute,_,type,Type}|Forms]) ->
    [{alias,Type}|get_type_defs(Forms)];
get_type_defs([{attribute,_,enum,Type}|Forms]) ->
    [{enum,Type}|get_type_defs(Forms)];
get_type_defs([{attribute,_,opaque,Type}|Forms]) ->
    [{opaque,Type}|get_type_defs(Forms)];
get_type_defs([_|Forms]) ->
    get_type_defs(Forms).

get_exports(Forms) ->
    lists:flatten([Fs || {attribute,_,export,Fs} <- Forms]).

get_export_types(Forms) ->
    lists:flatten([Ts || {attribute,_,export_type,Ts} <- Forms]).

mk_sccs(Functions) ->
    Graph = digraph:new(),
    VFuns = lists:map(
        fun(Fun) ->
            V = {get_fn_name(Fun), get_fn_arg_len(Fun)},
            digraph:add_vertex(Graph,V),
            {V,Fun}
        end,
        Functions),
    % For every function
    lists:map(
        fun(Fun) ->
            Clauses = get_fn_clauses(Fun),
            % a function needs the type of the function calls in it's body
            % hence edges are of from FromV -(needed_by)-> ToV
            ToV = {get_fn_name(Fun), get_fn_arg_len(Fun)},
            % For every clause
            lists:map(
                fun(C) ->
                    % get calls in a clause
                    FromVs = get_calls_from_clause(C),
                    % add all the calls as edges to the graph
                    lists:map(
                        fun(FromV)->
                            digraph:add_edge(Graph,FromV,ToV)
                        end,
                        FromVs)
                end,
                Clauses)
        end,
        Functions),
    SCCs = digraph_utils:strong_components(Graph),
    FunsIndex = lists:map(fun(Fun) -> {get_fn_name(Fun), get_fn_arg_len(Fun)} end, Functions),
    OFuns = lists:foldl(fun(F, Map) -> Map#{F => maps:size(Map)} end, #{}, FunsIndex),
    SCCs1 = lists:map(
        fun(SCC) -> lists:sort(fun(A, B) -> maps:get(A, OFuns) < maps:get(B, OFuns) end, SCC) end,
        SCCs),
    SCCs2 = lists:sort(fun([F1|_],[F2|_]) -> maps:get(F1, OFuns) < maps:get(F2, OFuns) end, SCCs1),
    SCCGraph = build_scc_graph(Graph, SCCs2),
    SortedSCCs1 = topsort(SCCs2, SCCGraph),
    lists:map(
        fun(SCC) ->
            lists:map(
                fun(V) ->
                    element(2,lists:keyfind(V,1,VFuns))
                end,
                SCC)
        end,
        SortedSCCs1).

topsort([], _) ->
    [];
topsort(SCCs, SCCGraph) ->
    {[Free|_], _} = lists:partition(
        fun(SCC) ->
            In = digraph:in_neighbours(SCCGraph, SCC),
            TrueIn = lists:filter(fun(E) -> E =/= SCC end, In),
            TrueIn == []
        end,
        SCCs),
    {_,Rest} = lists:partition(fun(SCC) -> SCC == Free end,SCCs),
    digraph:del_vertex(SCCGraph, Free),
    [Free] ++ topsort(Rest, SCCGraph).

get_calls_from_clause(Clause) ->
    get_calls_from_body(element(5,Clause)).

get_calls_from_body(Exprs) ->
    lists:concat(
        lists:map(fun get_calls_from_expr/1,Exprs)).

get_calls_from_expr({call,_,{atom,_,Fn},FnArgs}) ->
    [ {Fn,length(FnArgs)} |
        lists:concat(
            lists:map(fun get_calls_from_expr/1, FnArgs))];
get_calls_from_expr({'fun',_,{function,Fn,Arity}}) ->
    [ {Fn,Arity} ];
get_calls_from_expr(E) when is_tuple(E) ->
    Es_ = erlang:tuple_to_list(E),
    lists:concat(lists:map(fun get_calls_from_expr/1, Es_));
get_calls_from_expr(Es) when is_list(Es) ->
    lists:concat(lists:map(fun get_calls_from_expr/1, Es));
get_calls_from_expr(_) -> [].

-spec build_scc_graph(digraph:graph(),[[digraph:vertex()]]) -> digraph:graph().
build_scc_graph(FunDGraph,SCCs) ->
    SCCGraph = digraph:new(),
    % add all SCC vertices to SCCGraph
    lists:map(fun(V) -> digraph:add_vertex(SCCGraph,V) end, SCCs),
    % add edges between SCCs using the (component) edges between members
    % If an edge exists between members of two different SCCs,
    % then there exists an edge between the two SCCs in the exact same direction.
    lists:map(
        fun(FromSCC) ->
            % for every element of an SCC
            lists:map(
                fun(FunV) ->
                    % find (dependancies) neighbours that need the type of this FunV
                    % (FunV -needed_by-> Neighbour)
                    Neighbours = digraph:out_neighbours(FunDGraph,FunV),
                    % find parents of neighbours
                    ToSCCs = lists:map(fun(Nb) -> find_parent_sCC(SCCs,Nb) end, Neighbours),
                    lists:map(
                        fun(ToSCC) ->
                            digraph:add_edge(SCCGraph,FromSCC,ToSCC)
                        end,
                        ToSCCs)
                end,
                FromSCC)
        end,
        SCCs),
    SCCGraph.

find_parent_sCC(SCCs,FunV) ->
    Parents = lists:dropwhile(
        fun(SCC) -> not lists:member(FunV,SCC) end, SCCs),
    lists:nth(1,Parents).

get_fn_name(Fun) -> element(3,Fun).
get_fn_arg_len(Fun) -> element(4,Fun).
get_fn_clauses(Fun) -> element(5,Fun).

st_deps(Forms) ->
    Module = get_module(Forms),
    Remotes = collect(Forms, fun st_pred/1, fun remote/1),
    [{L, M} || {L, M} <- Remotes, M =/= ffi, M =/= Module].

ffi_deps(Forms) ->
    Module = get_module(Forms),
    Remotes = collect(Forms, fun ffi_pred/1, fun remote/1),
    [{L, M} || {L, M} <- Remotes, M =/= ffi, M =/= Module].

%% remote call
remote({remote,_Ln,{atom,Ln,Mod},_}) ->
    {Ln, Mod};
%% remote type
remote({remote_type,_Ln,[{atom,Ln,Mod}|_]}) ->
    {Ln, Mod};
%% remote enum ctr
remote({enum,_,{op,_,'.',{op,_,'.',{atom,Ln,Mod},{atom,_,_Enum}},{atom,_,_Ctr}},_Args}) ->
    {Ln, Mod};
%% fn mod:f/n
remote({'fun',_Ln,{function,{atom,Ln,Mod},{atom,_,F},{integer,_,A}}}) when is_atom(Mod),is_atom(F),is_integer(A) ->
    {Ln, Mod};
remote(_Form) ->
    false.

st_pred(_Form) ->
    true.

%% ffi is not interested in function bodies
ffi_pred({function,_,_,_,_}) ->
    false;
ffi_pred(_) ->
    true.

collect(Forms, Pred, Collect) ->
    do_collect(Forms, Pred, Collect, []).

do_collect([], _Pred, _Collect, Acc) ->
    Acc;
do_collect([H|T], Pred, Collect, Acc) ->
    Acc1 = do_collect(T, Pred, Collect, Acc),
    do_collect(H, Pred, Collect, Acc1);
do_collect(X, Pred, Collect, Acc) when is_tuple(X) ->
    case Pred(X) of
        true ->
            Acc1 = do_collect(tuple_to_list(X), Pred, Collect, Acc),
            case Collect(X) of
                false -> Acc1;
                Delta -> [Delta|Acc1]
            end;
        false ->
            Acc
    end;
do_collect(_X, _Pred, _Collect, Acc) ->
    Acc.

format_error({'unspeced_fun', N, A}) ->
    Fun = atom_to_list(N) ++ "/" ++ integer_to_list(A),
    io_lib:format("-lang([erl2, st]): Exported function ~s doesn't have any spec", [Fun]);
format_error({'not_supported_syntax', Form, Details}) ->
    io_lib:format("-lang([erl2, st]): Not supported syntax (~s): ~p", [Details, Form]);
format_error({'not_supported_syntax', Form}) ->
    io_lib:format("-lang([erl2, st]): Not supported syntax: ~p", [Form]).
