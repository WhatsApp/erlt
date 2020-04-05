-module(erl2ocaml).

-export([erl2ocaml_ffi/1, erl2ocaml_st/1, main/1, ffi/0]).

-record(context, {module :: atom()}).

ffi() ->
    FfiLines = [
        "type 'a tuple1 = Tuple1 of 'a\n",
        "type any\n",
        "type atom\n",
        "type binary\n",
        "type bitstring\n",
        "type byte\n",
        "type identifier\n",
        "type iodata\n",
        "type iolist\n",
        "type ('k, 'v) map\n",
        "type neg_integer\n",
        "type none\n",
        "type non_neg_integer\n",
        "type number\n",
        "type pid\n",
        "type port\n",
        "type pos_integer\n",
        "type reference\n",
        "type term\n",
        "type timeout\n",
        "type node = atom\n",
        "type no_return = none\n",
        "val same'2 : 'a * 'a -> unit\n",
        "val to_string'1 : 'a -> string\n",
        "val list_diff'2 : 'a list * 'a list -> 'a list\n"
    ],
    iolist_to_binary(FfiLines).

main(["-erl", InFile, "-ml", MlFile, "-mli", MliFile]) ->
    swap_erl_parse(),
    {ok, Forms} = epp:parse_file(InFile, []),
    {MliCode, MlCode} = erl2ocaml_st(Forms),
    file:write_file(MliFile, MliCode),
    file:write_file(MlFile, MlCode);
main(["-ast", File]) ->
    swap_erl_parse(),
    {ok, Forms} = epp:parse_file(File, []),
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

swap_erl_parse() ->
    code:purge(erl_parse),
    true = code:delete(erl_parse),
    {_,Code,File} = code:get_object_code(erl_parse),
    {module, _Name} = code:load_binary(erl_parse, File,Code).

erl2ocaml_ffi(Forms) ->
    Ctx = #context{module = get_module(Forms)},
    Specs = get_specs(Forms),
    OTypes = [erl2ocaml_spec(S, Ctx) || S <- Specs],
    SpecLines = ["val " ++ N ++ " : " ++ T ++ "\n" || {N, T} <- OTypes],
    TypeDefs = get_type_defs(Forms),
    TypeDefLines = type_def_scc(TypeDefs, Ctx),
    InterfaceLines = TypeDefLines ++ SpecLines,
    MliCode = iolist_to_binary(InterfaceLines),
    MliCode.

erl2ocaml_st(Forms) ->
    Ctx = #context{module = get_module(Forms)},
    Funs = get_fns(Forms),
    SortedFuns = mk_sccs(Funs),
    Specs = get_specs(Forms),
    OTypes = [erl2ocaml_spec(S, Ctx) || S <- Specs],
    SpecLines = ["val " ++ N ++ " : " ++ T ++ "\n" || {N, T} <- OTypes],
    %% TODO - this also requires SCCs
    TypeDefs = get_type_defs(Forms),
    TypeDefLines = type_def_scc(TypeDefs, Ctx),
    RawDocs = erl2ocaml_sccs(SortedFuns, OTypes, Ctx),
    %% io:format("RawDocs:\n~p\n", [RawDocs]),
    FunLines = indent_docs(RawDocs),
    InterfaceLines = TypeDefLines ++ SpecLines,
    ImplementationLines = TypeDefLines ++ FunLines,
    MliCode = iolist_to_binary(InterfaceLines),
    MlCode = iolist_to_binary(ImplementationLines),
    {MliCode, MlCode}.

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
            OHead = {"| " ++ head(Head) ++ " ->" },
            [OHead, expr_seq(Body, Ctx)];
        Guards ->
            [Guard1|Guards1] =
                lists:map(
                    fun([E0|Es]) ->
                        lists:foldl(fun(H,Acc) -> {op, Line, 'and', Acc, H} end, E0, Es)
                    end,
                    Guards),
            GuardExp = lists:foldl(fun(H,Acc) -> {op, Line, 'or', Acc, H} end, Guard1, Guards1),
            [{"| " ++ head(Head) ++ " when" }, [expr(GuardExp, Ctx)], {"->"}, expr_seq(Body, Ctx)]
    end.

head(Ps) ->
    OPs = patterns(Ps),
    Ops1 = interleave(false, ", ", OPs),
    Docs = ["(", Ops1, ")"],
    OHead = binary_to_list(iolist_to_binary(Docs)),
    OHead.

patterns([P0|Ps]) ->
    P1 = pattern(P0),
    [P1|patterns(Ps)];
patterns([]) -> [].

pattern({var,_Line,'_'}) ->
    "_";
pattern({var,_Line,V}) ->
    "v_" ++ atom_to_list(V);
pattern({match,_Line,P,{var,_Line,V}}) ->
    pattern(P) ++ " as " ++ "v_" ++ atom_to_list(V);
pattern({match,Line,_,MP}) ->
    erlang:error({not_supported, Line, complex_match_pattern, MP});
pattern({integer,_Line,I}) ->
    integer_to_list(I);
pattern({char,_Line,C}) ->
    "'" ++ io_lib:format("~c", [C]) ++ "'";
pattern({float,_Line,F}) ->
    float_to_list(F, [{decimals, 0}]);
pattern({atom,_Line,true}) ->
    "true";
pattern({atom,_Line,false}) ->
    "false";
pattern({atom,Line,A}) ->
    erlang:error({not_supported, Line, atom, A});
pattern({enum,_,{atom,_,Ctr},[]}) ->
    first_upper(atom_to_list(Ctr));
pattern({enum,_,{remote,_,{atom,_,Mod},{atom,_,Ctr}},[]}) ->
    first_upper(atom_to_list(Mod)) ++ "." ++ first_upper(atom_to_list(Ctr));
pattern({enum,_,{op,_,'.',{atom,_,Mod},{atom,_,Ctr}},[]}) ->
    first_upper(atom_to_list(Mod)) ++ "." ++ first_upper(atom_to_list(Ctr));
pattern({enum,_,{atom,_,Ctr},Args}) ->
    first_upper(atom_to_list(Ctr))
        ++ "("
        ++ binary_to_list(iolist_to_binary(interleave(false, ", ", patterns(Args))))
        ++ ")";
pattern({enum,_,{remote,_,{atom,_,Mod},{atom,_,Ctr}},Args}) ->
    first_upper(atom_to_list(Mod)) ++ "." ++ first_upper(atom_to_list(Ctr))
        ++ "("
        ++ binary_to_list(iolist_to_binary(interleave(false, ", ", patterns(Args))))
        ++ ")";
pattern({enum,_,{op,_,'.',{atom,_,Mod},{atom,_,Ctr}},Args}) ->
    first_upper(atom_to_list(Mod)) ++ "." ++ first_upper(atom_to_list(Ctr))
        ++ "("
        ++ binary_to_list(iolist_to_binary(interleave(false, ", ", patterns(Args))))
        ++ ")";
pattern({string,_Line,S}) ->
    "\"" ++ S ++ "\"";
pattern({nil,_Line}) ->
    "[]";
pattern({cons,_Line,H,T}) ->
    "(" ++ pattern(H) ++  "::" ++ pattern(T) ++ ")";
pattern({tuple,_Line,[P]}) ->
    Docs = ["Ffi.Tuple1(", [pattern(P)] , ")"],
    binary_to_list(iolist_to_binary(Docs));
pattern({tuple,_Line,Ps}) ->
    OPs = patterns(Ps),
    Ops1 = interleave(false, ", ", OPs),
    Docs = ["(", Ops1, ")"],
    binary_to_list(iolist_to_binary(Docs));
pattern(Pat = {_,Line,_}) ->
    erlang:error({not_supported, Line, pattern, Pat}).

expr_seq([E={match,Line,_,_}], _Ctx) ->
    erlang:error({not_supported, Line, last_match_expr, E});
expr_seq([E0], Ctx) ->
    E1 = expr(E0, Ctx),
    [E1];
expr_seq(ES, Ctx)->
    expr1([], ES, Ctx).

expr1([], [E={match,Line,_,_}], _Ctx) ->
    erlang:error({not_supported, Line, last_match_expr, E});
expr1(Acc, [E], Ctx)->
    Delta = [expr(E, Ctx)],
    Acc ++ Delta;
expr1(Acc, [{match,_Line,P,E}|Es], Ctx)->
    PatString = pattern(P),
    ExprDoc = expr(E, Ctx),
    Delta = [{"let " ++ PatString ++ " = "}] ++ [ExprDoc] ++ [{"in"}],
    Acc1 = Acc ++ Delta,
    expr1(Acc1, Es, Ctx);
expr1(Acc, [{match_rec,_Line,P,E}|Es], Ctx)->
    PatString = pattern(P),
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
expr({atom,Line,A}, _Ctx) ->
    erlang:error({not_supported, Line, atom, A});
expr({string,_Line,S}, _Ctx) ->
    {"\"" ++ S ++ "\""};
expr({char,_Line,C}, _Ctx) ->
    {"'" ++ io_lib:format("~c", [C]) ++ "'"};
expr({nil,_Line}, _Ctx) ->
    {"[]"};
expr({cons,_Line,H,T}, Ctx) ->
    [{"(("}, expr(H, Ctx), {") :: ("}, expr(T, Ctx), {"))"}];
expr(E={lc,Line,_,_}, _Ctx) ->
    erlang:error({not_supported, Line, list_comprehension, E});
expr(E={bc,Line,_,_}, _Ctx) ->
    erlang:error({not_supported, Line, binary_comprehension, E});
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
expr({block,_Line,Es}, Ctx) ->
    expr_seq(Es, Ctx);
expr({'if',Line,_Clauses}, _Ctx) ->
    erlang:error({not_supported, Line, if_expression});
expr({'case',_Line,Expr,Clauses}, Ctx) ->
    [{"("}, {"match"}] ++ [expr(Expr, Ctx)] ++ [{"with"}] ++ [clauses(Clauses, Ctx)] ++ [{")"}];
expr({'fun',Line,Body}, Ctx) ->
    case Body of
        {clauses,Clauses} ->
            [{"("}, {"function"}, clauses(Clauses, Ctx), {")"}];
        {function,F,A} when is_atom(F), is_integer(A) ->
            Fn = atom_to_list(F) ++ "'" ++ integer_to_list(A),
            [{Fn}];
        {function,M,F,A} when is_atom(M), is_atom(F), is_integer(A) ->
            FQFn = "(" ++ remote_fun(M,F,A) ++ ")",
            {FQFn};
        {function,{atom,_,M},{atom,_,F},{integer,_,A}} when is_atom(M), is_atom(F), is_integer(A) ->
            FQFn = "(" ++ remote_fun(M,F,A) ++ ")",
            {FQFn};
        {function,_M,_F,_A} ->
            %% _A can be a var, we are not supporting such dynamic reference
            erlang:error({not_supported, Line, such_body, Body})
    end;
expr({named_fun,Line,Name,Clauses}, Ctx) ->
    Exprs = [{match_rec, Line, {var, Line, Name}, {'fun',Line, {clauses, Clauses}}}, {var, Line, Name}],
    expr({block, Line, Exprs}, Ctx);
expr({call,Line,{atom, _, F},As}, Ctx) ->
    Fn = atom_to_list(F) ++ "'" ++ integer_to_list(length(As)),
    [{"("}, {Fn}, expr({tuple1, Line, As}, Ctx), {")"}];
expr({op,_,'.',Rec,{atom,_,K}}, Ctx) ->
    [{"("}, expr(Rec, Ctx), {")"}, {"#" ++ "get_" ++ atom_to_list(K)}];
expr({call,Line,{remote,_Line,{atom,_,M},{atom,FLine,F}},As}, Ctx) when M == Ctx#context.module ->
    expr({call,Line,{atom,FLine,F},As}, Ctx);
expr({call,Line,{remote,_Line,{atom,_,M},{atom,_,F}},As}, Ctx) ->
    Arity = length(As),
    FQFn = remote_fun(M,F,Arity),
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
expr({enum,_,{atom,_,Ctr},[]}, _Ctx) ->
    {first_upper(atom_to_list(Ctr))};
expr({enum,_,{remote,_,{atom,_,Mod},{atom,_,Ctr}},[]}, _Ctx) ->
    {first_upper(atom_to_list(Mod)) ++ "." ++ first_upper(atom_to_list(Ctr))};
expr({enum,_,{op,_,'.',{atom,_,Mod},{atom,_,Ctr}},[]}, _Ctx) ->
    {first_upper(atom_to_list(Mod)) ++ "." ++ first_upper(atom_to_list(Ctr))};
expr({enum,_,{atom,_,Ctr},Args}, Ctx) ->
    [{first_upper(atom_to_list(Ctr))}, {"("}, interleave1(false, [expr(A, Ctx) || A <- Args]), {")"}];
expr({enum,_,{remote,_,{atom,_,Mod},{atom,_,Ctr}},Args}, Ctx) ->
    [{first_upper(atom_to_list(Mod)) ++ "." ++ first_upper(atom_to_list(Ctr))},
        {"("}, interleave1(false, [expr(A, Ctx) || A <- Args]), {")"}];
expr({enum,_,{op,_,'.',{atom,_,Mod},{atom,_,Ctr}},Args}, Ctx) ->
    [{first_upper(atom_to_list(Mod)) ++ "." ++ first_upper(atom_to_list(Ctr))},
        {"("}, interleave1(false, [expr(A, Ctx) || A <- Args]), {")"}];
expr(E={remote,Line,_M,_F}, _Ctx) ->
    erlang:error({not_supported, Line, E});
expr(Exp, _Ctx) ->
    erlang:error({not_supported, erlang:element(2, Exp), expr, Exp}).

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

remote_fun(M,F,Arity) ->
    M1 = atom_to_list(M),
    {M2, F1} =
        case string:prefix(M1, "ocaml_") of
            'nomatch' -> {M1, atom_to_list(F) ++ "'" ++ integer_to_list(Arity)};
            Rest -> {Rest, atom_to_list(F)}
        end,
    M3 = first_upper(M2),
    FQFn = M3 ++ "." ++ F1,
    FQFn.

init_assoc({map_field_assoc,_Line,{atom,_ALine,A},V}, Ctx) ->
    A1 = atom_to_list(A),
    [
        {"val val_" ++ A1 ++ " = "},
        [expr(V, Ctx)],
        {"method get_" ++ A1 ++ " = val_" ++ A1},
        {"method set_" ++ A1 ++ " new_val_" ++ A1 ++ " = {< " ++ "val_" ++ A1 ++ " =  new_val_" ++ A1 ++ " >}"}
    ];
init_assoc({map_field_assoc,Line,E,_V}, _Ctx) ->
    erlang:error({not_supported, Line, non_atom_key, E}).

update_assocs(Acc, [A|As], Ctx) ->
    update_assocs(update_assoc(Acc, A, Ctx), As, Ctx);
update_assocs(Acc, [], _Ctx) ->
    Acc.

update_assoc(Acc, {map_field_exact,_Line,{atom,_ALine,A},V}, Ctx) ->
    [{"("}, Acc, {")"}, {"#" ++ "set_" ++ atom_to_list(A)}, expr(V, Ctx)];
update_assoc(_Acc, {map_field_exact,Line,E,_V}, _Ctx) ->
    erlang:error({not_supported, Line, non_atom_key, E});
update_assoc(_Acc, E={map_field_assoc,Line,_K,_V}, _Ctx) ->
    erlang:error({not_supported, Line, update_map_field_assoc, E}).

type_def_scc(TypeDefs, Ctx) ->
    type_def_scc(true, TypeDefs, Ctx).

type_def_scc(_, [], _Ctx) ->
    "";
type_def_scc(true, [TypeDef|TypeDefs], Ctx) ->
    type_def("type", TypeDef, Ctx) ++ type_def_scc(false, TypeDefs, Ctx);
type_def_scc(false, [TypeDef|TypeDefs], Ctx) ->
    type_def("and", TypeDef, Ctx) ++ type_def_scc(false, TypeDefs, Ctx).

type_def(OCamlPrefix, {alias, {N,T,TVs}}, Ctx) ->
    type_def_lhs(OCamlPrefix, N, TVs, Ctx) ++ " = " ++ type(T, Ctx) ++ "\n";
type_def(OCamlPrefix, {enum, {N,T,TVs}}, Ctx) ->
    type_def_lhs(OCamlPrefix, N, TVs, Ctx) ++ " = " ++ enum_ctr_defs(T, Ctx) ++ "\n".

type_def_lhs(OCamlPrefix, N, [], _Ctx) ->
    OCamlPrefix ++ " " ++ atom_to_list(N);
type_def_lhs(OCamlPrefix, N, [TV], Ctx) ->
    OCamlPrefix ++ " " ++ type(TV, Ctx) ++ " " ++ atom_to_list(N);
type_def_lhs(OCamlPrefix, N, TVs, Ctx) ->
    OCamlPrefix ++ " (" ++ interleave(false, ", ", [type(TV, Ctx) || TV <- TVs])  ++ ") " ++ atom_to_list(N).

enum_ctr_defs({type,_Ln,union, CtrDefs}, Ctx) ->
    interleave(false, " | ", [enum_ctr_def(CtrDef, Ctx) || CtrDef <- CtrDefs]);
enum_ctr_defs(CtrDef, Ctx) ->
    enum_ctr_def(CtrDef, Ctx).

enum_ctr_def({type,_,enum,[{atom,_,CtrN}]}, _Ctx) ->
    first_upper(atom_to_list(CtrN));
enum_ctr_def({type,_,enum,[{atom,_,CtrN}| Ts]}, Ctx) ->
    first_upper(atom_to_list(CtrN))
        ++ " of "
        ++ interleave(false, " * ", ["(" ++ type(T, Ctx) ++ ")" || T <- Ts]).

erl2ocaml_spec({attribute,_Line,spec,{{Name,Arity},[FT]}}, Ctx) ->
    {atom_to_list(Name) ++ "'" ++ integer_to_list(Arity), type(FT, Ctx)};
erl2ocaml_spec({attribute,Line,spec,_}, _) ->
    erlang:error({not_supported, Line, spec}).

get_map_tv([]) -> undefined;
get_map_tv([{type,_,map_field_exact,[{var,_,'_'},{var,_,G}]}]) ->
    "'t" ++ atom_to_list(G);
get_map_tv([_|As]) -> get_map_tv(As).

gen_map_tv() ->
    Counter =
        case erlang:get('map_tv_counter') of
            'undefined' -> 1;
            C -> C
        end,
    erlang:put('map_tv_counter', Counter + 1),
    "'row_tv__" ++ integer_to_list(Counter).

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
    {MapTV, Suffix, Assocs1} =
        case get_map_tv(Assocs) of
            'undefined' ->
                {gen_map_tv(), [], Assocs};
            TV ->
                [_|Tmp] = lists:reverse(Assocs),
                {TV, [".."], lists:reverse(Tmp)}
        end,
    AssocsTypes = lists:map(fun(A) -> map_field_type(A, MapTV, Ctx) end, Assocs1),
    "< " ++ interleave(false, " ; ", AssocsTypes ++ Suffix) ++ " > as " ++ MapTV;
type({var,_Line,'_'}, _Ctx) ->
    "_";
type({var,_Line,V}, _Ctx) ->
    "'t" ++ erlang:atom_to_list(V);
type({type,_Line,tuple,[]}, _Ctx) ->
    "unit";
type({type,_Line,tuple,[T]}, Ctx) ->
    "(" ++ type(T, Ctx) ++ ") Ffi.tuple1";
type({type,_Line,tuple,TS}, Ctx) ->
    TSStrings = lists:map(fun(T) -> "(" ++ type(T, Ctx) ++ ")" end, TS),
    interleave(false, " * ", TSStrings);
type({user_type,_,N,[]}, _Ctx) ->
    atom_to_list(N);
type({user_type,_,N,Ts}, Ctx) ->
    Ts1 = lists:map(fun(T) -> "(" ++ type(T, Ctx) ++ ")" end, Ts),
    "(" ++ interleave(false, " , ", Ts1) ++ ") " ++ atom_to_list(N);
type({remote_type,Line,[{atom,_,M},{atom,_,N},Ts]}, Ctx) when M == Ctx#context.module ->
    type({user_type,Line,N,Ts}, Ctx);
type({remote_type,_,[{atom,_,M},{atom,_,N},[]]}, _Ctx) ->
    first_upper(atom_to_list(M)) ++ "." ++ atom_to_list(N);
type({remote_type,_,[{atom,_,M},{atom,_,N},Ts]}, Ctx) ->
    Ts1 = lists:map(fun(T) -> "(" ++ type(T, Ctx) ++ ")" end, Ts),
    "(" ++ interleave(false, " , ", Ts1) ++ ") " ++ first_upper(atom_to_list(M)) ++ "." ++ atom_to_list(N);
type(Type, _Ctx) ->
    Line = erlang:element(2, Type),
    erlang:error({not_supported, Line, Type}).

map_field_type({type,_,map_field_exact,[{atom,_,K},VT]}, SelfType, Ctx) ->
    VType = type(VT, Ctx),
    "get_" ++ atom_to_list(K) ++ " : " ++ VType ++ "; set_" ++ atom_to_list(K) ++ " : (" ++ VType ++ ") ->"  ++ SelfType;
map_field_type(Type={type,Line,_,_}, _SelfType, _Ctx) ->
    erlang:error({not_supported, Line, Type}).

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
get_type_defs([_|Forms]) ->
    get_type_defs(Forms).

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
