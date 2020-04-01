-module(erl2ocaml).

-export([main/1]).

main(["-erl", InFile, "-ml", MlFile, "-mli", MliFile]) ->
    swap_erl_parse(),
    {ok, Forms} = epp:parse_file(InFile, []),
    Funs = get_fns(Forms),
    SortedFuns = mk_sccs(Funs),
    RawDocs = erl2ocaml_sccs(SortedFuns),
    %% io:format("RawDocs:\n~p\n", [RawDocs]),
    FunLines = indent_docs(RawDocs),
    Specs = get_specs(Forms),
    SpecLines = lists:map(fun erl2ocaml_spec/1, Specs),
    %% TODO - this also requires SCCs
    TypeDefs = get_type_defs(Forms),
    TypeDefLines = type_def_scc(TypeDefs),
    InterfaceLines = TypeDefLines ++ SpecLines,
    ImplementationLines = TypeDefLines ++ FunLines,
    file:write_file(MliFile, iolist_to_binary(InterfaceLines)),
    file:write_file(MlFile, iolist_to_binary(ImplementationLines));
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

swap_erl_parse() ->
    code:purge(erl_parse),
    true = code:delete(erl_parse),
    {_,Code,File} = code:get_object_code(erl_parse),
    {module, _Name} = code:load_binary(erl_parse, File,Code).

usage() ->
    io:format("usage:\n"),
    io:format("  erl2ocaml -erl mod.erl -ml mod.ml -mli mod.mli:\n").

erl2ocaml_sccs(SCCs) ->
    lists:flatmap(fun erl2ocaml_scc/1, SCCs).

erl2ocaml_scc(Funs) ->
    erl2ocaml_scc1(true, Funs).

erl2ocaml_scc1(_, []) ->
    [];
erl2ocaml_scc1(true, [F|Fs]) ->
    [translateFun("let rec ", F) | erl2ocaml_scc1(false, Fs)];
erl2ocaml_scc1(false, [F|Fs]) ->
    [translateFun("and ", F) | erl2ocaml_scc1(false, Fs)].

translateFun(Prefix, {function, _Line, Name, Arity, Clauses}) ->
    function(Prefix, Name, Arity, Clauses).

function(Prefix, Name, Arity, Clauses) ->
    NameStr = atom_to_list(Name) ++ "'" ++ integer_to_list(Arity),
    [{Prefix ++ NameStr ++ " = function"}, clauses(Clauses), {""}].

clauses([EC|Cs]) ->
    OC = clause(EC),
    OC ++ clauses(Cs);
clauses([]) -> [].

clause({clause, Line, Head, Guard, Body}) ->
    case Guard of
        [] ->
            OHead = {"| " ++ head(Head) ++ " ->" },
            [OHead, expr_seq(Body)];
        Guards ->
            [Guard1|Guards1] =
                lists:map(
                    fun([E0|Es]) ->
                        lists:foldl(fun(H,Acc) -> {op, Line, 'and', Acc, H} end, E0, Es)
                    end,
                    Guards),
            GuardExp = lists:foldl(fun(H,Acc) -> {op, Line, 'or', Acc, H} end, Guard1, Guards1),
            [{"| " ++ head(Head) ++ " when" }, [expr(GuardExp)], {"->"}, expr_seq(Body)]
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

expr_seq([E={match,Line,_,_}]) ->
    erlang:error({not_supported, Line, last_match_expr, E});
expr_seq([E0]) ->
    E1 = expr(E0),
    [E1];
expr_seq(ES)->
    expr1([], ES).

expr1([], [E={match,Line,_,_}]) ->
    erlang:error({not_supported, Line, last_match_expr, E});
expr1(Acc, [E]) ->
    Delta = [expr(E)],
    Acc ++ Delta;
expr1(Acc, [{match,_Line,P,E}|Es]) ->
    PatString = pattern(P),
    ExprDoc = expr(E),
    Delta = [{"let " ++ PatString ++ " = "}] ++ [ExprDoc] ++ [{"in"}],
    Acc1 = Acc ++ Delta,
    expr1(Acc1, Es);
expr1(Acc, [{match_rec,_Line,P,E}|Es]) ->
    PatString = pattern(P),
    ExprDoc = expr(E),
    Delta = [{"let rec " ++ PatString ++ " = "}] ++ [ExprDoc] ++ [{"in"}],
    Acc1 = Acc ++ Delta,
    expr1(Acc1, Es);
expr1(Acc, [E|Es]) ->
    ExprDoc = expr(E),
    Delta = [{"let _ = "}] ++ [ExprDoc] ++ [{"in"}],
    Acc1 = Acc ++ Delta,
    expr1(Acc1, Es).

expr({var,_Line,V}) ->
    {"v_" ++ atom_to_list(V)};
expr({integer,_Line,I}) ->
    {integer_to_list(I)};
expr({float,_Line,F}) ->
    {float_to_list(F, [{decimals, 0}])};
expr({atom,_Line,true}) ->
    {"true"};
expr({atom,_Line,false}) ->
    {"false"};
expr({atom,Line,A}) ->
    erlang:error({not_supported, Line, atom, A});
expr({string,_Line,S}) ->
    {"\"" ++ S ++ "\""};
expr({char,_Line,C}) ->
    {"'" ++ io_lib:format("~c", [C]) ++ "'"};
expr({nil,_Line}) ->
    {"[]"};
expr({cons,_Line,H,T}) ->
    OH = expr(H),
    OT = expr(T),
    [{"("}, OH, {"::"}, OT, {")"}];
expr(E={lc,Line,_,_}) ->
    erlang:error({not_supported, Line, list_comprehension, E});
expr(E={bc,Line,_,_}) ->
    erlang:error({not_supported, Line, binary_comprehension, E});
expr({tuple,_Line,[E]}) ->
    [{"Ffi.Tuple1("}, [expr(E)], {")"}];
expr({tuple,_Line,Es}) ->
    expr({tuple1,_Line,Es});
expr({tuple1,_Line,Es}) ->
    Docs0 = lists:map(fun expr/1, Es),
    Docs1 = interleave1(false, Docs0),
    Docs2 = [{"("}, Docs1, {")"}],
    Docs2;
expr({map,_Line, InitAssocs}) ->
    Fields =  lists:map(fun init_assoc/1, InitAssocs),
    [{"object"}, Fields, {"end"}];
expr({map,Line,Map,UpdateAssocs}) ->
    FfiSame = {remote, Line, {atom, Line, 'ffi'}, {atom, Line, 'same'}},
    MapsGet = {remote, Line, {atom, Line, 'maps'}, {atom, Line, 'get'}},
    SameFields =
        [{'call',Line,FfiSame,[V,{'call',Line,MapsGet,[K,Map]}]} || {map_field_exact,_,K={atom,_,_},V} <- UpdateAssocs],
    SameMaps =
        [{'call',Line,FfiSame,[Map,{map1,Line, Map, [UA]}]} || UA <- UpdateAssocs],
    Exprs = SameMaps ++ SameFields ++ [{map1,Line, Map, UpdateAssocs}],
    expr({block, Line, Exprs});
expr({map1,_Line, Map, UpdateAssocs}) ->
    OMap = expr(Map),
    update_assocs(OMap, UpdateAssocs);
expr({block,_Line,Es}) ->
    expr_seq(Es);
expr({'if',Line,_Clauses}) ->
    erlang:error({not_supported, Line, if_expression});
expr({'case',_Line,Expr,Clauses}) ->
    [{"("}, {"match"}] ++ [expr(Expr)] ++ [{"with"}] ++ [clauses(Clauses)] ++ [{")"}];
expr({'fun',Line,Body}) ->
    case Body of
        {clauses,Clauses} ->
            [{"("}, {"function"}, clauses(Clauses), {")"}];
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
expr({named_fun,Line,Name,Clauses}) ->
    Exprs = [{match_rec, Line, {var, Line, Name}, {'fun',Line, {clauses, Clauses}}}, {var, Line, Name}],
    expr({block, Line, Exprs});
expr({call,Line,{atom, _, F},As}) ->
    Fn = atom_to_list(F) ++ "'" ++ integer_to_list(length(As)),
    [{"("}, {Fn}, expr({tuple1, Line, As}), {")"}];
expr({call,_Line,{remote,_Line,{atom,_,maps},{atom,_,get}},[{atom,_,K}, M]}) ->
    MExp = expr(M),
    [{"("}, MExp, {")"}, {"#" ++ "get_" ++ atom_to_list(K)}];
expr({call,Line,{remote,_Line,{atom,_,M},{atom,_,F}},As}) ->
    Arity = length(As),
    FQFn = remote_fun(M,F,Arity),
    [{"("}, {FQFn}, expr({tuple1, Line, As}), {")"}];
expr({call,Line,F,As}) ->
    [{"("}, expr(F), {")"}, expr({tuple1, Line, As})];
expr({op,_Line,Op,A}) ->
    [{uop(Op)}, {"("}, expr(A), {")"}];
expr({op,_Line,'xor',L,R}) ->
    [{"("}, {"not"}, {"("}, expr(L), {")"}, {")"}, {"<>"}, {"("}, {"not"}, {"("}, expr(R), {")"}, {")"}];
expr({op,Line,'--',L,R}) ->
    [{"("}, {"Ffi.list_diff'2"}, expr({tuple1, Line, [L, R]}), {")"}];
expr({op,_Line,Op,L,R}) ->
    [{"("}, expr(L), {")"}, {bop(Op)}, {"("}, expr(R), {")"}];
expr({enum,_,{atom,_,Ctr},[]}) ->
    {first_upper(atom_to_list(Ctr))};
expr({enum,_,{remote,_,{atom,_,Mod},{atom,_,Ctr}},[]}) ->
    {first_upper(atom_to_list(Mod)) ++ "." ++ first_upper(atom_to_list(Ctr))};
expr({enum,_,{op,_,'.',{atom,_,Mod},{atom,_,Ctr}},[]}) ->
    {first_upper(atom_to_list(Mod)) ++ "." ++ first_upper(atom_to_list(Ctr))};
expr({enum,_,{atom,_,Ctr},Args}) ->
    [{first_upper(atom_to_list(Ctr))}, {"("}, interleave1(false, lists:map(fun expr/1, Args)), {")"}];
expr({enum,_,{remote,_,{atom,_,Mod},{atom,_,Ctr}},Args}) ->
    [{first_upper(atom_to_list(Mod)) ++ "." ++ first_upper(atom_to_list(Ctr))},
        {"("},
        interleave1(false, lists:map(fun expr/1, Args)),
        {")"}];
expr({enum,_,{op,_,'.',{atom,_,Mod},{atom,_,Ctr}},Args}) ->
    [{first_upper(atom_to_list(Mod)) ++ "." ++ first_upper(atom_to_list(Ctr))},
        {"("},
        interleave1(false, lists:map(fun expr/1, Args)),
        {")"}];
expr(E={remote,Line,_M,_F}) ->
    erlang:error({not_supported, Line, E});
expr(Exp) ->
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

init_assoc({map_field_assoc,_Line,{atom,_ALine,A},V}) ->
    A1 = atom_to_list(A),
    [
        {"val val_" ++ A1 ++ " = "},
        [expr(V)],
        {"method get_" ++ A1 ++ " = val_" ++ A1},
        {"method set_" ++ A1 ++ " new_val_" ++ A1 ++ " = {< " ++ "val_" ++ A1 ++ " =  new_val_" ++ A1 ++ " >}"}
    ];
init_assoc({map_field_assoc,Line,E,_V}) ->
    erlang:error({not_supported, Line, non_atom_key, E}).

update_assocs(Acc, [A|As]) ->
    update_assocs(update_assoc(Acc, A), As);
update_assocs(Acc, []) ->
    Acc.

update_assoc(Acc, {map_field_exact,_Line,{atom,_ALine,A},V}) ->
    [{"("}, Acc, {")"}, {"#" ++ "set_" ++ atom_to_list(A)}, expr(V)];
update_assoc(_Acc, {map_field_exact,Line,E,_V}) ->
    erlang:error({not_supported, Line, non_atom_key, E});
update_assoc(_Acc, E={map_field_assoc,Line,_K,_V}) ->
    erlang:error({not_supported, Line, update_map_field_assoc, E}).

type_def_scc(TypeDefs) ->
    type_def_scc(true, TypeDefs).

type_def_scc(_, []) ->
    "";
type_def_scc(true, [TypeDef|TypeDefs]) ->
    type_def("type", TypeDef) ++ type_def_scc(false, TypeDefs);
type_def_scc(false, [TypeDef|TypeDefs]) ->
    type_def("and", TypeDef) ++ type_def_scc(false, TypeDefs).

type_def(OCamlPrefix, {alias, {N,T,TVs}}) ->
    type_def_lhs(OCamlPrefix, N, TVs) ++ " = " ++ type(T) ++ "\n";
type_def(OCamlPrefix, {enum, {N,T,TVs}}) ->
    type_def_lhs(OCamlPrefix, N, TVs) ++ " = " ++ enum_ctr_defs(T) ++ "\n".

type_def_lhs(OCamlPrefix, N, []) ->
    OCamlPrefix ++ " " ++ atom_to_list(N);
type_def_lhs(OCamlPrefix, N, [TV]) ->
    OCamlPrefix ++ " " ++ type(TV) ++ " " ++ atom_to_list(N);
type_def_lhs(OCamlPrefix, N, TVs) ->
    OCamlPrefix ++ " (" ++ interleave(false, ", ", lists:map(fun type/1, TVs))  ++ ") " ++ atom_to_list(N).

enum_ctr_defs({type,_Ln,union, CtrDefs}) ->
    interleave(false, " | ", lists:map(fun enum_ctr_def/1, CtrDefs));
enum_ctr_defs(CtrDef) ->
    enum_ctr_def(CtrDef).

enum_ctr_def({type,_,enum,[{atom,_,CtrN}]}) ->
    first_upper(atom_to_list(CtrN));
enum_ctr_def({type,_,enum,[{atom,_,CtrN}| Ts]}) ->
    first_upper(atom_to_list(CtrN))
        ++ " of "
        ++ interleave(false, " * ", lists:map(fun(T) -> "(" ++ type(T) ++ ")" end, Ts)).

erl2ocaml_spec({attribute,_Line,spec,{{Name,Arity},[FT]}}) ->
    NameStr = atom_to_list(Name) ++ "'" ++ integer_to_list(Arity),
    FTStr = type(FT),
    "val " ++ NameStr ++ " : " ++ FTStr ++ "\n";
erl2ocaml_spec({attribute,Line,spec,_}) ->
    erlang:error({not_supported, Line, spec}).

get_map_tv([]) -> undefined;
get_map_tv([{type,_,map_field_exact,[{var,_,'_'},{var,_,G}]}]) ->
    "'map_" ++ atom_to_list(G);
get_map_tv([_|As]) -> get_map_tv(As).

gen_map_tv() ->
    Counter =
        case erlang:get('map_tv_counter') of
            'undefined' -> 1;
            C -> C
        end,
    erlang:put('map_tv_counter', Counter + 1),
    "'map_" ++ integer_to_list(Counter).

type({type,Ln,any,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,any},[]]});
type({type,Ln,atom,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,atom},[]]});
type({type,Ln,binary,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,binary},[]]});
type({type,Ln,bitstring,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,bitstring},[]]});
type({type,Ln,byte,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,byte},[]]});
type({type,Ln,identifier,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,identifier},[]]});
type({type,Ln,iodata,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,iodata},[]]});
type({type,Ln,iolist,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,iolist},[]]});
type({type,Ln,neg_integer,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,neg_integer},[]]});
type({type,Ln,node,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,node},[]]});
type({type,Ln,non_neg_integer,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,non_neg_integer},[]]});
type({type,Ln,none,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,none},[]]});
type({type,Ln,no_return,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,no_return},[]]});
type({type,Ln,number,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,number},[]]});
type({type,Ln,pid,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,pid},[]]});
type({type,Ln,port,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,port},[]]});
type({type,Ln,pos_integer,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,pos_integer},[]]});
type({type,Ln,reference,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,reference},[]]});
type({type,Ln,term,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,term},[]]});
type({type,Ln,timeout,[]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,timeout},[]]});
type({type,_Line,'fun',[{type,_,product,[A]},B]}) ->
    T1 = "(" ++ type(A) ++ ")",
    T2 = "(" ++ type(B) ++ ")",
    T1 ++ " ->" ++ T2;
type({type,_Line,'fun',[{type,Lt,product,As},B]}) ->
    T1 = "(" ++ type({type,Lt,tuple,As}) ++ ")",
    T2 = "(" ++ type(B) ++ ")",
    T1 ++ " ->" ++ T2;
type({type, _,integer,[]}) ->
    "int";
type({type, _,float,[]}) ->
    "float";
type({type, _,string,[]}) ->
    "string";
type({type, _,char,[]}) ->
    "char";
type({type, _,boolean,[]}) ->
    "bool";
type({type, _,list,[T]}) ->
    type(T) ++ " list";
type({type,Ln,map,[{type,_,map_field_assoc,[KT,VT]}]}) ->
    type({remote_type,Ln,[{atom,Ln,ffi},{atom,Ln,map},[KT,VT]]});
type({type,_,map,Assocs}) ->
    {MapTV, Suffix, Assocs1} =
        case get_map_tv(Assocs) of
            'undefined' ->
                {gen_map_tv(), "", Assocs};
            TV ->
                [_|Tmp] = lists:reverse(Assocs),
                {TV, "; ..", lists:reverse(Tmp)}
        end,
    AssocsTypes = lists:map(fun(A) -> map_field_type(A, MapTV) end, Assocs1),
    "< " ++ interleave(false, " ; ", AssocsTypes) ++ Suffix ++ " > as " ++ MapTV;
type({var,_Line,'_'}) ->
    "_";
type({var,_Line,V}) ->
    "'" ++ erlang:atom_to_list(V);
type({type,_Line,tuple,[]}) ->
    "unit";
type({type,_Line,tuple,[T]}) ->
    "(" ++ type(T) ++ ") Ffi.tuple1";
type({type,_Line,tuple,TS}) ->
    TSStrings = lists:map(fun(T) -> "(" ++ type(T) ++ ")" end, TS),
    interleave(false, " * ", TSStrings);
type({user_type,_,N,[]}) ->
    atom_to_list(N);
type({user_type,_,N,Ts}) ->
    Ts1 = lists:map(fun(T) -> "(" ++ type(T) ++ ")" end, Ts),
    "(" ++ interleave(false, " , ", Ts1) ++ ") " ++ atom_to_list(N);
type({remote_type,_,[{atom,_,M},{atom,_,N},[]]}) ->
    first_upper(atom_to_list(M)) ++ "." ++ atom_to_list(N);
type({remote_type,_,[{atom,_,M},{atom,_,N},Ts]}) ->
    Ts1 = lists:map(fun(T) -> "(" ++ type(T) ++ ")" end, Ts),
    "(" ++ interleave(false, " , ", Ts1) ++ ") " ++ first_upper(atom_to_list(M)) ++ "." ++ atom_to_list(N);
type(Type) ->
    Line = erlang:element(2, Type),
    erlang:error({not_supported, Line, Type}).

map_field_type({type,_,map_field_exact,[{atom,_,K},VT]}, SelfType) ->
    VType = type(VT),
    "get_" ++ atom_to_list(K) ++ " : " ++ VType ++ "; set_" ++ atom_to_list(K) ++ " : (" ++ VType ++ ") ->"  ++ SelfType;
map_field_type(Type={type,Line,_,_}, _SelfType) ->
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

get_type_defs([]) ->
    [];
get_type_defs([{attribute,_,type,Type}|Forms]) ->
    [{alias,Type}|get_type_defs(Forms)];
get_type_defs([{attribute,_,opaque,Type}|Forms]) ->
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
