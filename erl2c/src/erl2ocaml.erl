-module(erl2ocaml).

-mode(compile).
-export([main/1]).

main(["-in", InFile, "-out", OutFile]) ->
    {ok, Forms} = epp:parse_file(InFile, []),
    RawDocs = erl2ocaml(Forms),
    %% io:format("RawDocs:\n~p\n", [RawDocs]),
    IndentedDocs = indent_docs(RawDocs),
    file:write_file(OutFile, iolist_to_binary(IndentedDocs));
main(["-ast", File]) ->
    {ok, Forms} = epp:parse_file(File, []),
    io:format("Forms:\n~p\n", [Forms]);
main(_) ->
    usage().

usage() ->
    io:format("usage:\n"),
    io:format("  erl2ocaml -in InputFile.erl -out OutputFile.ml:\n").

erl2ocaml(Forms) ->
    AuxFuns = [[{"let same''2(x, y) = x == y"}]],
    GenFuns = [OcamlForm || Form <- Forms, OcamlForm <- [form(Form)], OcamlForm =/= []],
    AuxFuns ++ GenFuns.

form({attribute, _Line, _AttrName, _AttrValue}) ->
    [];
form({eof, _}) ->
    [];
form({function, _Line, Name, Arity, Clauses}) ->
    function(Name, Arity, Clauses).

function(Name, Arity, Clauses) ->
    NameStr = atom_to_list(Name) ++ "'" ++ integer_to_list(Arity),
    [{"let " ++ NameStr ++ " = function"}, clauses(Clauses), {""}].

clauses([EC|Cs]) ->
    OC = clause(EC),
    OC ++ clauses(Cs);
clauses([]) -> [].

clause({clause, Line, Head, Guard, Body}) ->
    case Guard of
        [] -> ok;
        _ -> erlang:error({not_supported, Line, guards, Guard})
    end,
    OHead = {"| " ++ head(Head) ++ " ->" },
    [OHead, expr_seq(Body)].

head(Ps) ->
    OPs = patterns(Ps),
    Ops1 = interleave(false, OPs),
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
pattern({string,_Line,S}) ->
    "\"" ++ S ++ "\"";
pattern({nil,_Line}) ->
    "[]";
pattern({cons,_Line,H,T}) ->
    "(" ++ pattern(H) ++  "::" ++ pattern(T) ++ ")";
pattern({tuple,_Line,Ps}) ->
    OPs = patterns(Ps),
    Ops1 = interleave(false, OPs),
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
expr({tuple,_Line,Es}) ->
    Docs0 = lists:map(fun expr/1, Es),
    Docs1 = interleave1(false, Docs0),
    Docs2 = [{"("}, Docs1, {")"}],
    Docs2;
expr({map,_Line, InitAssocs}) ->
    Fields =  lists:map(fun init_assoc/1, InitAssocs),
    [{"object"}, Fields, {"end"}];
expr({map,Line, Map, UpdateAssocs}) ->
    SameExprs = [{'call',Line, {atom, Line, 'same\''}, [Map,{map1,Line, Map, [UA]}]} || UA <- UpdateAssocs],
    Exprs = SameExprs ++ [{map1,Line, Map, UpdateAssocs}],
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
    [{"("}, {Fn}, expr({tuple, Line, As}), {")"}];
expr({call,_Line,{remote,_Line,{atom,_,maps},{atom,_,get}},[{atom,_,K}, M]}) ->
    MExp = expr(M),
    [{"("}, MExp, {")"}, {"#" ++ "get_" ++ atom_to_list(K)}];
expr({call,Line,{remote,_Line,{atom,_,M},{atom,_,F}},As}) ->
    Arity = length(As),
    FQFn = remote_fun(M,F,Arity),
    [{"("}, {FQFn}, expr({tuple, Line, As}), {")"}];
expr({call,Line,F,As}) ->
    [{"("}, expr(F), {")"}, expr({tuple, Line, As})];
expr({op,Line,_Op,_A0}) ->
    %% TODO - unary operations
    erlang:error({not_supported, Line, unary_operation});
expr({op,Line,_Op,_L,_R}) ->
    %% TODO - unary operations
    erlang:error({not_supported, Line, binary_operation});
expr(E={remote,Line,_M,_F}) ->
    erlang:error({not_supported, Line, E});
expr(Exp) ->
    erlang:error({not_supported, erlang:element(2, Exp), expr, Exp}).

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

%% interleaves a list of strings using ", " as a separator
interleave(_, []) ->
    [];
interleave(false, [E|ES]) ->
    [E | interleave(true, ES)];
interleave(true, [E|ES]) ->
    [", " | [E | interleave(true, ES)]].

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
