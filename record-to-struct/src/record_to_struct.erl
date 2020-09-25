-module(record_to_struct).

-export([format_error/1,parse_transform/2]).

%% Internal state.

-record(rtos, {convert=[],                      %Records to convert
               records=[],                      %Record definitions
               errors=[],
               warnings=[]
              }).

%% format_error(Error) -> Chars.

format_error(Else) ->
    lists:flatten(io_lib:format("Unknown error code ~tw",[Else])).

%% parse_transform(ModuleForms, Options) ->
%%     Forms | {error,Errors,Warnings} | {warning,Forms,Warnings}.

parse_transform(Forms0, _Options) ->
    St0 = #rtos{},
    {Forms1,St1} = forms(Forms0, St0),
    %% io:format("pt: ~p\n", [Forms1]),
    do_return(Forms1, St1).

do_return(Forms, #rtos{errors=[],warnings=[]}) -> Forms;
do_return(Forms, #rtos{errors=[],warnings=Ws}) -> {warning,Forms,Ws};
do_return(_Forms, #rtos{errors=Es,warnings=Ws}) -> {error,Es,Ws}.

forms(Fs, St) ->
    lists:mapfoldl(fun (F, S) -> form(F, S) end, St, Fs).

form({attribute,Line,record,Def}=Form, St0) ->
    St1 = add_record_def(Line, Def, St0),
    {Form,St1};
form({attribute,_Line,records_to_convert_to_structs,Rs}=Form,
     #rtos{convert=Cv}=St) ->
    {Form,St#rtos{convert=Cv ++ Rs}};
form({function,Line,Name,Arity,Cls}, St) ->
    function(Line, Name, Arity, Cls, St);
form(Other, St) ->
    {Other,St}.

function(Line, Name, Arity, Cls0, St0) ->
    {Cls1,St1} = clauses(Cls0, St0),
    {{function,Line,Name,Arity,Cls1},St1}.

clauses(Cls, St) ->
    lists:mapfoldl(fun (C, S) -> clause(C, S) end, St, Cls).

%% clause(Clause, State) -> {Clause, State}.

clause({clause,Line,H0,G0,B0}, St0) ->
    {H1,St1} = head(H0, St0),
    {G1,St2} = guard(G0, St1),
    {B1,St3} = body(B0, St2),
    {{clause,Line,H1,G1,B1},St3}.

head(Ps, St) ->
    lists:mapfoldl(fun (P, S) -> pattern(P, S) end, St, Ps).

guard(Gseq, St) ->
    lists:mapfoldl(fun (G, S) -> exprs(G, S) end, St, Gseq).

body(Es, St) ->
    exprs(Es, St).

exprs(Es, St) ->
    lists:mapfoldl(fun (E, S) -> expr(E, S) end, St, Es).

%% Data types.
expr({cons,Line,H0,T1}, St0) ->
    {H1,St1} = expr(H0, St0),
    {T1,St2} = expr(T1, St1),
    {{cons,Line,H1,T1},St2};
expr({tuple,Line,Es0}, St0) ->
    {Es1,St1} = exprs(Es0, St0),
    {{tuple,Line,Es1},St1};
expr({map,Line,Assocs0}, St0) ->
    {Assocs1,St1} = map_assocs(Assocs0, St0),
    {{map,Line,Assocs1},St1};
expr({map,Line,Expr0,Assocs0}, St0) ->
    {Expr1,St1} = expr(Expr0, St0),
    {Assocs1,St2} = map_assocs(Assocs0, St1),
    {{map,Line,Expr1,Assocs1},St2};
%% The record expressions.
expr({record,Line,Name,Fields}, St) ->
    make_record_struct(Line, Name, Fields, St);
expr({record_field,Line,Expr0,Name,Field}, St0) ->
    {Expr1,St1} = expr(Expr0, St0),
    struct_field(Line, Expr1, Name, Field, St1);
expr({record,Line,Expr0,Name,Fields}, St0) ->
    {Expr1,St1} = expr(Expr0, St0),
    update_struct(Line, Expr1, Name, Fields, St1);
%% Functions.
expr({call,Line,{remote,Rline,Mod0,Fun0},Args0}, St0) ->
    {Mod1,St1} = expr(Mod0, St0),
    {Fun1,St2} = expr(Fun0, St1),
    {Args1,St3} = exprs(Args0, St2),
    {{call,Line,{remote,Rline,Mod1,Fun1},Args1},St3};
expr({call,Line,Fun0,Args0}, St0) ->
    {Fun1,St1} = expr(Fun0, St0),
    {Args1,St2} = exprs(Args0, St1),
    {{call,Line,Fun1,Args1},St2};
expr({'fun',Line,{clauses,Cs0}}, St0) ->
    {Cs1,St1} = clauses(Cs0, St0),
    {{'fun',Line,{clauses,Cs1}},St1};
expr({named_fun,Line,Name,{clauses,Cs0}}, St0) ->
    {Cs1,St1} = clauses(Cs0, St0),
    {{named_fun,Line,Name,{clauses,Cs1}},St1};
%% Forms.
expr({'case',Line,Expr0,Cls0}, St0) ->
    {Expr1,St1} = expr(Expr0, St0),
    {Cls1,St2} = clauses(Cls0, St1),
    {{'case',Line,Expr1,Cls1},St2};
expr({'if',Line,Cls0}, St0) ->
    {Cls1,St1} = clauses(Cls0, St0),
    {{'if',Line,Cls1},St1};
expr({'receive',Line,Cls0}, St0) ->             %Receive with no timeout
    {Cls1,St1} = clauses(Cls0, St0),
    {{'recieve',Line,Cls1},St1};
expr({'receive',Line,To0,Body0,Cls0}, St0) ->   %Receive with timeout
    {Cls1,St1} = clauses(Cls0, St0),
    {To1,St2} = expr(To0, St1),
    {Body1,St3} = body(Body0, St2),
    {{'recieve',Line,Cls1,To1,Body1},St3};
expr({op,Line,Op,Arg10,Arg20}, St0) ->
    {Arg11,St1} = expr(Arg10, St0),
    {Arg21,St2} = expr(Arg20, St1),
    {{op,Line,Op,Arg11,Arg21},St2};
expr({op,Line,Op,Arg0}, St0) ->
    {Arg1,St1} = expr(Arg0, St0),
    {{op,Line,Op,Arg1},St1};
expr({match,Line,Pat0,Val0}, St0) ->
    {Pat1,St1} = expr(Pat0, St0),
    {Val1,St2} = expr(Val0, St1),
    {{match,Line,Pat1,Val1},St2};
%% Everything else, atoms, nil, number, vars.
expr(Expr, St) ->
    {Expr,St}.

pattern(Pat, St) ->
    expr(Pat, St).                              %Maybe

%% convert_record(Name, State) -> true | false.
%% add_record_def(Line, RecordDef, State) -> State.
%% make_record_struct(Line, Name, Fields, State) -> {Struct,State}.
%% struct_field(Line, Expr, Name, Field, State) -> {Value,State}.
%% update_struct(Line, Expr, Name, Fields, State) -> {Struct,State}.

convert_record(Name, #rtos{convert=Cv,records=Rs}) ->
    lists:member(Name, Cv) andalso lists:keymember(Name, 1, Rs).

add_record_def(Line, {Name,Fields0}, #rtos{records=Rs}=St) ->
    Fields1 = lists:map(fun ({record_field,_,{atom,_,Field}}) ->
                                {Field,{atom,Line,undefined}};
                            ({record_field,_,{atom,_,Field},Def}) ->
                                {Field,Def};
                            ({typed_record_field,
                              {record_field,_,{atom,_,Field}},_Type}) ->
                                {Field,{atom,Line,undefined}};
                            ({typed_record_field,
                              {record_field,_,{atom,_,Field},Def},_Type}) ->
                                {Field,Def}
                        end, Fields0),
    St#rtos{records=[{Name,Fields1} | Rs]}.

make_record_struct(Line, Name, Fields0, St0) ->
    %% Convert all the fields from dfeinition and input.
    {_,DefFs} = lists:keyfind(Name, 1, St0#rtos.records),
    Fields1 = get_fields(Line, DefFs, Fields0),
    {Fields2,St1} = record_fields(Fields1, St0),
    case convert_record(Name, St0) of
        true ->
            Mps = to_map_pairs(Line, Fields2, map_field_assoc),
            Struct = {map_field_assoc,
                      Line,
                      {atom,Line,'--struct--'},
                      {atom,Line,Name}},
            {{map,Line,[Struct|Mps]}, St1};
        false ->
            {{record,Line,Name,Fields2},St0}
    end.

struct_field(Line, Expr, Name, Field, St)     ->
    case convert_record(Name, St) of
        true ->
            Mget = {remote,Line,{atom,Line,maps},{atom,Line,get}},
            {{call,Line,Mget,[Field,Expr]},St};
        false ->
            {{record_field,Line,Expr,Name,Field},St}
    end.

update_struct(Line, Expr, Name, Fields0, St0) ->
    {Fields1,St1} = record_fields(Fields0, St0),
    case convert_record(Name, St0) of
        true ->
            Mps = to_map_pairs(Line, Fields1, map_field_exact),
            {{map,Line,Expr,Mps},St1};
        false ->
            {{record,Line,Expr,Name,Fields1},St1}
    end.

%% record_fields(Fields, State) -> {Cfields,State}.
%% get_fields(Line, DefinedFields, ArgFields) -> Fields.
%% field_value(Field, DefaultValue, ArgFields) -> Value.

record_fields(Fields, St) ->
    lists:mapfoldl(fun ({record_field,Line,K,Val0}, St0) ->
                           {Val1,St1} = expr(Val0, St0),
                           {{record_field,Line,K,Val1},St1}
                   end, St, Fields).

get_fields(Line, DefFs, Fs) ->
    lists:map(fun ({F,DefV}) ->
                      Val = get_field_value(F, DefV, Fs),
                      {record_field,Line,{atom,Line,F},Val}
              end, DefFs).

get_field_value(F, _DefV, [{record_field,_,{atom,_,F},V}|_]) -> V;
get_field_value(F, DefV, [_|Fields]) ->
    get_field_value(F, DefV, Fields);
get_field_value(_F, DefV, []) -> DefV.

%% to_map_pairs(Line, Fields, Type) -> MapFields.

to_map_pairs(Line, Fields, Type) ->
    lists:map(fun ({record_field,_,F,V}) -> {Type,Line,F,V} end, Fields).

%% map_assocs(MapAssocs, State) -> {MapAssocs,State}.

map_assocs(Assocs, St) ->
    Fun = fun ({Type,Line,Key0,Val0}, St0) ->
                  {Key1,St1} = expr(Key0, St0),
                  {Val1,St2} = expr(Val0, St1),
                  {{Type,Line,Key1,Val1},St2}
          end,
    lists:mapfoldl(Fun, St, Assocs).
