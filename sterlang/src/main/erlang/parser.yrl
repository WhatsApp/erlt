%% -*- erlang -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2020. All Rights Reserved.
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
%%
%% %CopyrightEnd%
%%

%% Definition of the Erlang grammar.

Nonterminals
form
attribute attr_val
function function_clauses function_clause
clause_args clause_guard clause_body
expr expr_max
pat_expr pat_expr_max map_pat_expr struct_pat_expr enum_pat_expr
pat_argument_list pat_exprs
list tail
list_comprehension lc_expr lc_exprs
binary_comprehension
tuple enum_expr
struct_expr struct_tuple struct_field struct_fields
map_expr map_tuple map_field map_fields
if_expr if_clause if_clauses case_expr cr_clause cr_clauses receive_expr
fun_expr fun_clause fun_clauses
try_expr try_catch try_clause try_clauses
function_call argument_list
remote_id
exprs guard
atomic
prefix_op mult_op add_op list_op comp_op
binary bin_elements bin_element bit_expr
opt_bit_size_expr bit_size_expr opt_bit_type_list bit_type_list bit_type
top_type top_types type field_defs field_def
fun_type
type_spec
map_field_types map_field_type struct_kind var_list vars.

Terminals
char integer float atom string var

'(' ')' ',' '->' '{' '}' '[' ']' '|' '||' '<-' ';' ':' '#' '.' '^'
'after' 'begin' 'case' 'try' 'catch' 'end' 'fun' 'if' 'of' 'receive' 'when'
'andalso' 'orelse'
'bnot' 'not'
'*' '/' 'div' 'rem' 'band' 'and'
'+' '-' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor'
'++' '--'
'==' '/=' '=<' '<' '>=' '>' '=:=' '=/=' '<='
'<<' '>>'
'!' '=' '::'
'spec' 'struct' 'exception' 'message' 'type_kind' % helper
dot.

Expect 0.

Rootsymbol form.

%% Expressions

Unary 0 'catch'.
Right 100 '=' '!'.
Right 150 'orelse'.
Right 160 'andalso'.
Nonassoc 200 comp_op.
Right 300 list_op.
Left 400 add_op.
Left 500 mult_op.
Unary 600 prefix_op.
Nonassoc 700 '#'.
Nonassoc 800 ':'.
Left 900 '.'.

%% Types

Right 150 '::'.
Left 170 '|'.
Nonassoc 500 '*'. % for binary expressions

form -> attribute dot : '$1'.
form -> function dot : '$1'.

struct_kind -> 'struct'    : '$1'.
struct_kind -> 'exception' : '$1'.
struct_kind -> 'message'   : '$1'.

attribute -> '-' atom '(' attr_val ')' :
    build_attribute('$2', '$4', anno('$1','$5')).
attribute -> '-' type_kind atom var_list '::' top_type :
    type_def('$2', '$3', '$4', '$6', anno('$1','$6')).
attribute -> '-' struct_kind atom '::' '{' field_defs '}' :
    struct_def('$2', '$3', '$6', anno('$1','$7')).
attribute -> '-' 'spec' type_spec :
    type_spec('$3', anno('$1','$3')).

type_spec -> atom fun_type                : {type_spec, anno('$1','$2'), '$1', ['$2']}.

field_defs -> '$empty'                    : [].
field_defs -> field_def                   : ['$1'].
field_defs -> field_def ',' field_defs    : ['$1'|'$3'].

field_def -> atom '::' top_type           : {typed,'$1','$3'}.

top_types -> top_type                     : ['$1'].
top_types -> top_type ',' top_types       : ['$1'|'$3'].

top_type -> var '::' top_type             : {ann_type, anno('$1','$3'), ['$1','$3']}.
top_type -> type '|' top_type             : lift_unions('$1','$3').
top_type -> type                          : '$1'.

type -> '(' top_type ')'                      : '$2'.
type -> var                                   : '$1'.
type -> atom                                  : '$1'.
type -> atom '{' '}'                          : {type, anno('$1', '$3'), enum, '$1', []}.
type -> atom '{' top_types '}'                : {type, anno('$1', '$4'), enum, '$1', '$3'}.
type -> atom '(' ')'                          : build_gen_type('$1', anno('$1', '$3')).
type -> atom '(' top_types ')'                : build_type('$1', '$3', anno('$1', '$4')).
type -> atom ':' atom '(' ')'                 : {remote_type, anno('$1','$5'), ['$1', '$3', []]}.
type -> atom ':' atom '(' top_types ')'       : {remote_type, anno('$1','$6'), ['$1', '$3', '$5']}.
type -> '[' top_type ']'                      : {type, anno('$1','$3'), list, ['$2']}.
type -> '#' '(' ')'                           : {type, anno('$1','$3'), map, []}.
type -> '#' '(' map_field_types ')'           : build_map_type(anno('$1', '$4'), '$3').
type -> '{' '}'                               : {type, anno('$1','$2'), tuple, []}.
type -> '{' top_types '}'                     : {type, anno('$1','$3'), tuple, '$2'}.
type -> '#' atom '{' '}'                      : {type, anno('$1','$4'), struct, ['$2']}.
type -> 'fun' '(' fun_type ')'                : '$3'.

fun_type -> '(' ')' '->' top_type :
    {type, anno('$1','$4'), 'fun', [{type, anno('$1','$4'), product, []}, '$4']}.
fun_type -> '(' top_types ')' '->' top_type :
    {type, anno('$1','$5'), 'fun', [{type, anno('$1','$5'), product, '$2'},'$5']}.

map_field_types -> type                               : {[], '$1'}.
map_field_types -> map_field_type                     : ['$1'].
map_field_types -> map_field_type ',' map_field_types : build_map_internals_type('$1', '$3').

map_field_type -> atom '::' top_type  : {type, anno('$1', '$3'), map_field, ['$1', '$3']}.

attr_val -> expr             : '$1'.
attr_val -> expr ',' exprs   : ['$1' | '$3'].

function -> function_clauses : build_function('$1').

function_clauses -> function_clause : ['$1'].
function_clauses -> function_clause ';' function_clauses : ['$1'|'$3'].

function_clause -> atom clause_args clause_guard clause_body :
	{clause, anno('$1','$4'), element(3, '$1'), '$2', '$3', '$4'}.

clause_args -> pat_argument_list : element(1, '$1').

clause_guard -> 'when' guard : '$2'.
clause_guard -> '$empty' : [].

clause_body -> '->' exprs: '$2'.

expr -> expr '=' expr : {match,anno('$1','$3'),'$1','$3'}.
expr -> expr '!' expr : ?mkop2('$1', '$2', '$3').
expr -> expr 'orelse' expr : ?mkop2('$1', '$2', '$3').
expr -> expr 'andalso' expr : ?mkop2('$1', '$2', '$3').
expr -> expr comp_op expr : ?mkop2('$1', '$2', '$3').
expr -> expr list_op expr : ?mkop2('$1', '$2', '$3').
expr -> expr add_op expr : ?mkop2('$1', '$2', '$3').
expr -> expr mult_op expr : ?mkop2('$1', '$2', '$3').
expr -> prefix_op expr : ?mkop1('$1', '$2').
expr -> map_expr : '$1'.
expr -> function_call : '$1'.
expr -> enum_expr : '$1'.
expr -> struct_expr : '$1'.
expr -> expr_max : '$1'.

remote_id -> atom ':' atom : {remote, anno('$1','$3'), '$1', '$3'}.

expr_max -> expr_max '#' '(' atom ')' : {map_field, anno('$1','$5'), '$1', '$4'}.
expr_max -> var : '$1'.
expr_max -> atomic : '$1'.
expr_max -> list : '$1'.
expr_max -> binary : '$1'.
expr_max -> list_comprehension : '$1'.
expr_max -> binary_comprehension : '$1'.
expr_max -> tuple : '$1'.
expr_max -> '(' expr ')' : '$2'.
expr_max -> 'begin' exprs 'end' : {block,anno('$1','$3'),'$2'}.
expr_max -> if_expr : '$1'.
expr_max -> case_expr : '$1'.
expr_max -> receive_expr : '$1'.
expr_max -> fun_expr : '$1'.
expr_max -> try_expr : '$1'.

pat_expr -> pat_expr '=' pat_expr : {match,anno('$1','$3'),'$1','$3'}.
pat_expr -> pat_expr comp_op pat_expr : ?mkop2('$1', '$2', '$3').
pat_expr -> pat_expr list_op pat_expr : ?mkop2('$1', '$2', '$3').
pat_expr -> pat_expr add_op pat_expr : ?mkop2('$1', '$2', '$3').
pat_expr -> pat_expr mult_op pat_expr : ?mkop2('$1', '$2', '$3').
pat_expr -> prefix_op pat_expr : ?mkop1('$1', '$2').
pat_expr -> map_pat_expr : '$1'.
pat_expr -> struct_pat_expr : '$1'.
pat_expr -> enum_pat_expr : '$1'.
pat_expr -> pat_expr_max : '$1'.

pat_expr_max -> var : '$1'.
pat_expr_max -> atomic : '$1'.
pat_expr_max -> list : '$1'.
pat_expr_max -> binary : '$1'.
pat_expr_max -> tuple : '$1'.
pat_expr_max -> '(' pat_expr ')' : '$2'.

enum_pat_expr -> atom '.' atom '{' '}' :
    {enum, anno('$1','$5'), '$1', '$3', []}.
enum_pat_expr -> atom '.' atom '{' pat_exprs '}' :
    {enum, anno('$1','$6'), '$1', '$3', '$5'}.
enum_pat_expr -> remote_id '.' atom '{' '}' :
    {enum, anno('$1','$5'), '$1', '$3', []}.
enum_pat_expr -> remote_id '.' atom '{' pat_exprs '}' :
    {enum, anno('$1','$6'), '$1', '$3', '$5'}.

map_pat_expr -> '#' map_tuple :
	{map, anno('$1','$2'),strip_map_tuple('$2')}.
map_pat_expr -> pat_expr_max '#' map_tuple :
	{map, anno('$1','$3'),'$1',strip_map_tuple('$3')}.

struct_pat_expr -> '#' atom struct_tuple :
	{struct, anno('$1', '$3'), element(3, '$2'), element(1, '$3')}.

list -> '[' ']'       : {nil,  anno('$1','$2')}.
list -> '[' expr tail : {cons, anno('$1','$3'),'$2','$3'}.

tail -> ']'           : {nil,  anno('$1')}.
tail -> '|' expr ']'  : '$2'.
tail -> ',' expr tail : {cons, anno('$2','$3'),'$2','$3'}.


binary -> '<<' '>>' : {bin,anno('$1','$2'),[]}.
binary -> '<<' bin_elements '>>' : {bin,anno('$1','$3'),'$2'}.

bin_elements -> bin_element : ['$1'].
bin_elements -> bin_element ',' bin_elements : ['$1'|'$3'].

bin_element -> bit_expr opt_bit_size_expr opt_bit_type_list :
	{bin_element,anno('$1',filter_defaults(['$3', '$2', '$1'])),'$1','$2',strip_bit_type_list('$3')}.

bit_expr -> prefix_op expr_max : ?mkop1('$1', '$2').
bit_expr -> expr_max : '$1'.

opt_bit_size_expr -> ':' bit_size_expr : '$2'.
opt_bit_size_expr -> '$empty' : default.

opt_bit_type_list -> '/' bit_type_list : '$2'.
opt_bit_type_list -> '$empty' : default.

bit_type_list -> bit_type '-' bit_type_list : ['$1' | '$3'].
bit_type_list -> bit_type : ['$1'].

bit_type -> atom             : '$1'.
bit_type -> atom ':' integer : {bit_type_unit, anno('$1', '$3'), '$1','$3'}.

bit_size_expr -> expr_max : '$1'.

list_comprehension -> '[' expr '||' lc_exprs ']' :
	{lc,anno('$1','$5'),'$2','$4'}.
binary_comprehension -> '<<' expr_max '||' lc_exprs '>>' :
	{bc,anno('$1','$5'),'$2','$4'}.
lc_exprs -> lc_expr : ['$1'].
lc_exprs -> lc_expr ',' lc_exprs : ['$1'|'$3'].

lc_expr -> expr : '$1'.
lc_expr -> expr '<-' expr : {generate,anno('$1','$3'),'$1','$3'}.
lc_expr -> binary '<=' expr : {b_generate,anno('$1','$3'),'$1','$3'}.

tuple -> '{' '}' : {tuple,anno('$1','$2'),[]}.
tuple -> '{' exprs '}' : {tuple,anno('$1','$3'),'$2'}.

%% This is called from expr
enum_expr -> atom '.' atom '{' '}' :
    {enum, anno('$1','$5'), '$1', '$3', []}.
enum_expr -> atom '.' atom '{' exprs '}' :
    {enum, anno('$1','$6'), '$1', '$3', '$5'}.
enum_expr -> remote_id '.' atom '{' '}' :
    {enum, anno('$1','$5'), '$1', '$3', []}.
enum_expr -> remote_id '.' atom '{' exprs '}' :
    {enum, anno('$1','$6'), '$1', '$3', '$5'}.

map_expr -> '#' map_tuple :
	{map, anno('$1','$2'),strip_map_tuple('$2')}.
map_expr -> expr_max '#' map_tuple :
	{map, anno('$1','$3'),'$1',strip_map_tuple('$3')}.
map_expr -> map_expr '#' map_tuple :
	{map, anno('$1','$3'),'$1',strip_map_tuple('$3')}.

map_tuple -> '(' ')' : {map_tuple, anno('$1','$2'), []}.
map_tuple -> '(' map_fields ')' : {map_tuple, anno('$1','$3'), '$2'}.

map_fields -> map_field : ['$1'].
map_fields -> map_field ',' map_fields : ['$1' | '$3'].

map_field -> atom '=' expr : {map_field, anno('$1','$3'), '$1', '$3'}.

struct_expr -> '#' atom struct_tuple :
	{struct, anno('$1','$3'), element(3, '$2'), element(1, '$3')}.
struct_expr -> expr_max '#' atom '.' atom :
	{struct_field, anno('$2', '$5'), '$1', element(3, '$3'),'$5'}.
struct_expr -> struct_expr '#' atom '.' atom :
	{struct_field, anno('$2', '$5'), '$1', element(3, '$3'),'$5'}.
struct_expr -> expr_max '#' atom struct_tuple :
	{struct, anno('$2','$4'), '$1', element(3, '$3'), element(1, '$4')}.
struct_expr -> struct_expr '#' atom struct_tuple :
	{struct, anno('$2','$4'), '$1', element(3, hd('$3')), element(1, '$4')}.

struct_tuple -> '{' '}'               : {[],   anno('$1', '$2')}.
struct_tuple -> '{' struct_fields '}' : {'$2', anno('$1', '$3')}.

struct_fields -> struct_field : ['$1'].
struct_fields -> struct_field ',' struct_fields : ['$1' | '$3'].

struct_field -> atom '=' expr : {struct_field, anno('$1', '$3'), '$1', '$3'}.

function_call -> remote_id argument_list :
	{call, anno('$1','$2'), '$1', element(1, '$2')}.
function_call -> expr_max argument_list :
	{call, anno('$1','$2'), '$1', element(1, '$2')}.

if_expr -> 'if' if_clauses 'end' : {'if',anno('$1','$3'),'$2'}.

if_clauses -> if_clause : ['$1'].
if_clauses -> if_clause ';' if_clauses : ['$1' | '$3'].

if_clause -> guard clause_body :
	{clause,anno(hd(hd('$1')),'$2'),[],'$1','$2'}.

case_expr -> 'case' expr 'of' cr_clauses 'end' :
	{'case',anno('$1','$5'),'$2','$4'}.

cr_clauses -> cr_clause : ['$1'].
cr_clauses -> cr_clause ';' cr_clauses : ['$1' | '$3'].

cr_clause -> pat_expr clause_guard clause_body :
	{clause,anno('$1','$3'),['$1'],'$2','$3'}.

receive_expr -> 'receive' cr_clauses 'end' :
	{'receive',anno('$1','$3'),'$2'}.
receive_expr -> 'receive' 'after' expr clause_body 'end' :
	{'receive',anno('$1','$5'),[],'$3','$4'}.
receive_expr -> 'receive' cr_clauses 'after' expr clause_body 'end' :
	{'receive',anno('$1','$6'),'$2','$4','$5'}.

fun_expr -> 'fun' atom '/' integer :
    {'fun',anno('$1', '$4'),{function,element(3, '$2'),element(3, '$4')}}.
fun_expr -> 'fun' atom ':' atom '/' integer :
	{'fun',anno('$1','$6'),{function,'$2','$4','$6'}}.
fun_expr -> 'fun' fun_clauses 'end' :
	build_fun(anno('$1','$3'), '$2').

fun_clauses -> fun_clause : ['$1'].
fun_clauses -> fun_clause ';' fun_clauses : ['$1' | '$3'].

fun_clause -> pat_argument_list clause_guard clause_body :
	{clause,anno('$1','$3'),'fun',element(1, '$1'),'$2','$3'}.

fun_clause -> var pat_argument_list clause_guard clause_body :
	{clause,anno('$1','$4'),element(3, '$1'),element(1, '$2'),'$3','$4'}.

try_expr -> 'try' exprs 'of' cr_clauses try_catch :
	build_try('$1','$2','$4','$5').
try_expr -> 'try' exprs try_catch :
	build_try('$1','$2',[],'$3').

try_catch -> 'catch' try_clauses 'end' :
	{'$2',[],'$3'}.
try_catch -> 'catch' try_clauses 'after' exprs 'end' :
	{'$2','$4','$5'}.
try_catch -> 'after' exprs 'end' :
	{[],'$2','$3'}.

try_clauses -> try_clause : ['$1'].
try_clauses -> try_clause ';' try_clauses : ['$1' | '$3'].

try_clause -> pat_expr clause_guard clause_body :
	A = anno('$1','$3'),
	{clause,A,[{tuple,A,[{atom,A,throw},'$1',{var,A,'_'}]}],'$2','$3'}.

var_list -> '(' ')'      : [].
var_list -> '(' vars ')' : '$2'.

vars -> var          : ['$1'].
vars -> var ',' vars : ['$1' | '$3'].

argument_list -> '(' ')' : {[],anno('$1','$2')}.
argument_list -> '(' exprs ')' : {'$2',anno('$1','$3')}.

exprs -> expr : ['$1'].
exprs -> expr ',' exprs : ['$1' | '$3'].

pat_argument_list -> '(' ')' : {[],anno('$1','$2')}.
pat_argument_list -> '(' pat_exprs ')' : {'$2',anno('$1','$3')}.

pat_exprs -> pat_expr : ['$1'].
pat_exprs -> pat_expr ',' pat_exprs : ['$1' | '$3'].

guard -> exprs : ['$1'].
guard -> exprs ';' guard : ['$1'|'$3'].

atomic -> char : '$1'.
atomic -> integer : '$1'.
atomic -> float : '$1'.
atomic -> atom : '$1'.
atomic -> string : '$1'.

prefix_op -> '+' : '$1'.
prefix_op -> '-' : '$1'.
prefix_op -> 'bnot' : '$1'.
prefix_op -> 'not' : '$1'.
prefix_op -> '^' : '$1'.

mult_op -> '/' : '$1'.
mult_op -> '*' : '$1'.
mult_op -> 'div' : '$1'.
mult_op -> 'rem' : '$1'.
mult_op -> 'band' : '$1'.
mult_op -> 'and' : '$1'.

add_op -> '+' : '$1'.
add_op -> '-' : '$1'.
add_op -> 'bor' : '$1'.
add_op -> 'bxor' : '$1'.
add_op -> 'bsl' : '$1'.
add_op -> 'bsr' : '$1'.
add_op -> 'or' : '$1'.
add_op -> 'xor' : '$1'.

list_op -> '++' : '$1'.
list_op -> '--' : '$1'.

comp_op -> '==' : '$1'.
comp_op -> '/=' : '$1'.
comp_op -> '=<' : '$1'.
comp_op -> '<' : '$1'.
comp_op -> '>=' : '$1'.
comp_op -> '>' : '$1'.
comp_op -> '=:=' : '$1'.
comp_op -> '=/=' : '$1'.

Erlang code.

-export([main/1]).

-define(mkop2(L, OpAnno, R), begin
    {__Op, __Anno} = OpAnno,
    {op, anno(L, R), __Op, L, R}
end).

-define(mkop1(OpAnno, A), begin
    {__Op, __Anno} = OpAnno,
    {op, anno(OpAnno, A), __Op, A}
end).

parse_form([{'-', A1}, {atom, A2, spec} | Tokens]) ->
    parse([{'-', A1}, {'spec', A2} | Tokens]);
parse_form([{'-', A1}, {atom, A2, struct} | Tokens]) ->
    parse([{'-', A1}, {'struct', A2} | Tokens]);
parse_form([{'-', A1}, {atom, A2, exception} | Tokens]) ->
    parse([{'-', A1}, {'exception', A2} | Tokens]);
parse_form([{'-', A1}, {atom, A2, message} | Tokens]) ->
    parse([{'-', A1}, {'message', A2} | Tokens]);
parse_form([{'-', A1}, {atom, _, type} | Tokens]) ->
    parse([{'-', A1}, {type_kind, type} | Tokens]);
parse_form([{'-', A1}, {atom, _, enum} | Tokens]) ->
    parse([{'-', A1}, {type_kind, enum} | Tokens]);
parse_form([{'-', A1}, {atom, _, opaque} | Tokens]) ->
    parse([{'-', A1}, {type_kind, opaque} | Tokens]);
parse_form(Tokens) ->
    parse(Tokens).

type_def({type_kind, Kind}, {atom, _, Name}, Args, Type, Aa) ->
    {attribute, Aa, Kind, {Name, Type, Args}}.

struct_def({StructKind, _}, {atom, _An, StructName}, Fields, Aa) ->
    {attribute, Aa, StructKind, {StructName, struct_fields(Fields)}}.

type_spec({type_spec, _TA, {atom, _, Fun}, TypeSpecs}, Aa) ->
    {attribute, Aa, spec, {{Fun, find_arity_from_specs(TypeSpecs)}, TypeSpecs}}.

find_arity_from_specs([Spec | _]) ->
    {type, _, 'fun', [{type, _, product, Args}, _]} = Spec,
    length(Args).

lift_unions(T1, {type, _Aa, union, List}) ->
    {type, anno(T1), union, [T1 | List]};
lift_unions(T1, T2) ->
    {type, anno(T1), union, [T1, T2]}.

build_gen_type({atom, _, tuple}, Aa) ->
    {type, Aa, tuple, any};
build_gen_type({atom, _, map}, Aa) ->
    {type, Aa, map, any};
build_gen_type(Name, Aa) ->
    build_type(Name, [], Aa).

build_map_internals_type(This, {Fields, Rest}) ->
    {[This | Fields], Rest};
build_map_internals_type(This, Rest) when is_list(Rest) ->
    [This | Rest].

build_map_type(Anno, {Fields, RowType}) ->
    {type, Anno, open_map, Fields, RowType};
build_map_type(Anno, Fields) when is_list(Fields) ->
    {type, Anno, map, Fields}.

build_type({atom, _, Name}, Types, A) ->
    Tag = type_tag(Name, length(Types)),
    {Tag, A, Name, Types}.

type_tag(TypeName, NumberOfTypeVariables) ->
    case erl_internal:is_type(TypeName, NumberOfTypeVariables) of
        true -> type;
        false -> user_type
    end.

build_attribute({atom, _, module}, {atom, _, Module}, Aa) ->
    {attribute, Aa, module, Module};
build_attribute({atom, _, export}, Val, Aa) ->
    {attribute, Aa, export, farity_list(Val)};
build_attribute({atom, _, export_type}, Val, Aa) ->
    {attribute, Aa, export_type, farity_list(Val)};
build_attribute({atom, _, import}, [{atom, _, Mod}, ImpList], Aa) ->
    {attribute, Aa, import, {Mod, farity_list(ImpList)}};
build_attribute({atom, _, import_type}, [{atom, _, Mod}, ImpList], Aa) ->
    {attribute, Aa, import_type, {Mod, farity_list(ImpList)}};
build_attribute({atom, _, file}, [{string, _, Name}, {integer, _, Line}], Aa) ->
    {attribute, Aa, file, {Name, Line}};
build_attribute({atom, _, lang}, {atom, _, Lang}, Aa) when Lang == ffi; Lang == st ->
    {attribute, Aa, lang, Lang};
build_attribute(_, _, Aa) ->
    ret_err(Aa, "bad attribute").

farity_list({cons, _Ac, {op, _Ao, '/', {atom, _Aa, A}, {integer, _Ai, I}}, Tail}) ->
    [{A, I} | farity_list(Tail)];
farity_list({nil, _An}) ->
    [];
farity_list(Other) ->
    ret_err(anno(Other), "bad function arity").

struct_fields([{typed, {atom, Aa, A}, TypeInfo} | Fields]) ->
    [{typed_struct_field, {struct_field, Aa, {atom, Aa, A}}, TypeInfo} | struct_fields(Fields)];
struct_fields([Other | _Fields]) ->
    ret_err(anno(Other), "bad struct field");
struct_fields([]) ->
    [].

%% build_function([Clause]) -> {function,Anno,Name,Arity,[Clause]}

build_function(Cs) ->
    Name = element(3, hd(Cs)),
    Arity = length(element(4, hd(Cs))),
    {function, anno(hd(Cs), Cs), Name, Arity, check_clauses(Cs, Name, Arity)}.

%% build_fun(Anno, [Clause]) -> {'fun',Anno,{clauses,[Clause]}}.

build_fun(Anno, Cs) ->
    Name = element(3, hd(Cs)),
    Arity = length(element(4, hd(Cs))),
    CheckedCs = check_clauses(Cs, Name, Arity),
    case Name of
        'fun' ->
            {'fun', Anno, {clauses, CheckedCs}};
        Name ->
            {named_fun, Anno, Name, CheckedCs}
    end.

check_clauses(Cs, Name, Arity) ->
    [
        case C of
            {clause, A, N, As, G, B} when N =:= Name, length(As) =:= Arity ->
                {clause, A, As, G, B};
            {clause, A, _N, _As, _G, _B} ->
                ret_err(A, "head mismatch")
        end
        || C <- Cs
    ].

filter_defaults([default|Rest]) -> filter_defaults(Rest);
filter_defaults([NonDefault|_]) -> NonDefault.

%% Strip out all annotations for bit type specifications, we should revisit if this makes sense.
strip_bit_type_list([{bit_type_unit, _Anno, Type, Unit} | Rest]) ->
    [{element(3, Type), element(3, Unit)} | strip_bit_type_list(Rest)];
strip_bit_type_list([Type | Rest]) ->
    [element(3, Type) | strip_bit_type_list(Rest)];
strip_bit_type_list([]) ->
    [];
strip_bit_type_list(default) ->
    default.

strip_map_tuple({map_tuple, _Anno, List}) ->
    List.

build_try(Try, Es, Scs, {Ccs, As, End}) ->
    {'try', anno(Try, End), Es, Scs, Ccs, As}.

ret_err({{Start, End}, _}, S) ->
    return_error({Start, End}, S);
ret_err(Loc, S) ->
    return_error(Loc, S).

anno(Tup) -> element(2, Tup).

anno(Left, Right) when is_list(Right) ->
    merge_anno(anno(Left), anno(lists:last(Right)));
anno(Left, Right) when is_tuple(Right) ->
    merge_anno(anno(Left), anno(Right)).

merge_anno({Start1,_}, {_,End2}) ->
    {Start1, End2}.

%% the main logic

main(["-ifile", IFile, "-ofile", OFile]) ->
    Forms = parse_file(IFile),
    Lang = parse_lang(Forms),
    Forms1 = normalize_for_typecheck(Forms, Lang),
    CodeETF = erlang:term_to_binary(Forms1),
    ok = filelib:ensure_dir(OFile),
    ok = file:write_file(OFile, CodeETF);
main(["-ast", Filename]) ->
    Forms = parse_file(Filename),
    Lang = parse_lang(Forms),
    Forms1 = normalize_for_typecheck(Forms, Lang),
    io:format("Forms:\n~p\n", [Forms1]);
main(["-idir", IDir, "-odir", ODir]) ->
    {ok, Files} = file:list_dir(IDir),
    SortedFiles = lists:sort(Files),
    ErlFiles = lists:filter(
        fun(Name) -> filename:extension(Name) == ".erl" end,
        SortedFiles
    ),
    lists:foreach(
        fun(ErlFile) ->
            EtfFile = filename:basename(ErlFile, ".erl") ++ ".etf",
            IFile = filename:join(IDir, ErlFile),
            OFile = filename:join(ODir, EtfFile),
            ok = main(["-ifile", IFile, "-ofile", OFile])
        end,
        ErlFiles
    ),
    ok.

parse_lang(Forms) ->
    lists:nth(1, [Lang || {attribute, _, lang, Lang} <- Forms]).

normalize_for_typecheck(Forms, Lang) ->
    Forms1 =
        case Lang of
            st -> Forms;
            ffi -> [F || F <- Forms, not is_fun_form(F)]
        end,
    Forms1.

is_fun_form({function, _, _, _, _}) -> true;
is_fun_form(_) -> false.

parse_file(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Chars = unicode:characters_to_list(Data, utf8),
    parse_chars(Chars, {1, 1}).

parse_chars(Chars, Location) ->
    case erl_scan:tokens([], Chars, Location, ['text']) of
        {done, Result, Chars1} ->
            case Result of
                {ok, Tokens, Location1} ->
                    Tokens1 = norm_loc_tokens(Tokens),
                    case parse_form(Tokens1) of
                        {ok, Form} ->
                            [Form | parse_chars(Chars1, Location1)];
                        {error, E} ->
                            [{error, E}]
                    end;
                {error, E, _Location1} ->
                    [{error, E}];
                {eof, EndLocation} ->
                    [{eof, EndLocation}]
            end;
        {more, _} ->
            case erl_scan:tokens([], Chars ++ eof, Location, ['text']) of
                {done, Result, _} ->
                    case Result of
                        {ok, Tokens = [FirstToken | _], _} ->
                            Tokens1 = norm_loc_tokens(Tokens),
                            case parse_form(Tokens1) of
                                {ok, Form} ->
                                    [Form];
                                {error, _} ->
                                    [{error, {erl_scan:location(FirstToken)}}]
                            end;
                        {error, _, _} ->
                            [{error, {Location}}];
                        {eof, EndLocation} ->
                            [{eof, EndLocation}]
                    end;
                {more, _} ->
                    [{error, Location}]
            end
    end.

norm_loc_token(Tok) ->
    [{text, Text}, {location, {L1, C1}}] = erlang:element(2, Tok),
    {L2, C2} = end_text_location(Text, L1, C1),
    erlang:setelement(2, Tok, {{L1, C1}, {L2, C2}}).

norm_loc_tokens([]) ->
    [];
norm_loc_tokens([Tok | Toks]) ->
    [norm_loc_token(Tok) | norm_loc_tokens(Toks)].

end_text_location("", Line, Column) ->
    {Line, Column};
end_text_location([$\n|String], Line, _Column) ->
    end_text_location(String, Line+1, 1);
end_text_location([_|String], Line, Column) ->
    end_text_location(String, Line, Column+1).

