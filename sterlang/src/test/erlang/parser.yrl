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
pat_expr pat_expr_max shape_pat_expr struct_pat_expr enum_pat_expr
tuple_pat_expr list_pat_expr tail_pat_expr binary_pat_expr bin_pat_elements
bin_pat_element bit_pat_expr shape_tuple_pat shape_fields_pat shape_field_pat
struct_tuple_pat fields_pat field_pat
pat_argument_list pat_exprs
list tail
list_comprehension lc_expr lc_exprs
binary_comprehension
tuple enum_expr
struct_expr struct_tuple field fields struct_name
shape_expr shape_tuple shape_field shape_fields
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
shape_field_types shape_field_type struct_kind var_list vars enum_variants enum_variant.

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
'spec' 'struct' 'exception' 'message' 'type_attr'  'opaque_attr' 'enum' 'unchecked_opaque' % helper
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
attribute -> '-' type_attr atom var_list '::' top_type :
    type_def('$3', '$4', '$6', anno('$1','$6')).
attribute -> unchecked_opaque '-' type_attr atom var_list '::' top_type :
    unchecked_type_def('$4', '$5', anno('$2','$7')).
attribute -> '-' opaque_attr atom var_list '::' top_type :
    opaque_def('$3', '$4', '$6', anno('$1','$6')).
attribute -> '-' enum atom var_list '::' '(' enum_variants ')' :
    enum_def('$3', '$4', '$7', anno('$1','$8')).
attribute -> '-' struct_kind atom var_list '::' '(' field_defs ')' :
    struct_def('$2', '$3', '$4', '$7', anno('$1','$8')).
attribute -> '-' 'spec' type_spec :
    type_spec('$3', anno('$1','$3')).

type_spec -> atom fun_type                : {type_spec, anno('$1','$2'), '$1', ['$2']}.

field_defs -> '$empty'                    : [].
field_defs -> field_def                   : ['$1'].
field_defs -> field_def ',' field_defs    : ['$1'|'$3'].

field_def -> type                         : {field_definition, anno('$1'), 'positional', 'undefined', '$1'}.
field_def -> atom '::' type               : {field_definition, anno('$1', '$3'), '$1', 'undefined', '$3'}.
field_def -> atom '=' expr '::' type      : {field_definition, anno('$1', '$5'), '$1', '$3', '$5'}.

top_types -> top_type                     : ['$1'].
top_types -> top_type ',' top_types       : ['$1'|'$3'].

top_type -> var '::' top_type             : {ann_type, anno('$1','$3'), ['$1','$3']}.
top_type -> type                          : '$1'.

type -> '(' top_type ')'                      : '$2'.
type -> var                                   : '$1'.
type -> atom '(' ')'                          : build_type('$1', [], anno('$1', '$3')).
type -> atom '(' top_types ')'                : build_type('$1', '$3', anno('$1', '$4')).
type -> atom ':' atom '(' ')'                 : {remote_type, anno('$1','$5'), ['$1', '$3', []]}.
type -> atom ':' atom '(' top_types ')'       : {remote_type, anno('$1','$6'), ['$1', '$3', '$5']}.
type -> '[' top_type ']'                      : {type, anno('$1','$3'), list, ['$2']}.
type -> '#' '(' ')'                           : {type, anno('$1','$3'), shape, []}.
type -> '#' '(' shape_field_types ')'         : build_shape_type(anno('$1', '$4'), '$3').
type -> '{' '}'                               : {type, anno('$1','$2'), tuple, []}.
type -> '{' top_types '}'                     : {type, anno('$1','$3'), tuple, '$2'}.
type -> 'fun' '(' fun_type ')'                : '$3'.

enum_variant -> atom                          : {type, anno('$1'), enum, '$1', []}.
enum_variant -> atom '{' field_defs '}'       : {type, anno('$1', '$4'), enum, '$1', '$3'}.

enum_variants -> enum_variant                   : ['$1'].
enum_variants -> enum_variant ',' enum_variants : ['$1'|'$3'].

fun_type -> '(' ')' '->' top_type :
    {type, anno('$1','$4'), 'fun', [{type, anno('$1','$4'), product, []}, '$4']}.
fun_type -> '(' top_types ')' '->' top_type :
    {type, anno('$1','$5'), 'fun', [{type, anno('$1','$5'), product, '$2'},'$5']}.

shape_field_types -> var                                    : {[], '$1'}.
shape_field_types -> shape_field_type                       : ['$1'].
shape_field_types -> shape_field_type ',' shape_field_types : build_shape_internals_type('$1', '$3').

shape_field_type -> atom '::' top_type                      : {type, anno('$1', '$3'), shape_field, ['$1', '$3']}.

attr_val -> expr             : '$1'.
attr_val -> expr ',' exprs   : ['$1' | '$3'].

function -> '[' atom ']' function_clauses : build_unchecked_function('$2', '$4').
function -> function_clauses              : build_function('$1').

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
expr -> shape_expr : '$1'.
expr -> function_call : '$1'.
expr -> enum_expr : '$1'.
expr -> struct_expr : '$1'.
expr -> expr_max : '$1'.

remote_id -> atom ':' atom : {remote, anno('$1','$3'), '$1', '$3'}.

expr_max -> expr_max '#' '(' atom ')' : {shape_field, anno('$1','$5'), '$1', '$4'}.
expr_max -> '^' var : {pinned_var, anno('$1', '$2'), element(3, '$2')}.
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
pat_expr -> pat_expr_max : '$1'.

pat_expr_max -> '^' var : {pinned_var, anno('$1', '$2'), element(3, '$2')}.
pat_expr_max -> var : '$1'.
pat_expr_max -> atomic : '$1'.
pat_expr_max -> list_pat_expr : '$1'.
pat_expr_max -> binary_pat_expr : '$1'.
pat_expr_max -> tuple_pat_expr : '$1'.
pat_expr_max -> shape_pat_expr : '$1'.
pat_expr_max -> struct_pat_expr : '$1'.
pat_expr_max -> enum_pat_expr : '$1'.
pat_expr_max -> '(' pat_expr ')' : '$2'.

enum_pat_expr -> atom '.' atom :
    {enum, anno('$1','$3'), '$1', '$3', []}.
enum_pat_expr -> atom '.' atom '{' fields_pat '}' :
    {enum, anno('$1','$6'), '$1', '$3', '$5'}.
enum_pat_expr -> remote_id '.' atom :
    {enum, anno('$1','$3'), '$1', '$3', []}.
enum_pat_expr -> remote_id '.' atom '{' fields_pat '}' :
    {enum, anno('$1','$6'), '$1', '$3', '$5'}.

shape_pat_expr -> '#' shape_tuple_pat :
	{shape, anno('$1','$2'),strip_shape_tuple('$2')}.
shape_pat_expr -> pat_expr_max '#' shape_tuple_pat :
	{shape, anno('$1','$3'),'$1',strip_shape_tuple('$3')}.

tuple_pat_expr -> '{' '}'           : {tuple,anno('$1','$2'),[]}.
tuple_pat_expr -> '{' pat_exprs '}' : {tuple,anno('$1','$3'),'$2'}.

list_pat_expr -> '[' ']'                    : {nil,  anno('$1','$2')}.
list_pat_expr -> '[' pat_expr tail_pat_expr : {cons, anno('$1','$3'),'$2','$3'}.

tail_pat_expr -> ']'                        : {nil,  anno('$1')}.
tail_pat_expr -> '|' pat_expr ']'           : '$2'.
tail_pat_expr -> ',' pat_expr tail_pat_expr : {cons, anno('$2','$3'),'$2','$3'}.

binary_pat_expr -> '<<' '>>' : {bin,anno('$1','$2'),[]}.
binary_pat_expr -> '<<' bin_pat_elements '>>' : {bin,anno('$1','$3'),'$2'}.

bin_pat_elements -> bin_pat_element : ['$1'].
bin_pat_elements -> bin_pat_element ',' bin_pat_elements : ['$1'|'$3'].

bin_pat_element -> bit_pat_expr opt_bit_size_expr opt_bit_type_list :
	{bin_element,anno('$1',filter_defaults(['$3', '$2', '$1'])),'$1','$2',strip_bit_type_list('$3')}.

bit_pat_expr -> prefix_op pat_expr_max : ?mkop1('$1', '$2').
bit_pat_expr -> pat_expr_max : '$1'.

struct_pat_expr -> '#' struct_name struct_tuple_pat :
	{struct, anno('$1', '$3'), '$2', element(1, '$3')}.

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
enum_expr -> atom '.' atom :
    {enum, anno('$1','$3'), '$1', '$3', []}.
enum_expr -> atom '.' atom '{' fields '}' :
    {enum, anno('$1','$6'), '$1', '$3', '$5'}.
enum_expr -> remote_id '.' atom :
    {enum, anno('$1','$3'), '$1', '$3', []}.
enum_expr -> remote_id '.' atom '{' fields '}' :
    {enum, anno('$1','$6'), '$1', '$3', '$5'}.

shape_expr -> '#' shape_tuple :
	{shape, anno('$1','$2'),strip_shape_tuple('$2')}.
shape_expr -> expr_max '#' shape_tuple :
	{shape, anno('$1','$3'),'$1',strip_shape_tuple('$3')}.
shape_expr -> shape_expr '#' shape_tuple :
	{shape, anno('$1','$3'),'$1',strip_shape_tuple('$3')}.

shape_tuple -> '(' ')' : {shape_tuple, anno('$1','$2'), []}.
shape_tuple -> '(' shape_fields ')' : {shape_tuple, anno('$1','$3'), '$2'}.

shape_fields -> shape_field : ['$1'].
shape_fields -> shape_field ',' shape_fields : ['$1' | '$3'].

shape_field -> atom '=' expr : {shape_field, anno('$1','$3'), '$1', '$3'}.

shape_tuple_pat -> '(' ')' : {shape_tuple, anno('$1','$2'), []}.
shape_tuple_pat -> '(' shape_fields_pat ')' : {shape_tuple, anno('$1','$3'), '$2'}.

shape_fields_pat -> shape_field_pat : ['$1'].
shape_fields_pat -> shape_field_pat ',' shape_fields_pat : ['$1' | '$3'].

shape_field_pat -> atom '=' pat_expr : {shape_field, anno('$1','$3'), '$1', '$3'}.

struct_expr -> '#' struct_name struct_tuple :
	{struct, anno('$1','$3'), '$2', element(1, '$3')}.
struct_expr -> expr_max '#' struct_name '.' atom :
	{struct_field, anno('$2', '$5'), '$1', '$3', '$5'}.
struct_expr -> struct_expr '#' struct_name '.' atom :
	{struct_field, anno('$2', '$5'), '$1', '$3', '$5'}.
struct_expr -> expr_max '#' struct_name '.' var :
	{struct_field, anno('$2', '$5'), '$1', '$3', field_index('$5')}.
struct_expr -> struct_expr '#' struct_name '.' var :
	{struct_field, anno('$2', '$5'), '$1', '$3', field_index('$5')}.
struct_expr -> expr_max '#' struct_name struct_tuple :
	{struct, anno('$2','$4'), '$1', '$3', element(1, '$4')}.
struct_expr -> struct_expr '#' struct_name struct_tuple :
	{struct, anno('$2','$4'), '$1', '$3', element(1, '$4')}.

struct_name -> atom : element(3, '$1').
struct_name -> atom ':' atom : {remote, anno('$1', '$3'), '$1', '$3'}.

struct_tuple -> '{' '}'        : {[],   anno('$1', '$2')}.
struct_tuple -> '{' fields '}' : {'$2', anno('$1', '$3')}.

fields -> field : ['$1'].
fields -> field ',' fields : ['$1' | '$3'].

field -> atom '=' expr : {field, anno('$1', '$3'), '$1', '$3'}.
field ->          expr : {field, anno('$1'), 'undefined', '$1'}.

struct_tuple_pat -> '{' '}'            : {[],   anno('$1', '$2')}.
struct_tuple_pat -> '{' fields_pat '}' : {'$2', anno('$1', '$3')}.

fields_pat -> field_pat : ['$1'].
fields_pat -> field_pat ',' fields_pat : ['$1' | '$3'].

field_pat -> atom '=' pat_expr : {field, anno('$1', '$3'), '$1', '$3'}.
field_pat ->          pat_expr : {field, anno('$1'), 'undefined', '$1'}.

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
	{clause,anno('$1', '$3'),['$1'],'$2','$3'}.

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

parse_form([{'[', _}, {atom, _, unchecked}, {',', _}, {atom, _, opaque}, {']', _}, {'-', A1}, {atom, _, type} | Tokens]) ->
    parse([{unchecked_opaque}, {'-', A1}, {type_attr} | Tokens]);
parse_form([{'[', _}, {atom, _, opaque}, {',', _}, {atom, _, unchecked}, {']', _}, {'-', A1}, {atom, _, type} | Tokens]) ->
    parse([{unchecked_opaque}, {'-', A1}, {type_attr} | Tokens]);
parse_form([{'-', A1}, {atom, A2, spec} | Tokens]) ->
    parse([{'-', A1}, {'spec', A2} | Tokens]);
parse_form([{'-', A1}, {atom, A2, struct} | Tokens]) ->
    parse([{'-', A1}, {'struct', A2} | Tokens]);
parse_form([{'-', A1}, {atom, A2, exception} | Tokens]) ->
    parse([{'-', A1}, {'exception', A2} | Tokens]);
parse_form([{'-', A1}, {atom, A2, message} | Tokens]) ->
    parse([{'-', A1}, {'message', A2} | Tokens]);
parse_form([{'-', A1}, {atom, _, opaque} | Tokens]) ->
    parse([{'-', A1}, {opaque_attr} | Tokens]);
parse_form([{'-', A1}, {atom, _, type} | Tokens]) ->
    parse([{'-', A1}, {type_attr} | Tokens]);
parse_form([{'-', A1}, {atom, _, enum} | Tokens]) ->
    parse([{'-', A1}, {enum} | Tokens]);
parse_form(Tokens) ->
    parse(Tokens).

type_def({atom, _, Name}, Args, Type, Aa) ->
    {attribute, Aa, type, {Name, Type, Args}}.

unchecked_type_def({atom, _, Name}, Args, Aa) ->
    {attribute, Aa, unchecked_type, {Name, Args}}.

opaque_def({atom, _, Name}, Args, Type, Aa) ->
    {attribute, Aa, opaque, {Name, Type, Args}}.

enum_def({atom, _, Name}, Args, Variants, Aa) ->
    {attribute, Aa, enum, {Name, Args, Variants}}.

struct_def({StructKind, _}, {atom, _An, StructName}, Args, Fields, Aa) ->
    {attribute, Aa, StructKind, {StructName, Args, Fields}}.

type_spec({type_spec, _TA, {atom, _, Fun}, TypeSpecs}, Aa) ->
    {attribute, Aa, spec, {{Fun, find_arity_from_specs(TypeSpecs)}, TypeSpecs}}.

find_arity_from_specs([Spec | _]) ->
    {type, _, 'fun', [{type, _, product, Args}, _]} = Spec,
    length(Args).

build_shape_internals_type(This, {Fields, Rest}) ->
    {[This | Fields], Rest};
build_shape_internals_type(This, Rest) when is_list(Rest) ->
    [This | Rest].

build_shape_type(Anno, {Fields, RowType}) ->
    {type, Anno, open_shape, Fields, RowType};
build_shape_type(Anno, Fields) when is_list(Fields) ->
    {type, Anno, shape, Fields}.

build_type({atom, _, Name}, Types, A) ->
    Tag = type_tag(Name, length(Types)),
    {Tag, A, Name, Types}.

type_tag(message, NumberOfTypeVariables) ->
    type;
type_tag(exception, NumberOfTypeVariables) ->
    type;
type_tag(TypeName, NumberOfTypeVariables) ->
    case erl_internal:is_type(TypeName, NumberOfTypeVariables) of
        true -> type;
        false -> user_type
    end.

field_index({var, Anno, Name}) ->
    case atom_to_list(Name) of
        "_" ++ Num ->
            try {integer, Anno, list_to_integer(Num)}
            catch error:badarg -> ret_err(Anno, "bad field index")
            end;
        _ ->
            ret_err(Anno, "bad field index")
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
build_attribute(_, _, Aa) ->
    ret_err(Aa, "bad attribute").

farity_list({cons, _Ac, {op, _Ao, '/', {atom, _Aa, A}, {integer, _Ai, I}}, Tail}) ->
    [{A, I} | farity_list(Tail)];
farity_list({nil, _An}) ->
    [];
farity_list(Other) ->
    ret_err(anno(Other), "bad function arity").

build_function(Cs) ->
    Name = element(3, hd(Cs)),
    Arity = length(element(4, hd(Cs))),
    {function, anno(hd(Cs), Cs), Name, Arity, check_clauses(Cs, Name, Arity)}.

build_unchecked_function({atom, _, unchecked}, Cs) ->
    Name = element(3, hd(Cs)),
    Arity = length(element(4, hd(Cs))),
    {unchecked_function, Name, Arity};
build_unchecked_function({atom, A, _}, _) ->
    ret_err(A, "bad modifier").


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

strip_shape_tuple({shape_tuple, _Anno, List}) ->
    List.

build_try(Try, Es, Scs, {Ccs, As, End}) ->
    {'try', anno(Try, End), Es, Scs, Ccs, As}.

-spec ret_err(_, _) -> no_return().
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
    CodeETF = erlang:term_to_binary(Forms),
    ok = filelib:ensure_dir(OFile),
    ok = file:write_file(OFile, CodeETF);
main(["-ast", Filename]) ->
    Forms = parse_file(Filename),
    io:format("Forms:\n~p\n", [Forms]);
main(["-idir", IDir, "-odir", ODir]) ->
    {ok, Files} = file:list_dir(IDir),
    SortedFiles = lists:sort(Files),
    ErlFiles = lists:filter(
        fun(Name) -> filename:extension(Name) == ".erlt" end,
        SortedFiles
    ),
    lists:foreach(
        fun(ErlFile) ->
            EtfFile = filename:basename(ErlFile, ".erlt") ++ ".etf",
            IFile = filename:join(IDir, ErlFile),
            OFile = filename:join(ODir, EtfFile),
            ok = main(["-ifile", IFile, "-ofile", OFile])
        end,
        ErlFiles
    ),
    ok.

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
    [{text, Text}, {location, {L1, C1}}] = erl_anno:to_term(erlang:element(2, Tok)),
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
