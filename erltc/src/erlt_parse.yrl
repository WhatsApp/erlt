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
expr expr_remote expr_max
pat_expr pat_expr_max map_pat_expr record_pat_expr struct_pat_expr enum_pat_expr
anon_struct_pat_expr
pat_argument_list pat_exprs
list tail
list_comprehension lc_expr lc_exprs
binary_comprehension
tuple enum_expr record_expr
anon_struct_expr anon_struct_field_expr
struct_expr struct_name struct_tuple struct_fields struct_field
map_expr map_tuple map_field map_field_assoc map_field_exact map_fields map_key
if_expr if_clause if_clauses case_expr cr_clause cr_clauses receive_expr
fun_expr fun_clause fun_clauses atom_or_var integer_or_var
try_expr try_catch try_clause try_clauses try_opt_stacktrace
function_call argument_list
exprs guard
atomic strings dot_atom
prefix_op mult_op add_op list_op comp_op
binary bin_elements bin_element bit_expr
opt_bit_size_expr bit_size_expr opt_bit_type_list bit_type_list bit_type
top_type top_types type typed_expr typed_attr_val
type_sig type_sigs type_guard type_guards fun_type anon_fun_type binary_type
type_spec spec_fun typed_exprs typed_record_fields field_types field_type
map_pair_types map_pair_type bin_base_type bin_unit_type
struct_def type_name vars field_defs field_def anon_field_defs non_default_field_def.

Terminals
char integer float atom string var

'(' ')' ',' '->' '{' '}' '[' ']' '|' '||' '<-' ';' ':' '#' '.' '^'
'after' 'begin' 'case' 'try' 'catch' 'end' 'fun' 'if' 'of' 'receive' 'when'
'andalso' 'orelse'
'bnot' 'not'
'*' '/' 'div' 'rem' 'band' 'and'
'+' '-' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor'
'++' '--'
'==' '/=' '=<' '<' '>=' '>' '=:=' '=/=' '<=' '=>' ':='
'<<' '>>'
'!' '=' '::' '..' '...'
'spec' 'callback' struct_like % helper
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
Nonassoc 200 '..'.
Nonassoc 500 '*'. % for binary expressions

form -> attribute dot : '$1'.
form -> function dot : '$1'.

attribute -> '-' atom attr_val               : ?set_anno(build_attribute('$2', '$3'), ?anno('$1','$3')).
attribute -> '-' atom typed_attr_val         : ?set_anno(build_typed_attribute('$2','$3'), ?anno('$1','$3')).
attribute -> '-' atom '(' typed_attr_val ')' : ?set_anno(build_typed_attribute('$2','$4'), ?anno('$1','$5')).
attribute -> '-' 'spec' type_spec            : ?set_anno(build_type_spec('$2', '$3'), ?anno('$1','$3')).
attribute -> '-' 'callback' type_spec        : ?set_anno(build_type_spec('$2', '$3'), ?anno('$1','$3')).
attribute -> '-' struct_like atom struct_def : build_struct_def(?anno('$1', '$4'), '$3', '$4').

dot_atom -> atom : '$1'.
dot_atom -> '.' atom : ?mkop2({atom,?anno('$1'),''}, '$1', '$2').
dot_atom -> dot_atom '.' atom : ?mkop2('$1', '$2', '$3').

struct_def -> type_name '::' '(' ')' : {struct_def, ?anno('$1', '$4'), '$1', []}.
struct_def -> type_name '::' '(' field_defs ')' : {struct_def, ?anno('$1', '$5'), '$1', '$4'}.

type_name -> atom : {call, ?anno('$1'), '$1', []}.
type_name -> atom '(' vars ')' : {call, ?anno('$1', '$4'), '$1', '$3'}.

vars -> var ',' vars : ['$1' | '$3'].
vars -> var : ['$1'].

field_defs -> field_def ',' field_defs : ['$1' | '$3'].
field_defs -> field_def : ['$1'].

anon_field_defs -> non_default_field_def ',' anon_field_defs : ['$1' | '$3'].
anon_field_defs -> non_default_field_def : ['$1'].

field_def -> atom '=' expr '::' type : {field_definition, ?anno('$1', '$5'), '$1', '$3', '$5'}.
field_def -> non_default_field_def : '$1'.
    
non_default_field_def -> atom '::' type : {field_definition, ?anno('$1', '$3'), '$1', undefined, '$3'}.

type_spec -> spec_fun type_sigs : {type_spec, ?anno('$1','$2'), '$1', '$2'}.
type_spec -> '(' spec_fun type_sigs ')' : {type_spec, ?anno('$1','$4'), '$2', '$3'}.

spec_fun ->                       dot_atom : fold_dots('$1').
spec_fun ->              dot_atom ':' atom : {fold_dots('$1'), '$3'}.

typed_attr_val -> expr ',' typed_record_fields : {typed_record, ?anno('$1','$3'), '$1', '$3'}.
typed_attr_val -> expr '::' top_type           : {type_def, ?anno('$1','$3'), '$1', '$3'}.

typed_record_fields -> '{' typed_exprs '}' : {tuple, ?anno('$1','$3'), '$2'}.

typed_exprs -> typed_expr                 : ['$1'].
typed_exprs -> typed_expr ',' typed_exprs : ['$1'|'$3'].
typed_exprs -> expr ',' typed_exprs       : ['$1'|'$3'].
typed_exprs -> typed_expr ',' exprs       : ['$1'|'$3'].

typed_expr -> expr '::' top_type          : {typed,'$1','$3'}.

type_sigs -> type_sig                     : ['$1'].
type_sigs -> type_sig ';' type_sigs       : ['$1'|'$3'].

type_sig -> fun_type                      : '$1'.
type_sig -> fun_type 'when' type_guards   : {type, ?anno('$1','$3'), bounded_fun,
                                             ['$1','$3']}.

type_guards -> type_guard                 : ['$1'].
type_guards -> type_guard ',' type_guards : ['$1'|'$3'].

type_guard -> atom '(' top_types ')'   : build_compat_constraint('$1', '$3').
type_guard -> var '::' top_type        : build_constraint('$1', '$3').

top_types -> top_type                     : ['$1'].
top_types -> top_type ',' top_types       : ['$1'|'$3'].

top_type -> var '::' top_type         : {ann_type, ?anno('$1','$3'), ['$1','$3']}.
top_type -> type '|' top_type     : lift_unions('$1','$3').
top_type -> type                      : '$1'.

type -> type '..' type                    : {type, ?anno('$1','$3'), range, ['$1', '$3']}.
type -> type add_op type                  : ?mkop2('$1', '$2', '$3').
type -> type mult_op type                 : ?mkop2('$1', '$2', '$3').
type -> prefix_op type                    : ?mkop1('$1', '$2').
type -> '(' top_type ')'                  : '$2'.
type -> var                               : '$1'.
type -> dot_atom                          : fold_dots('$1').
type -> dot_atom '{' '}'                  : ?set_anno(build_enum_type('$1', []), ?anno('$1', '$3')).
type -> dot_atom '{' top_types '}'        : ?set_anno(build_enum_type('$1', '$3'), ?anno('$1', '$4')).
type -> dot_atom '(' ')'                  : ?set_anno(build_gen_type('$1'), ?anno('$1', '$3')).
type -> dot_atom '(' top_types ')'        : ?set_anno(build_type('$1', '$3'), ?anno('$1', '$4')).
type -> dot_atom ':' atom '(' ')'         : {remote_type, ?anno('$1','$5'),
                                             [fold_dots('$1'), '$3', []]}.
type -> dot_atom ':' atom '(' top_types ')' : {remote_type, ?anno('$1','$6'),
                                             [fold_dots('$1'), '$3', '$5']}.
type -> '[' ']'                           : {type, ?anno('$1','$2'), nil, []}.
type -> '[' top_type ']'                  : {type, ?anno('$1','$3'), list, ['$2']}.
type -> '[' top_type ',' '...' ']'        : {type, ?anno('$1','$5'),
                                             nonempty_list, ['$2']}.
type -> '#' '{' '}'                       : {type, ?anno('$1','$3'), map, []}.
type -> '#' '{' map_pair_types '}'        : {type, ?anno('$1','$4'), map, '$3'}.
type -> '#' '(' ')'                       : {type, ?anno('$1','$3'), closed_anon_struct, []}.
type -> '#' '(' var ')'                       : {type, ?anno('$1','$3'), open_anon_struct, []}.
type -> '#' '(' anon_field_defs ')'       : {type, ?anno('$1','$4'), closed_anon_struct, '$3'}.
type -> '#' '(' anon_field_defs '|' var ')' : {type, ?anno('$1','$4'), open_anon_struct, '$3'}.
type -> '{' '}'                           : {type, ?anno('$1','$2'), tuple, []}.
type -> '{' top_types '}'                 : {type, ?anno('$1','$3'), tuple, '$2'}.
type -> '#' atom ':' atom '{' '}'         : {type, ?anno('$1','$6'), record, [{qualified_record,'$2','$4'}]}.
type -> '#' atom '{' '}'                  : {type, ?anno('$1','$4'), record, ['$2']}.
type ->
 '#' atom ':' atom '{' field_types '}'    : {type, ?anno('$1','$7'),
                                             record, [{qualified_record,'$2','$4'}|'$6']}.
type -> '#' atom '{' field_types '}'      : {type, ?anno('$1','$5'),
                                             record, ['$2'|'$4']}.
type -> binary_type                       : '$1'.
type -> integer                           : '$1'.
type -> char                              : '$1'.
type -> 'fun' '(' ')'                     : {type, ?anno('$1','$3'), 'fun', []}.
type -> 'fun' '(' anon_fun_type ')'       : '$3'.

anon_fun_type -> '(' '...' ')' '->' top_type :
    {type, ?anno('$1','$5'), 'fun', [{type, ?anno('$1','$5'), any}, '$5']}.
anon_fun_type -> fun_type : '$1'.

fun_type -> '(' ')' '->' top_type :
    {type, ?anno('$1','$4'), 'fun', [{type, ?anno('$1','$4'), product, []}, '$4']}.
fun_type -> '(' top_types ')' '->' top_type :
    {type, ?anno('$1','$5'), 'fun', [{type, ?anno('$1','$5'), product, '$2'},'$5']}.

map_pair_types -> map_pair_type                    : ['$1'].
map_pair_types -> map_pair_type ',' map_pair_types : ['$1'|'$3'].

map_pair_type  -> top_type '=>' top_type  : {type, ?anno('$1','$3'),
                                             map_field_assoc,['$1','$3']}.
map_pair_type  -> top_type ':=' top_type  : {type, ?anno('$1','$3'),
                                             map_field_exact,['$1','$3']}.

field_types -> field_type                 : ['$1'].
field_types -> field_type ',' field_types : ['$1'|'$3'].

field_type -> atom '::' top_type          : {type, ?anno('$1','$3'), field_type,
                                             ['$1', '$3']}.

binary_type -> '<<' '>>'                  : {type, ?anno('$1','$2'),binary,
					     [abstract2(0, ?anno('$1')),
					      abstract2(0, ?anno('$1'))]}.
binary_type -> '<<' bin_base_type '>>'    : {type, ?anno('$1','$3'),binary,
					     ['$2', abstract2(0, ?anno('$1'))]}.
binary_type -> '<<' bin_unit_type '>>'    : {type, ?anno('$1','$3'),binary,
                                             [abstract2(0, ?anno('$1')), '$2']}.
binary_type -> '<<' bin_base_type ',' bin_unit_type '>>'
                                    : {type, ?anno('$1','$5'), binary, ['$2', '$4']}.

bin_base_type -> var ':' type          : build_bin_type(['$1'], '$3').

bin_unit_type -> var ':' var '*' type  : build_bin_type(['$1', '$3'], '$5').

attr_val -> expr                     : ['$1'].
attr_val -> expr ',' exprs           : ['$1' | '$3'].
attr_val -> '(' expr ',' exprs ')'   : ['$2' | '$4'].

function -> function_clauses : build_function('$1').

function_clauses -> function_clause : ['$1'].
function_clauses -> function_clause ';' function_clauses : ['$1'|'$3'].

function_clause -> atom clause_args clause_guard clause_body :
	{clause,?anno('$1','$4'),element(3, '$1'),'$2','$3','$4'}.


clause_args -> pat_argument_list : element(1, '$1').

clause_guard -> 'when' guard : '$2'.
clause_guard -> '$empty' : [].

clause_body -> '->' exprs: '$2'.

expr -> 'catch' expr : {'catch',?anno('$1','$2'),'$2'}.
expr -> expr '=' expr : {match,?anno('$1','$3'),'$1','$3'}.
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
expr -> record_expr : '$1'.
expr -> anon_struct_expr : '$1'.
expr -> anon_struct_field_expr : '$1'.
expr -> struct_expr : '$1'.
expr -> expr_remote : '$1'.

expr_remote -> expr_max ':' expr_max : {remote,?anno('$1','$3'),'$1','$3'}.
expr_remote -> expr_max : '$1'.

expr_max -> expr_max '.' expr_max : ?mkop2('$1', '$2', '$3').
expr_max -> var : '$1'.
expr_max -> atomic : '$1'.
expr_max -> list : '$1'.
expr_max -> binary : '$1'.
expr_max -> list_comprehension : '$1'.
expr_max -> binary_comprehension : '$1'.
expr_max -> tuple : '$1'.
expr_max -> '(' expr ')' : ?set_anno('$2', ?anno('$1', '$3')).
expr_max -> 'begin' exprs 'end' : {block,?anno('$1','$3'),'$2'}.
expr_max -> if_expr : '$1'.
expr_max -> case_expr : '$1'.
expr_max -> receive_expr : '$1'.
expr_max -> fun_expr : '$1'.
expr_max -> try_expr : '$1'.

pat_expr -> pat_expr '=' pat_expr : {match,?anno('$1','$3'),'$1','$3'}.
pat_expr -> pat_expr comp_op pat_expr : ?mkop2('$1', '$2', '$3').
pat_expr -> pat_expr list_op pat_expr : ?mkop2('$1', '$2', '$3').
pat_expr -> pat_expr add_op pat_expr : ?mkop2('$1', '$2', '$3').
pat_expr -> pat_expr mult_op pat_expr : ?mkop2('$1', '$2', '$3').
pat_expr -> prefix_op pat_expr : ?mkop1('$1', '$2').
pat_expr -> map_pat_expr : '$1'.
pat_expr -> record_pat_expr : '$1'.
pat_expr -> anon_struct_pat_expr : '$1'.
pat_expr -> struct_pat_expr : '$1'.
pat_expr -> enum_pat_expr : '$1'.
pat_expr -> pat_expr_max : '$1'.

pat_expr_max -> pat_expr_max '.' pat_expr_max : ?mkop2('$1', '$2', '$3').
pat_expr_max -> var : '$1'.
pat_expr_max -> atomic : '$1'.
pat_expr_max -> list : '$1'.
pat_expr_max -> binary : '$1'.
pat_expr_max -> tuple : '$1'.
pat_expr_max -> '(' pat_expr ')' : '$2'.

enum_pat_expr -> enum_pat_expr '{' '}' : build_enum('$1',[],?anno('$1','$3')).
enum_pat_expr -> pat_expr_max '{' '}' : build_enum('$1',[],?anno('$1','$3')).
enum_pat_expr -> enum_pat_expr '{' pat_exprs '}' : build_enum('$1','$3',?anno('$1','$4')).
enum_pat_expr -> pat_expr_max '{' pat_exprs '}' : build_enum('$1','$3',?anno('$1','$4')).

anon_struct_pat_expr -> '#' '(' ')' : {anon_struct, ?anno('$1', '$3'), []}.
anon_struct_pat_expr -> '#' '(' struct_fields ')' : {anon_struct, ?anno('$1', '$4'), '$3'}.

map_pat_expr -> '#' map_tuple :
	{map, ?anno('$1','$2'),strip_map_tuple('$2')}.
map_pat_expr -> pat_expr_max '#' map_tuple :
	{map, ?anno('$1','$3'),'$1',strip_map_tuple('$3')}.
map_pat_expr -> map_pat_expr '#' map_tuple :
	{map, ?anno('$1','$3'),'$1',strip_map_tuple('$3')}.

record_pat_expr -> '#' atom '.' atom :
	{record_index,?anno('$1','$4'),element(3, '$2'),'$4'}.

struct_pat_expr -> '#' struct_name struct_tuple :
    {struct, ?anno('$1', '$3'), '$2', '$3'}.

list -> '[' ']' : {nil,?anno('$1','$2')}.
list -> '[' expr tail : {cons,?anno('$1','$3'),'$2','$3'}.

tail -> ']' : {nil,?anno('$1')}.
tail -> '|' expr ']' : ?set_anno('$2',?anno('$1','$3')).
tail -> ',' expr tail : {cons,?anno('$2','$3'),'$2','$3'}.


binary -> '<<' '>>' : {bin,?anno('$1','$2'),[]}.
binary -> '<<' bin_elements '>>' : {bin,?anno('$1','$3'),'$2'}.

bin_elements -> bin_element : ['$1'].
bin_elements -> bin_element ',' bin_elements : ['$1'|'$3'].

bin_element -> bit_expr opt_bit_size_expr opt_bit_type_list :
	{bin_element,?anno('$1',filter_defaults(['$3', '$2', '$1'])),'$1','$2',strip_bit_type_list('$3')}.

bit_expr -> prefix_op expr_max : ?mkop1('$1', '$2').
bit_expr -> expr_max : '$1'.

opt_bit_size_expr -> ':' bit_size_expr : '$2'.
opt_bit_size_expr -> '$empty' : default.

opt_bit_type_list -> '/' bit_type_list : '$2'.
opt_bit_type_list -> '$empty' : default.

bit_type_list -> bit_type '-' bit_type_list : ['$1' | '$3'].
bit_type_list -> bit_type : ['$1'].

bit_type -> atom             : '$1'.
bit_type -> atom ':' integer : {bit_type_unit, ?anno('$1', '$3'), '$1','$3'}.

bit_size_expr -> expr_max : '$1'.


list_comprehension -> '[' expr '||' lc_exprs ']' :
	{lc,?anno('$1','$5'),'$2','$4'}.
binary_comprehension -> '<<' expr_max '||' lc_exprs '>>' :
	{bc,?anno('$1','$5'),'$2','$4'}.
lc_exprs -> lc_expr : ['$1'].
lc_exprs -> lc_expr ',' lc_exprs : ['$1'|'$3'].

lc_expr -> expr : '$1'.
lc_expr -> expr '<-' expr : {generate,?anno('$1','$3'),'$1','$3'}.
lc_expr -> binary '<=' expr : {b_generate,?anno('$1','$3'),'$1','$3'}.

tuple -> '{' '}' : {tuple,?anno('$1','$2'),[]}.
tuple -> '{' exprs '}' : {tuple,?anno('$1','$3'),'$2'}.

%% This is called from expr
enum_expr -> expr_remote '{' '}' : build_enum('$1',[],?anno('$1','$3')).
enum_expr -> expr_remote '{' exprs '}' : build_enum('$1','$3',?anno('$1','$4')).

anon_struct_expr -> '#' '(' ')' : {anon_struct, ?anno('$1', '$3'), []}.
anon_struct_expr -> '#' '(' struct_fields ')' : {anon_struct, ?anno('$1', '$4'), '$3'}.
anon_struct_expr -> expr_max '#' '(' ')' : {anon_struct_update, ?anno('$1', '$4'), '$1', []}.
anon_struct_expr -> expr_max '#' '(' struct_fields ')' :
    {anon_struct_update, ?anno('$1', '$5'), '$1', '$4'}.
anon_struct_expr -> anon_struct_expr '#' '(' struct_fields ')' :
    {anon_struct_update, ?anno('$1', '$5'), '$1', '$4'}.
anon_struct_expr -> anon_struct_expr '#' '(' ')' : {anon_struct_update, ?anno('$1', '$4'), '$1', []}.

anon_struct_field_expr -> anon_struct_expr '#' '(' atom ')' : {anon_struct_field, ?anno('$1', '$5'), '$1', '$4'}.
anon_struct_field_expr -> expr_max '#' '(' atom ')' : {anon_struct_field, ?anno('$1', '$5'), '$1', '$4'}.

map_expr -> '#' map_tuple :
	{map, ?anno('$1','$2'),strip_map_tuple('$2')}.
map_expr -> expr_max '#' map_tuple :
	{map, ?anno('$1','$3'),'$1',strip_map_tuple('$3')}.
map_expr -> map_expr '#' map_tuple :
	{map, ?anno('$1','$3'),'$1',strip_map_tuple('$3')}.

map_tuple -> '{' '}' : {map_tuple, ?anno('$1','$2'), []}.
map_tuple -> '{' map_fields '}' : {map_tuple, ?anno('$1','$3'), '$2'}.

map_fields -> map_field : ['$1'].
map_fields -> map_field ',' map_fields : ['$1' | '$3'].

map_field -> map_field_assoc : '$1'.
map_field -> map_field_exact : '$1'.

map_field_assoc -> map_key '=>' expr :
	{map_field_assoc,?anno('$1','$3'),'$1','$3'}.

map_field_exact -> map_key ':=' expr :
	{map_field_exact,?anno('$1','$3'),'$1','$3'}.

map_key -> expr : '$1'.

record_expr -> '#' atom '.' atom :
    {record_index, ?anno('$1', '$4'), '$2', '$4'}.

struct_expr -> '#' struct_name struct_tuple :
    {struct, ?anno('$1', '$3'), '$2', '$3'}.
struct_expr -> expr_max '#' struct_name '.' atom :
    {struct_field, ?anno('$1', '$5'), '$1', '$3', '$5'}.
struct_expr -> struct_expr '#' struct_name '.' atom :
    {struct_field, ?anno('$1', '$5'), '$1', '$3', '$5'}.
struct_expr -> expr_max '#' struct_name struct_tuple :
    {struct, ?anno('$1', '$4'), '$1', '$3', '$4'}.
struct_expr -> struct_expr '#' struct_name struct_tuple :
    {struct, ?anno('$1', '$4'), '$1', '$3', '$4'}.

struct_name -> atom : '$1'.
struct_name -> atom ':' atom : {remote, ?anno('$1', '$3'), '$1', '$3'}.

struct_tuple -> '{' '}' : [].
struct_tuple -> '{' struct_fields '}' : '$2'.

struct_fields -> struct_field : ['$1'].
struct_fields -> struct_field ',' struct_fields : ['$1' | '$3'].

struct_field -> atom '=' expr : {struct_field, ?anno('$1','$3'), '$1', '$3'}.

%% N.B. This is called from expr.

function_call -> expr_remote argument_list :
	{call,?anno('$1','$2'),'$1',element(1, '$2')}.
function_call -> '.' expr_remote argument_list :
        Anno = ?anno('$1','$3'),
        {call,Anno,
         case '$2' of
             {remote,L,M,F} ->
                 %% move dot inside remote
                 {remote,L,?mkop2({atom,Anno,''},'$1',M),F};
             _ ->
                 ?mkop2({atom,Anno,''},'$1','$2')
         end,
         element(1, '$3')}.

if_expr -> 'if' if_clauses 'end' : {'if',?anno('$1','$3'),'$2'}.

if_clauses -> if_clause : ['$1'].
if_clauses -> if_clause ';' if_clauses : ['$1' | '$3'].

if_clause -> guard clause_body :
	{clause,?anno(hd(hd('$1')),'$2'),[],'$1','$2'}.


case_expr -> 'case' expr 'of' cr_clauses 'end' :
	{'case',?anno('$1','$5'),'$2','$4'}.

cr_clauses -> cr_clause : ['$1'].
cr_clauses -> cr_clause ';' cr_clauses : ['$1' | '$3'].

%% FIXME: merl in syntax_tools depends on patterns in a 'case' being
%% full expressions. Therefore, we can't use pat_expr here. There
%% should be a better way.

cr_clause -> expr clause_guard clause_body :
	{clause,?anno('$1','$3'),['$1'],'$2','$3'}.

receive_expr -> 'receive' cr_clauses 'end' :
	{'receive',?anno('$1','$3'),'$2'}.
receive_expr -> 'receive' 'after' expr clause_body 'end' :
	{'receive',?anno('$1','$5'),[],'$3','$4'}.
receive_expr -> 'receive' cr_clauses 'after' expr clause_body 'end' :
	{'receive',?anno('$1','$6'),'$2','$4','$5'}.


fun_expr -> 'fun' dot_atom '/' integer :
        case '$2'of
            {atom,_,_} ->
                {'fun',?anno('$1','$4'),{function,element(3, '$2'),element(3, '$4')}};
            {op,_,'.',M,F} ->
                {'fun',?anno('$1','$4'),{function,M,F,'$4'}}
        end.
fun_expr -> 'fun' atom_or_var ':' atom_or_var '/' integer_or_var :
	{'fun',?anno('$1','$6'),{function,'$2','$4','$6'}}.
fun_expr -> 'fun' fun_clauses 'end' :
	build_fun(?anno('$1','$3'), '$2').

atom_or_var -> atom : '$1'.
atom_or_var -> var : '$1'.

integer_or_var -> integer : '$1'.
integer_or_var -> var : '$1'.

fun_clauses -> fun_clause : ['$1'].
fun_clauses -> fun_clause ';' fun_clauses : ['$1' | '$3'].

fun_clause -> pat_argument_list clause_guard clause_body :
	{clause,?anno('$1','$3'),'fun',element(1, '$1'),'$2','$3'}.

fun_clause -> var pat_argument_list clause_guard clause_body :
	{clause,?anno('$1','$4'),element(3, '$1'),element(1, '$2'),'$3','$4'}.

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
	A = ?anno('$1','$3'),
	{clause,A,[{tuple,A,[{atom,A,throw},'$1',{var,A,'_'}]}],'$2','$3'}.
try_clause -> atom ',' pat_expr try_opt_stacktrace clause_guard clause_body :
	A = ?anno('$1','$6'),
	{clause,A,[{tuple,A,['$1','$3',{var,A,'$4'}]}],'$5','$6'}.
try_clause -> var ',' pat_expr try_opt_stacktrace clause_guard clause_body :
	A = ?anno('$1','$6'),
	{clause,A,[{tuple,A,['$1','$3',{var,A,'$4'}]}],'$5','$6'}.

try_opt_stacktrace -> ',' var : element(3, '$2').
try_opt_stacktrace -> '$empty' : '_'.

argument_list -> '(' ')' : {[],?anno('$1','$2')}.
argument_list -> '(' exprs ')' : {'$2',?anno('$1','$3')}.

pat_argument_list -> '(' ')' : {[],?anno('$1','$2')}.
pat_argument_list -> '(' pat_exprs ')' : {'$2',?anno('$1','$3')}.

exprs -> expr : ['$1'].
exprs -> expr ',' exprs : ['$1' | '$3'].

pat_exprs -> pat_expr : ['$1'].
pat_exprs -> pat_expr ',' pat_exprs : ['$1' | '$3'].

guard -> exprs : ['$1'].
guard -> exprs ';' guard : ['$1'|'$3'].

atomic -> char : '$1'.
atomic -> integer : '$1'.
atomic -> float : '$1'.
atomic -> atom : '$1'.
atomic -> strings : '$1'.

strings -> string : '$1'.
strings -> string strings :
        A0 = ?anno('$1','$2'),
        A1 = case {erl_anno:text(?anno('$1')), erl_anno:text(?anno('$2'))} of
                 {T1, T2} when is_list(T1), is_list(T2) ->
                     %% this normalizes separating whitespace to a single space
                     %% (note that the string quotes are included in the text)
                     erl_anno:set_text(T1 ++ " " ++  T2, A0);
                 _ -> A0
             end,
        {string,A1,element(3, '$1') ++ element(3, '$2')}.

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

Header
"%% This file was automatically generated from the file \"erlt_parse.yrl\"."
"%%"
"%% Copyright Ericsson AB 1996-2015. All Rights Reserved."
"%%"
"%% Licensed under the Apache License, Version 2.0 (the \"License\"); you may"
"%% not use this file except in compliance with the License. You may obtain"
"%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>"
"%%"
"%% Unless required by applicable law or agreed to in writing, software"
"%% distributed under the License is distributed on an \"AS IS\" BASIS,"
"%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied."
"%% See the License for the specific language governing permissions and"
"%% limitations under the License."
"".

Erlang code.

-export([parse_form/1, parse_exprs/1, parse_term/1]).
-export([normalise/1, abstract/1, tokens/1, tokens/2]).
-export([abstract/2, dotted_name/1, balance_dotted/1, fold_dots/1]).
-export([inop_prec/1, preop_prec/1, func_prec/0, max_prec/0]).
-export([type_inop_prec/1, type_preop_prec/1]).
-export([
    map_anno/2,
    fold_anno/3,
    mapfold_anno/3,
    new_anno/1,
    anno_to_term/1,
    anno_from_term/1
]).

-export([get_end_location/1]).

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([{hipe, [{regalloc, linear_scan}]}]).

-export_type([
    abstract_clause/0,
    abstract_expr/0,
    abstract_form/0,
    abstract_type/0,
    form_info/0,
    error_info/0
]).

%% The following types are exported because they are used by syntax_tools
-export_type([af_binelement/1, af_generator/0, af_remote_function/0]).

%% stuff for dotted names
-export([
    concat_dotted/1,
    is_valid_dotted/1,
    is_dotted/1,
    split_dotted/1,
    dotted_last/1,
    dotted_butlast/1,
    dotted_striplast/1
]).

-type dotted_name() :: atom() | string().

-export_type([dotted_name/0]).

%% Removed functions
-removed([
    {set_line, 2, "use erl_anno:set_line/2"},
    {get_attributes, 1, "erl_anno:{column,line,location,text}/1 instead"},
    {get_attribute, 2, "erl_anno:{column,line,location,text}/1 instead"}
]).

%% Start of Abstract Format

-type anno() :: erl_anno:anno().
-type abstract_form() ::
    af_module() |
    af_behavior() |
    af_behaviour() |
    af_export() |
    af_import() |
    af_import_type() |
    af_export_type() |
    af_compile() |
    af_file() |
    af_record_decl() |
    af_type_decl() |
    af_function_spec() |
    af_wild_attribute() |
    af_function_decl().

-type af_module() :: {'attribute', anno(), 'module', module()}.
-type af_behavior() :: {'attribute', anno(), 'behavior', behaviour()}.
-type af_behaviour() :: {'attribute', anno(), 'behaviour', behaviour()}.
-type behaviour() :: atom().
-type af_export() :: {'attribute', anno(), 'export', af_fa_list()}.
-type af_import() :: {'attribute', anno(), 'import', {module(), af_fa_list()}}.
-type af_fa_list() :: [{function_name(), arity()}].
-type af_import_type() :: {'attribute', anno(), 'import_type', {module(), af_ta_list()}}.
-type af_export_type() :: {'attribute', anno(), 'export_type', af_ta_list()}.
-type af_ta_list() :: [{type_name(), arity()}].
-type af_compile() :: {'attribute', anno(), 'compile', any()}.
-type af_file() :: {'attribute', anno(), 'file', {string(), anno()}}.
-type af_record_decl() ::
    {'attribute', anno(), 'record', {record_name(), [af_field_decl()]}}.

-type af_field_decl() :: af_typed_field() | af_field().
-type af_typed_field() ::
    {'typed_record_field', af_field(), abstract_type()}.

-type af_field() ::
    {'record_field', anno(), af_field_name()} |
    {'record_field', anno(), af_field_name(), abstract_expr()}.

-type af_type_decl() ::
    {'attribute', anno(), type_attr(), {type_name(), abstract_type(), [af_variable()]}}.

-type type_attr() :: 'opaque' | 'type' | 'enum'.
-type af_function_spec() ::
    {'attribute', anno(), spec_attr(),
        {{function_name(), arity()}, af_function_type_list()}} |
    {'attribute', anno(), 'spec',
        {{module(), function_name(), arity()}, af_function_type_list()}}.

-type spec_attr() :: 'callback' | 'spec'.
-type af_wild_attribute() :: {'attribute', anno(), atom(), any()}.
-type af_function_decl() ::
    {'function', anno(), function_name(), arity(), af_clause_seq()}.

-type abstract_expr() ::
    af_literal() |
    af_match(abstract_expr()) |
    af_variable() |
    af_tuple(abstract_expr()) |
    af_nil() |
    af_cons(abstract_expr()) |
    af_bin(abstract_expr()) |
    af_binary_op(abstract_expr()) |
    af_unary_op(abstract_expr()) |
    af_record_creation(abstract_expr()) |
    af_record_update(abstract_expr()) |
    af_record_index() |
    af_record_field_access(abstract_expr()) |
    af_map_creation(abstract_expr()) |
    af_map_update(abstract_expr()) |
    af_catch() |
    af_local_call() |
    af_remote_call() |
    af_list_comprehension() |
    af_binary_comprehension() |
    af_block() |
    af_if() |
    af_case() |
    af_try() |
    af_receive() |
    af_local_fun() |
    af_remote_fun() |
    af_fun() |
    af_named_fun().

-type af_record_update(T) ::
    {'record', anno(), abstract_expr(), record_name(), [af_record_field(T)]}.

-type af_catch() :: {'catch', anno(), abstract_expr()}.
-type af_local_call() :: {'call', anno(), af_local_function(), af_args()}.
-type af_remote_call() :: {'call', anno(), af_remote_function(), af_args()}.
-type af_args() :: [abstract_expr()].
-type af_local_function() :: abstract_expr().
-type af_remote_function() ::
    {'remote', anno(), abstract_expr(), abstract_expr()}.

-type af_list_comprehension() ::
    {'lc', anno(), af_template(), af_qualifier_seq()}.

-type af_binary_comprehension() ::
    {'bc', anno(), af_template(), af_qualifier_seq()}.

-type af_template() :: abstract_expr().
-type af_qualifier_seq() :: [af_qualifier(), ...].
-type af_qualifier() :: af_generator() | af_filter().
-type af_generator() ::
    {'generate', anno(), af_pattern(), abstract_expr()} |
    {'b_generate', anno(), af_pattern(), abstract_expr()}.

-type af_filter() :: abstract_expr().
-type af_block() :: {'block', anno(), af_body()}.
-type af_if() :: {'if', anno(), af_clause_seq()}.
-type af_case() :: {'case', anno(), abstract_expr(), af_clause_seq()}.
-type af_try() ::
    {'try', anno(), af_body(), af_clause_seq() | [], af_clause_seq() | [], af_body() | []}.

-type af_clause_seq() :: [af_clause(), ...].
-type af_receive() ::
    {'receive', anno(), af_clause_seq()} |
    {'receive', anno(), af_clause_seq(), abstract_expr(), af_body()}.

-type af_local_fun() ::
    {'fun', anno(), {'function', function_name(), arity()}}.

-type af_remote_fun() ::
    {'fun', anno(), {'function', module(), function_name(), arity()}} |
    {'fun', anno(),
        {'function', af_atom() | af_variable(), af_atom() | af_variable(),
            af_integer() | af_variable()}}.

-type af_fun() :: {'fun', anno(), {'clauses', af_clause_seq()}}.
-type af_named_fun() :: {'named_fun', anno(), fun_name(), af_clause_seq()}.
-type fun_name() :: atom().
-type abstract_clause() :: af_clause().
-type af_clause() ::
    {'clause', anno(), [af_pattern()], af_guard_seq(), af_body()}.

-type af_body() :: [abstract_expr(), ...].
-type af_guard_seq() :: [af_guard()].
-type af_guard() :: [af_guard_test(), ...].
-type af_guard_test() ::
    af_literal() |
    af_variable() |
    af_tuple(af_guard_test()) |
    af_nil() |
    af_cons(af_guard_test()) |
    af_bin(af_guard_test()) |
    af_binary_op(af_guard_test()) |
    af_unary_op(af_guard_test()) |
    af_record_creation(af_guard_test()) |
    af_record_index() |
    af_record_field_access(af_guard_test()) |
    af_map_creation(af_guard_test()) |
    af_map_update(af_guard_test()) |
    af_guard_call() |
    af_remote_guard_call().

-type af_record_field_access(T) ::
    {'record_field', anno(), T, record_name(), af_field_name()}.

-type af_map_creation(T) :: {'map', anno(), [af_assoc(T)]}.
-type af_map_update(T) :: {'map', anno(), T, [af_assoc(T)]}.
-type af_assoc(T) ::
    {'map_field_assoc', anno(), T, T} |
    af_assoc_exact(T).

-type af_assoc_exact(T) :: {'map_field_exact', anno(), T, T}.
-type af_guard_call() :: {'call', anno(), af_atom(), [af_guard_test()]}.
-type af_remote_guard_call() ::
    {'call', anno(), {'remote', anno(), af_lit_atom('erlang'), af_atom()}, [
        af_guard_test()
    ]}.

-type af_pattern() ::
    af_literal() |
    af_match(af_pattern()) |
    af_variable() |
    af_tuple(af_pattern()) |
    af_nil() |
    af_cons(af_pattern()) |
    af_bin(af_pattern()) |
    af_binary_op(af_pattern()) |
    af_unary_op(af_pattern()) |
    af_record_creation(af_pattern()) |
    af_record_index() |
    af_map_pattern().

-type af_record_index() ::
    {'record_index', anno(), record_name(), af_field_name()}.

-type af_record_creation(T) ::
    {'record', anno(), record_name(), [af_record_field(T)]}.

-type af_record_field(T) :: {'record_field', anno(), af_field_name(), T}.
-type af_map_pattern() ::
    {'map', anno(), [af_assoc_exact(af_pattern())]}.

-type abstract_type() ::
    af_annotated_type() |
    af_atom() |
    af_bitstring_type() |
    af_empty_list_type() |
    af_fun_type() |
    af_integer_range_type() |
    af_map_type() |
    af_predefined_type() |
    af_record_type() |
    af_remote_type() |
    af_singleton_integer_type() |
    af_tuple_type() |
    af_type_union() |
    af_type_variable() |
    af_user_defined_type().

-type af_annotated_type() ::
    % [Var, Type]
    {'ann_type', anno(), [af_anno() | abstract_type()]}.

-type af_anno() :: af_variable().
-type af_bitstring_type() ::
    {'type', anno(), 'binary', [af_singleton_integer_type()]}.

-type af_empty_list_type() :: {'type', anno(), 'nil', []}.
-type af_fun_type() ::
    {'type', anno(), 'fun', []} |
    {'type', anno(), 'fun', [
        {'type', anno(), 'any'} |
        abstract_type()
    ]} |
    af_function_type().

-type af_integer_range_type() ::
    {'type', anno(), 'range', [af_singleton_integer_type()]}.

-type af_map_type() ::
    {'type', anno(), 'map', 'any'} |
    {'type', anno(), 'map', [af_assoc_type()]}.

-type af_assoc_type() ::
    {'type', anno(), 'map_field_assoc', [abstract_type()]} |
    {'type', anno(), 'map_field_exact', [abstract_type()]}.

-type af_predefined_type() ::
    {'type', anno(), type_name(), [abstract_type()]}.

-type af_record_type() ::
    % [Name, T1, ... Tk]
    {'type', anno(), 'record', [
        (Name :: af_atom()) |
        af_record_field_type()
    ]}.

-type af_record_field_type() ::
    {'type', anno(), 'field_type', [
        (Name :: af_atom()) |
        % [Name, Type]
        abstract_type()
    ]}.

-type af_remote_type() ::
    {'remote_type', anno(), [
        (Module :: af_atom()) |
        (TypeName :: af_atom()) |
        % [Module, Name, [T]]
        [abstract_type()]
    ]}.

-type af_tuple_type() ::
    {'type', anno(), 'tuple', 'any'} |
    {'type', anno(), 'tuple', [abstract_type()]}.

-type af_type_union() ::
    % at least two
    {'type', anno(), 'union', [abstract_type(), ...]}.

% except '_'
-type af_type_variable() :: {'var', anno(), atom()}.
-type af_user_defined_type() ::
    {'user_type', anno(), type_name(), [abstract_type()]}.

-type af_function_type_list() :: [
    af_constrained_function_type() |
    af_function_type(),
    ...
].

-type af_constrained_function_type() ::
    % [Ft, Fc]
    {'type', anno(), 'bounded_fun', [
        af_function_type() |
        af_function_constraint()
    ]}.

-type af_function_type() ::
    {'type', anno(), 'fun', [
        {'type', anno(), 'product', [abstract_type()]} | abstract_type()
    ]}.

-type af_function_constraint() :: [af_constraint(), ...].
-type af_constraint() ::
    {'type', anno(), 'constraint', [
        af_lit_atom('is_subtype') |
        % [IsSubtype, [V, T]]
        [af_type_variable() | abstract_type()]
    ]}.

-type af_singleton_integer_type() ::
    af_integer() |
    af_character() |
    af_unary_op(af_singleton_integer_type()) |
    af_binary_op(af_singleton_integer_type()).

-type af_literal() ::
    af_atom() |
    af_character() |
    af_float() |
    af_integer() |
    af_string().

-type af_atom() :: af_lit_atom(atom()).
-type af_lit_atom(A) :: {'atom', anno(), A}.
-type af_character() :: {'char', anno(), char()}.
-type af_float() :: {'float', anno(), float()}.
-type af_integer() :: {'integer', anno(), non_neg_integer()}.
-type af_string() :: {'string', anno(), string()}.
-type af_match(T) :: {'match', anno(), af_pattern(), T}.
% | af_anon_variable()
-type af_variable() :: {'var', anno(), atom()}.
%-type af_anon_variable() :: {'var', anno(), '_'}.

-type af_tuple(T) :: {'tuple', anno(), [T]}.
-type af_nil() :: {'nil', anno()}.
-type af_cons(T) :: {'cons', anno(), T, T}.
-type af_bin(T) :: {'bin', anno(), [af_binelement(T)]}.
-type af_binelement(T) ::
    {'bin_element', anno(), T, af_binelement_size(), type_specifier_list()}.

-type af_binelement_size() :: 'default' | abstract_expr().
-type af_binary_op(T) :: {'op', anno(), binary_op(), T, T}.
-type binary_op() ::
    '/' |
    '*' |
    'div' |
    'rem' |
    'band' |
    'and' |
    '+' |
    '-' |
    'bor' |
    'bxor' |
    'bsl' |
    'bsr' |
    'or' |
    'xor' |
    '++' |
    '--' |
    '==' |
    '/=' |
    '=<' |
    '<' |
    '>=' |
    '>' |
    '=:=' |
    '=/='.

-type af_unary_op(T) :: {'op', anno(), unary_op(), T}.
-type unary_op() :: '+' | '-' | 'bnot' | 'not' | '^'.
%% See also lib/stdlib/{src/erl_bits.erl,include/erl_bits.hrl}.
-type type_specifier_list() :: 'default' | [type_specifier(), ...].
-type type_specifier() ::
    type() |
    signedness() |
    endianness() |
    unit().

-type type() ::
    'integer' |
    'float' |
    'binary' |
    'bytes' |
    'bitstring' |
    'bits' |
    'utf8' |
    'utf16' |
    'utf32'.

-type signedness() :: 'signed' | 'unsigned'.
-type endianness() :: 'big' | 'little' | 'native'.
-type unit() :: {'unit', 1..256}.
-type record_name() :: atom().
-type af_field_name() :: af_atom().
-type function_name() :: atom().
-type type_name() :: atom().
-type form_info() ::
    {'eof', erl_anno:line()} |
    {'error', erlt_scan:error_info() | error_info()} |
    {'warning', erlt_scan:error_info() | error_info()}.

%% End of Abstract Format

%% XXX. To be refined.
-type error_description() :: term().
-type error_info() :: {erl_anno:line(), module(), error_description()}.
-type token() :: erlt_scan:token().

%% mkop(Op, Arg) -> {op,Anno,Op,Arg}.
%% mkop(Left, Op, Right) -> {op,Anno,Op,Left,Right}.

%% keep track of annotation info in tokens
-define(anno(Tup), element(2, Tup)).
-define(anno(First, Last), anno(First, Last)).
-define(set_anno(Tup, Anno), setelement(2, Tup, Anno)).

-define(mkop2(L, OpAnno, R), begin
    {__Op, __Anno} = OpAnno,
    {op, ?anno(L, R), __Op, L, R}
end).

-define(mkop1(OpAnno, A), begin
    {__Op, __Anno} = OpAnno,
    {op, ?anno(OpAnno, A), __Op, A}
end).

%-define(DEBUG, true).

-ifdef(DEBUG).

%% Assumes that erl_anno has been compiled with DEBUG=true.
-define(ANNO_CHECK(Tokens),
    [] = [T || T <- Tokens, not is_list(element(2, T))]
).

-else.

-define(ANNO_CHECK(Tokens), ok).

-endif.

%% Entry points compatible to old erl_parse.
%% These really suck and are only here until Calle gets multiple
%% entry points working.

-spec parse_form(Tokens) -> {ok, AbsForm} | {error, ErrorInfo} when
    Tokens :: [token()], AbsForm :: abstract_form(), ErrorInfo :: error_info().
parse_form([{'-', A1}, {atom, A2, spec} | Tokens]) ->
    NewTokens = [{'-', A1}, {'spec', A2} | Tokens],
    ?ANNO_CHECK(NewTokens),
    parse(NewTokens);
parse_form([{'-', A1}, {atom, A2, callback} | Tokens]) ->
    NewTokens = [{'-', A1}, {'callback', A2} | Tokens],
    ?ANNO_CHECK(NewTokens),
    parse(NewTokens);
parse_form([{'-', A1}, {atom, A2, StructLike} = Atom | Tokens]) when
    StructLike =:= struct; StructLike =:= message; StructLike =:= exception
->
    NewTokens = [{'-', A1}, {struct_like, A2}, Atom | Tokens],
    ?ANNO_CHECK(NewTokens),
    parse(NewTokens);
parse_form(Tokens) ->
    ?ANNO_CHECK(Tokens),
    parse(Tokens).

-spec parse_exprs(Tokens) -> {ok, ExprList} | {error, ErrorInfo} when
    Tokens :: [token()], ExprList :: [abstract_expr()], ErrorInfo :: error_info().
parse_exprs(Tokens) ->
    ?ANNO_CHECK(Tokens),
    A = erl_anno:new(0),
    case parse([{atom, A, f}, {'(', A}, {')', A}, {'->', A} | Tokens]) of
        {ok, {function, _Lf, f, 0, [{clause, _Lc, [], [], Exprs}]}} ->
            {ok, Exprs};
        {error, _} = Err ->
            Err
    end.

-spec parse_term(Tokens) -> {ok, Term} | {error, ErrorInfo} when
    Tokens :: [token()], Term :: term(), ErrorInfo :: error_info().
parse_term(Tokens) ->
    ?ANNO_CHECK(Tokens),
    A = erl_anno:new(0),
    case parse([{atom, A, f}, {'(', A}, {')', A}, {'->', A} | Tokens]) of
        {ok, {function, _Af, f, 0, [{clause, _Ac, [], [], [Expr]}]}} ->
            try normalise(Expr) of
                Term -> {ok, Term}
            catch
                _:_R -> {error, {location(?anno(Expr)), ?MODULE, "bad term"}}
            end;
        {ok, {function, _Af, f, A, [{clause, _Ac, [], [], [_E1, E2 | _Es]}]}} ->
            {error, {location(?anno(E2)), ?MODULE, "bad term"}};
        {error, _} = Err ->
            Err
    end.

-type attributes() ::
    'export' |
    'file' |
    'import' |
    'import_type' |
    'module' |
    'opaque' | 'record' | 'type' | 'enum'.

build_typed_attribute(
    {atom, Aa, record},
    {typed_record, _TRA, {atom, _An, RecordName}, RecTuple}
) ->
    {attribute, Aa, record, {RecordName, record_tuple(RecTuple)}};
build_typed_attribute(
    {atom, Aa, exception},
    {typed_record, _TRA, {atom, _An, RecordName}, RecTuple}
) ->
    {attribute, Aa, exception, {RecordName, record_tuple(RecTuple)}};
build_typed_attribute(
    {atom, Aa, message},
    {typed_record, _TRA, {atom, _An, RecordName}, RecTuple}
) ->
    {attribute, Aa, message, {RecordName, record_tuple(RecTuple)}};
build_typed_attribute(
    {atom, Aa, Attr},
    {type_def, _TDA, {call, _, {atom, _, TypeName}, Args}, Type}
) when Attr =:= 'type'; Attr =:= 'opaque'; Attr =:= 'enum' ->
    lists:foreach(
        fun
            ({var, A, '_'}) -> ret_err(A, "bad type variable");
            (_) -> ok
        end,
        Args
    ),
    case
        lists:all(
            fun
                ({var, _, _}) -> true;
                (_) -> false
            end,
            Args
        )
    of
        true -> {attribute, Aa, Attr, {TypeName, Type, Args}};
        false -> error_bad_decl(Aa, Attr)
    end;
build_typed_attribute({atom, Aa, Attr}, _) ->
    case Attr of
        record -> error_bad_decl(Aa, record);
        exception -> error_bad_decl(Aa, exception);
        message -> error_bad_decl(Aa, message);
        type -> error_bad_decl(Aa, type);
        opaque -> error_bad_decl(Aa, opaque);
        enum -> error_bad_decl(Aa, enum);
        _ -> ret_err(Aa, "bad attribute")
    end.

build_struct_def(Anno, {atom, _, Attr}, {struct_def, DefAnno, Name, Fields}) ->
    {call, _, {atom, _, TypeName} = Tag, Args} = Name,
    Type = {type, DefAnno, struct, Tag, Fields},
    {attribute, Anno, Attr, {TypeName, Type, Args}}.

build_type_spec({Kind, Aa}, {type_spec, _TA, SpecFun, TypeSpecs}) when Kind =:= spec; Kind =:= callback ->
    NewSpecFun =
        case SpecFun of
            {atom, _, Fun} ->
                {Fun, find_arity_from_specs(TypeSpecs)};
            {{atom, _, Mod}, {atom, _, Fun}} ->
                {Mod, Fun, find_arity_from_specs(TypeSpecs)}
        end,
    {attribute, Aa, Kind, {NewSpecFun, TypeSpecs}}.

find_arity_from_specs([Spec | _]) ->
    %% Use the first spec to find the arity. If all are not the same,
    %% erl_lint will find this.
    Fun =
        case Spec of
            {type, _, bounded_fun, [F, _]} -> F;
            {type, _, 'fun', _} = F -> F
        end,
    {type, _, 'fun', [{type, _, product, Args}, _]} = Fun,
    length(Args).

%% The 'is_subtype(V, T)' syntax is not supported as of Erlang/OTP
%% 19.0, but is kept for backward compatibility.
build_compat_constraint({atom, _, is_subtype}, [{var, _, _} = LHS, Type]) ->
    build_constraint(LHS, Type);
build_compat_constraint({atom, _, is_subtype}, [LHS, _Type]) ->
    ret_err(?anno(LHS), "bad type variable");
build_compat_constraint({atom, A, Atom}, _Types) ->
    ret_err(A, io_lib:format("unsupported constraint ~tw", [Atom])).

build_constraint({atom, _, is_subtype}, [{var, _, _} = LHS, Type]) ->
    build_constraint(LHS, Type);
build_constraint({atom, A, Atom}, _Foo) ->
    ret_err(A, io_lib:format("unsupported constraint ~tw", [Atom]));
build_constraint({var, A, '_'}, _Types) ->
    ret_err(A, "bad type variable");
build_constraint(LHS, Type) ->
    IsSubType = {atom, ?anno(LHS), is_subtype},
    {type, ?anno(LHS), constraint, [IsSubType, [LHS, Type]]}.

lift_unions(T1, {type, _Aa, union, List}) ->
    {type, ?anno(T1), union, [T1 | List]};
lift_unions(T1, T2) ->
    {type, ?anno(T1), union, [T1, T2]}.

build_gen_type({atom, Aa, tuple}) ->
    {type, Aa, tuple, any};
build_gen_type({atom, Aa, map}) ->
    {type, Aa, map, any};
build_gen_type(Name) ->
    build_type(Name, []).

build_bin_type([{var, _, '_'} | Left], Int) ->
    build_bin_type(Left, Int);
build_bin_type([], Int) ->
    Int;
build_bin_type([{var, Aa, _} | _], _) ->
    ret_err(Aa, "Bad binary type").

build_enum_type(Name, Types) ->
    case erlt_parse:balance_dotted(Name) of
        {op, A, '.', {op, A2, '.', M, E}, N} ->
            {type, A, enum, {remote, A2, fold_dots(M), E}, N, Types};
        {op, A, '.', E, N} ->
            {type, A, enum, E, N, Types};
        {atom, A, _} = N ->
            {type, A, enum, N, Types};
        Other ->
            ret_err(?anno(Other), "bad enum type")
    end.

build_type({op, A, '.', M, N}, Types) ->
    {remote_type, A, [fold_dots(M), N, Types]};
build_type({atom, A, Name}, Types) ->
    Tag = type_tag(Name, length(Types)),
    {Tag, A, Name, Types}.

type_tag(TypeName, NumberOfTypeVariables) ->
    case erl_internal:is_type(TypeName, NumberOfTypeVariables) of
        true -> type;
        false -> user_type
    end.

abstract2(Term, Anno) ->
    Line = erl_anno:line(Anno),
    abstract(Term, Line).

%% build_attribute(AttrName, AttrValue) ->
%%	{attribute,Anno,module,Module}
%%	{attribute,Anno,export,Exports}
%%	{attribute,Anno,import,Imports}
%%	{attribute,Anno,import_type,Imports}
%%	{attribute,Anno,record,{Name,Inits}}
%%	{attribute,Anno,file,{Name,Line}}
%%	{attribute,Anno,Name,Val}

build_attribute({atom, Aa, module}, [{op, _Am, '.', _L, _R} = M]) ->
    build_attribute({atom, Aa, module}, [fold_dots(M)]);
build_attribute({atom, Aa, module}, Val) ->
    case Val of
        [{atom, _Am, Module}] ->
            {attribute, Aa, module, Module};
        [{atom, _Am, Module}, ExpList] ->
            {attribute, Aa, module, {Module, var_list(ExpList)}};
        _Other ->
            error_bad_decl(Aa, module)
    end;
build_attribute({atom, Aa, export}, Val) ->
    case Val of
        [ExpList] ->
            {attribute, Aa, export, farity_list(ExpList)};
        _Other ->
            error_bad_decl(Aa, export)
    end;
build_attribute({atom, Aa, import}, Val) ->
    case Val of
        [{atom, _Am, Mod}, ImpList] ->
            {attribute, Aa, import, {Mod, farity_list(ImpList)}};
        _Other ->
            error_bad_decl(Aa, import)
    end;
build_attribute({atom, Aa, import_type}, Val) ->
    case Val of
        [{atom, _Am, Mod}, ImpList] ->
            {attribute, Aa, import_type, {Mod, farity_list(ImpList)}};
        _Other ->
            error_bad_decl(Aa, import_type)
    end;
build_attribute({atom, Aa, exception}, Val) ->
    case Val of
        [{atom, _An, Record}, RecTuple] ->
            {attribute, Aa, exception, {Record, record_tuple(RecTuple)}};
        _Other ->
            error_bad_decl(Aa, exception)
    end;
build_attribute({atom, Aa, message}, Val) ->
    case Val of
        [{atom, _An, Record}, RecTuple] ->
            {attribute, Aa, message, {Record, record_tuple(RecTuple)}};
        _Other ->
            error_bad_decl(Aa, message)
    end;
build_attribute({atom, Aa, record}, Val) ->
    case Val of
        [{atom, _An, Record}, RecTuple] ->
            {attribute, Aa, record, {Record, record_tuple(RecTuple)}};
        _Other ->
            error_bad_decl(Aa, record)
    end;
build_attribute({atom, Aa, file}, Val) ->
    case Val of
        [{string, _An, Name}, {integer, _Al, Line}] ->
            {attribute, Aa, file, {Name, Line}};
        _Other ->
            error_bad_decl(Aa, file)
    end;
build_attribute({atom, Aa, Attr}, Val) ->
    case Val of
        [Expr0] ->
            Expr = attribute_farity(Expr0),
            {attribute, Aa, Attr, term(Expr)};
        _Other ->
            ret_err(Aa, "bad attribute")
    end.

var_list({cons, _Ac, {var, _, V}, Tail}) ->
    [V | var_list(Tail)];
var_list({nil, _An}) ->
    [];
var_list(Other) ->
    ret_err(?anno(Other), "bad variable list").

attribute_farity({cons, A, H, T}) ->
    {cons, A, attribute_farity(H), attribute_farity(T)};
attribute_farity({tuple, A, Args0}) ->
    Args = attribute_farity_list(Args0),
    {tuple, A, Args};
attribute_farity({map, A, Args0}) ->
    Args = attribute_farity_map(Args0),
    {map, A, Args};
attribute_farity({op, A, '/', {atom, _, _} = Name, {integer, _, _} = Arity}) ->
    {tuple, A, [Name, Arity]};
attribute_farity(Other) ->
    Other.

attribute_farity_list(Args) ->
    [attribute_farity(A) || A <- Args].

%% It is not meaningful to have farity keys.
attribute_farity_map(Args) ->
    [{Op, A, K, attribute_farity(V)} || {Op, A, K, V} <- Args].

-spec error_bad_decl(erl_anno:anno(), attributes()) -> no_return().
error_bad_decl(Anno, S) ->
    ret_err(Anno, io_lib:format("bad ~tw declaration", [S])).

farity_list({cons, _Ac, {op, _Ao, '/', {atom, _Aa, A}, {integer, _Ai, I}}, Tail}) ->
    [{A, I} | farity_list(Tail)];
farity_list({nil, _An}) ->
    [];
farity_list(Other) ->
    ret_err(?anno(Other), "bad function arity").

record_tuple({tuple, _At, Fields}) ->
    record_fields(Fields);
record_tuple(Other) ->
    ret_err(?anno(Other), "bad record declaration").

record_fields([{atom, Aa, A} | Fields]) ->
    [{record_field, Aa, {atom, Aa, A}} | record_fields(Fields)];
record_fields([{match, _Am, {atom, Aa, A}, Expr} | Fields]) ->
    [{record_field, Aa, {atom, Aa, A}, Expr} | record_fields(Fields)];
record_fields([{typed, Expr, TypeInfo} | Fields]) ->
    [Field] = record_fields([Expr]),
    [{typed_record_field, Field, TypeInfo} | record_fields(Fields)];
record_fields([Other | _Fields]) ->
    ret_err(?anno(Other), "bad record field");
record_fields([]) ->
    [].

term(Expr) ->
    try normalise(Expr)
    catch
        _:_R -> ret_err(?anno(Expr), "bad attribute")
    end.

%% build_function([Clause]) -> {function,Anno,Name,Arity,[Clause]}

build_function(Cs) ->
    Name = element(3, hd(Cs)),
    Arity = length(element(4, hd(Cs))),
    {function, ?anno(hd(Cs), Cs), Name, Arity, check_clauses(Cs, Name, Arity)}.

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
    {'try', ?anno(Try, End), Es, Scs, Ccs, As}.

build_enum(Name, Elements, Anno) ->
    case erlt_parse:balance_dotted(Name) of
        {op, _, '.', {op, ModAnno, '.', Mod, Enum}, Ctr} ->
            %% remote enum reference Mod.Enum.Constructor{...}
            {enum, Anno, {remote, ModAnno, Mod, Enum}, Ctr, Elements};
        {op, _, '.', Enum, Ctr} ->
            %% local qualified enum reference Enum.Constructor{...}
            {enum, Anno, Enum, Ctr, Elements};
        {atom, _, _} ->
            ret_err(Anno, "constructor missing enum qualifier");
        _Other ->
            ret_err(Anno, "bad enum")
    end.

-spec ret_err(_, _) -> no_return().
ret_err(Anno, S) ->
    return_error(location(Anno), S).

location(Anno) ->
    erl_anno:location(Anno).

%%  Convert between the abstract form of a term and a term.

-spec normalise(AbsTerm) -> Data when AbsTerm :: abstract_expr(), Data :: term().
normalise({char, _, C}) ->
    C;
normalise({integer, _, I}) ->
    I;
normalise({float, _, F}) ->
    F;
normalise({atom, _, A}) ->
    A;
normalise({string, _, S}) ->
    S;
normalise({nil, _}) ->
    [];
normalise({bin, _, Fs}) ->
    {value, B, _} =
        eval_bits:expr_grp(
            Fs,
            [],
            fun (E, _) ->
                {value, normalise(E), []}
            end,
            [],
            true
        ),
    B;
normalise({cons, _, Head, Tail}) ->
    [normalise(Head) | normalise(Tail)];
normalise({tuple, _, Args}) ->
    list_to_tuple(normalise_list(Args));
normalise({map, _, Pairs} = M) ->
    maps:from_list(
        lists:map(
            fun
                %% only allow '=>'
                ({map_field_assoc, _, K, V}) ->
                    {normalise(K), normalise(V)};
                (_) ->
                    erlang:error({badarg, M})
            end,
            Pairs
        )
    );
normalise({'fun', _, {function, {atom, _, M}, {atom, _, F}, {integer, _, A}}}) ->
    fun M:F/A;
%% Dotted atom
normalise({op, _, '.', _, _} = D) ->
    case dotted_name(D) of
        error -> erlang:error({badarg, D});
        As -> list_to_atom(concat_dotted(As))
    end;
%% Special case for unary +/-.
normalise({op, _, '+', {char, _, I}}) ->
    I;
normalise({op, _, '+', {integer, _, I}}) ->
    I;
normalise({op, _, '+', {float, _, F}}) ->
    F;
%Weird, but compatible!
normalise({op, _, '-', {char, _, I}}) ->
    -I;
normalise({op, _, '-', {integer, _, I}}) ->
    -I;
normalise({op, _, '-', {float, _, F}}) ->
    -F;
normalise(X) ->
    erlang:error({badarg, X}).

normalise_list([H | T]) ->
    [normalise(H) | normalise_list(T)];
normalise_list([]) ->
    [].

fold_dots(A) ->
    case dotted_name(A) of
        error -> A;
        As -> {atom, ?anno(A), list_to_atom(concat_dotted(As))}
    end.

dotted_name(Name) ->
    dotted_name(Name, [], []).

dotted_name({op, _, '.', E1, E2}, Es, As) ->
    dotted_name(E1, [E2 | Es], As);
dotted_name({atom, _, A}, [E | Es], As) ->
    dotted_name(E, Es, [A | As]);
dotted_name({atom, _, A}, [], As) ->
    lists:reverse([A | As]);
dotted_name(_, _, _) ->
    error.

%% ensure that dotted atoms are nested left-associatively even if
%% parentheses were used to force another parse: X.(b.a) -> (X.b).a, and
%% X.(p.(q.r)) -> (X.((p.q).r) -> (X.(p.q)).r) -> ((X.p).q).r)
balance_dotted({op, L1, '.', E1, E2}) ->
    case balance_dotted(E2) of
        {op, L2, '.', E21, {atom, _, _} = E22} ->
            {op, L2, '.', balance_dotted({op, L1, '.', E1, E21}), E22};
        NewE2 ->
            {op, L1, '.', balance_dotted(E1), NewE2}
    end;
balance_dotted(E) ->
    E.

-spec abstract(Data) -> AbsTerm when Data :: term(), AbsTerm :: abstract_expr().
abstract(T) ->
    Anno = erl_anno:new(0),
    abstract(T, Anno, enc_func(erlt_epp:default_encoding())).

-type encoding_func() :: fun((non_neg_integer()) -> boolean()).

%%% abstract/2 takes line and encoding options
-spec abstract(Data, Options) -> AbsTerm when
    Data :: term(),
    Options :: Line | [Option],
    Option :: {line, Line} | {encoding, Encoding},
    Encoding :: 'latin1' | 'unicode' | 'utf8' | 'none' | encoding_func(),
    Line :: erl_anno:line(),
    AbsTerm :: abstract_expr().
abstract(T, Line) when is_integer(Line) ->
    Anno = erl_anno:new(Line),
    abstract(T, Anno, enc_func(erlt_epp:default_encoding()));
abstract(T, Options) when is_list(Options) ->
    Line = proplists:get_value(line, Options, 0),
    Encoding = proplists:get_value(encoding, Options, erlt_epp:default_encoding()),
    EncFunc = enc_func(Encoding),
    Anno = erl_anno:new(Line),
    abstract(T, Anno, EncFunc).

-define(UNICODE(C),
    (C < 16#D800 orelse
        C > 16#DFFF andalso C < 16#FFFE orelse
        C > 16#FFFF andalso C =< 16#10FFFF)
).

enc_func(latin1) -> fun (C) -> C < 256 end;
enc_func(unicode) -> fun (C) -> ?UNICODE(C) end;
enc_func(utf8) -> fun (C) -> ?UNICODE(C) end;
enc_func(none) -> none;
enc_func(Fun) when is_function(Fun, 1) -> Fun;
enc_func(Term) -> erlang:error({badarg, Term}).

abstract(T, A, _E) when is_integer(T) ->
    {integer, A, T};
abstract(T, A, _E) when is_float(T) ->
    {float, A, T};
abstract(T, A, _E) when is_atom(T) ->
    {atom, A, T};
abstract([], A, _E) ->
    {nil, A};
abstract(B, A, _E) when is_bitstring(B) ->
    {bin, A, [abstract_byte(Byte, A) || Byte <- bitstring_to_list(B)]};
abstract([H | T], A, none = E) ->
    {cons, A, abstract(H, A, E), abstract(T, A, E)};
abstract(List, A, E) when is_list(List) ->
    abstract_list(List, [], A, E);
abstract(Tuple, A, E) when is_tuple(Tuple) ->
    {tuple, A, abstract_tuple_list(tuple_to_list(Tuple), A, E)};
abstract(Map, A, E) when is_map(Map) ->
    {map, A, abstract_map_fields(maps:to_list(Map), A, E)};
abstract(Fun, A, E) when is_function(Fun) ->
    case erlang:fun_info(Fun, type) of
        {type, external} ->
            Info = erlang:fun_info(Fun),
            {module, M} = lists:keyfind(module, 1, Info),
            {name, F} = lists:keyfind(name, 1, Info),
            {arity, Arity} = lists:keyfind(arity, 1, Info),
            {'fun', A,
                {function, abstract(M, A, E), abstract(F, A, E), abstract(Arity, A, E)}}
    end.

abstract_list([H | T], String, A, E) ->
    case is_integer(H) andalso H >= 0 andalso E(H) of
        true ->
            abstract_list(T, [H | String], A, E);
        false ->
            AbstrList = {cons, A, abstract(H, A, E), abstract(T, A, E)},
            not_string(String, AbstrList, A, E)
    end;
abstract_list([], String, A, _E) ->
    {string, A, lists:reverse(String)};
abstract_list(T, String, A, E) ->
    not_string(String, abstract(T, A, E), A, E).

not_string([C | T], Result, A, E) ->
    not_string(T, {cons, A, {integer, A, C}, Result}, A, E);
not_string([], Result, _A, _E) ->
    Result.

abstract_tuple_list([H | T], A, E) ->
    [abstract(H, A, E) | abstract_tuple_list(T, A, E)];
abstract_tuple_list([], _A, _E) ->
    [].

abstract_map_fields(Fs, A, E) ->
    [{map_field_assoc, A, abstract(K, A, E), abstract(V, A, E)} || {K, V} <- Fs].

abstract_byte(Byte, A) when is_integer(Byte) ->
    {bin_element, A, {integer, A, Byte}, default, default};
abstract_byte(Bits, A) ->
    Sz = bit_size(Bits),
    <<Val:Sz>> = Bits,
    {bin_element, A, {integer, A, Val}, {integer, A, Sz}, default}.

%%  Generate a list of tokens representing the abstract term.

-spec tokens(AbsTerm) -> Tokens when AbsTerm :: abstract_expr(), Tokens :: [token()].
tokens(Abs) ->
    tokens(Abs, []).

-spec tokens(AbsTerm, MoreTokens) -> Tokens when
    AbsTerm :: abstract_expr(), MoreTokens :: [token()], Tokens :: [token()].
tokens({char, A, C}, More) ->
    [{char, A, C} | More];
tokens({integer, A, N}, More) ->
    [{integer, A, N} | More];
tokens({float, A, F}, More) ->
    [{float, A, F} | More];
tokens({atom, Aa, A}, More) ->
    [{atom, Aa, A} | More];
tokens({var, A, V}, More) ->
    [{var, A, V} | More];
tokens({string, A, S}, More) ->
    [{string, A, S} | More];
tokens({nil, A}, More) ->
    [{'[', A}, {']', A} | More];
tokens({cons, A, Head, Tail}, More) ->
    [{'[', A} | tokens(Head, tokens_tail(Tail, More))];
tokens({tuple, A, []}, More) ->
    [{'{', A}, {'}', A} | More];
tokens({tuple, A, [E | Es]}, More) ->
    [{'{', A} | tokens(E, tokens_tuple(Es, ?anno(E), More))].

tokens_tail({cons, A, Head, Tail}, More) ->
    [{',', A} | tokens(Head, tokens_tail(Tail, More))];
tokens_tail({nil, A}, More) ->
    [{']', A} | More];
tokens_tail(Other, More) ->
    A = ?anno(Other),
    [{'|', A} | tokens(Other, [{']', A} | More])].

tokens_tuple([E | Es], Anno, More) ->
    [{',', Anno} | tokens(E, tokens_tuple(Es, ?anno(E), More))];
tokens_tuple([], Anno, More) ->
    [{'}', Anno} | More].

%% Give the relative precedences of operators.

inop_prec('=') -> {150, 100, 100};
inop_prec('!') -> {150, 100, 100};
inop_prec('orelse') -> {160, 150, 150};
inop_prec('andalso') -> {200, 160, 160};
inop_prec('==') -> {300, 200, 300};
inop_prec('/=') -> {300, 200, 300};
inop_prec('=<') -> {300, 200, 300};
inop_prec('<') -> {300, 200, 300};
inop_prec('>=') -> {300, 200, 300};
inop_prec('>') -> {300, 200, 300};
inop_prec('=:=') -> {300, 200, 300};
inop_prec('=/=') -> {300, 200, 300};
inop_prec('++') -> {400, 300, 300};
inop_prec('--') -> {400, 300, 300};
inop_prec('+') -> {400, 400, 500};
inop_prec('-') -> {400, 400, 500};
inop_prec('bor') -> {400, 400, 500};
inop_prec('bxor') -> {400, 400, 500};
inop_prec('bsl') -> {400, 400, 500};
inop_prec('bsr') -> {400, 400, 500};
inop_prec('or') -> {400, 400, 500};
inop_prec('xor') -> {400, 400, 500};
inop_prec('*') -> {500, 500, 600};
inop_prec('/') -> {500, 500, 600};
inop_prec('div') -> {500, 500, 600};
inop_prec('rem') -> {500, 500, 600};
inop_prec('band') -> {500, 500, 600};
inop_prec('and') -> {500, 500, 600};
inop_prec('#') -> {800, 700, 800};
inop_prec(':') -> {900, 800, 900};
inop_prec('.') -> {900, 900, 1000}.

-type pre_op() :: 'catch' | '+' | '-' | 'bnot' | 'not' | '#' | '^'.

-spec preop_prec(pre_op()) -> {prec(), prec()}.
preop_prec('catch') -> {0, 100};
preop_prec('+') -> {600, 700};
preop_prec('-') -> {600, 700};
preop_prec('bnot') -> {600, 700};
preop_prec('not') -> {600, 700};
preop_prec('^') -> {600, 700};
preop_prec('#') -> {700, 800}.

-spec func_prec() -> {prec(), prec()}.
func_prec() -> {800, 700}.

-spec max_prec() -> prec().
max_prec() -> 1000.

-type prec() :: non_neg_integer().
-type type_inop() ::
    '::' |
    '|' |
    '..' |
    '+' |
    '-' |
    'bor' |
    'bxor' |
    'bsl' | 'bsr' | '*' | '/' | 'div' | 'rem' | 'band'.

-type type_preop() :: '+' | '-' | 'bnot' | '#' | '^'.

-spec type_inop_prec(type_inop()) -> {prec(), prec(), prec()}.
type_inop_prec('=') -> {150, 100, 100};
type_inop_prec('::') -> {150, 150, 160};
type_inop_prec('|') -> {180, 170, 170};
type_inop_prec('..') -> {300, 200, 300};
type_inop_prec('+') -> {400, 400, 500};
type_inop_prec('-') -> {400, 400, 500};
type_inop_prec('bor') -> {400, 400, 500};
type_inop_prec('bxor') -> {400, 400, 500};
type_inop_prec('bsl') -> {400, 400, 500};
type_inop_prec('bsr') -> {400, 400, 500};
type_inop_prec('*') -> {500, 500, 600};
type_inop_prec('/') -> {500, 500, 600};
type_inop_prec('div') -> {500, 500, 600};
type_inop_prec('rem') -> {500, 500, 600};
type_inop_prec('band') -> {500, 500, 600};
type_inop_prec('#') -> {800, 700, 800}.

-spec type_preop_prec(type_preop()) -> {prec(), prec()}.
type_preop_prec('+') -> {600, 700};
type_preop_prec('-') -> {600, 700};
type_preop_prec('bnot') -> {600, 700};
type_preop_prec('^') -> {600, 700};
type_preop_prec('#') -> {700, 800}.

-type erlt_parse_tree() ::
    abstract_clause() |
    abstract_expr() |
    abstract_form() |
    abstract_type().

-spec map_anno(Fun, Abstr) -> NewAbstr when
    Fun :: fun((Anno) -> NewAnno),
    Anno :: erl_anno:anno(),
    NewAnno :: erl_anno:anno(),
    Abstr :: erlt_parse_tree() | form_info(),
    NewAbstr :: erlt_parse_tree() | form_info().
map_anno(F0, Abstr) ->
    F = fun (A, Acc) -> {F0(A), Acc} end,
    {NewAbstr, []} = modify_anno1(Abstr, [], F),
    NewAbstr.

-spec fold_anno(Fun, Acc0, Abstr) -> Acc1 when
    Fun :: fun((Anno, AccIn) -> AccOut),
    Anno :: erl_anno:anno(),
    Acc0 :: term(),
    Acc1 :: term(),
    AccIn :: term(),
    AccOut :: term(),
    Abstr :: erlt_parse_tree() | form_info().
fold_anno(F0, Acc0, Abstr) ->
    F = fun (A, Acc) -> {A, F0(A, Acc)} end,
    {_, NewAcc} = modify_anno1(Abstr, Acc0, F),
    NewAcc.

-spec mapfold_anno(Fun, Acc0, Abstr) -> {NewAbstr, Acc1} when
    Fun :: fun((Anno, AccIn) -> {NewAnno, AccOut}),
    Anno :: erl_anno:anno(),
    NewAnno :: erl_anno:anno(),
    Acc0 :: term(),
    Acc1 :: term(),
    AccIn :: term(),
    AccOut :: term(),
    Abstr :: erlt_parse_tree() | form_info(),
    NewAbstr :: erlt_parse_tree() | form_info().
mapfold_anno(F, Acc0, Abstr) ->
    modify_anno1(Abstr, Acc0, F).

-spec new_anno(Term) -> Abstr when Term :: term(), Abstr :: erlt_parse_tree() | form_info().
new_anno(Term) ->
    F = fun (L, Acc) -> {erl_anno:new(L), Acc} end,
    {NewAbstr, []} = modify_anno1(Term, [], F),
    NewAbstr.

-spec anno_to_term(Abstr) -> term() when Abstr :: erlt_parse_tree() | form_info().
anno_to_term(Abstract) ->
    F = fun (Anno, Acc) -> {erl_anno:to_term(Anno), Acc} end,
    {NewAbstract, []} = modify_anno1(Abstract, [], F),
    NewAbstract.

-spec anno_from_term(Term) -> erlt_parse_tree() | form_info() when Term :: term().
anno_from_term(Term) ->
    F = fun (T, Acc) -> {erl_anno:from_term(T), Acc} end,
    {NewTerm, []} = modify_anno1(Term, [], F),
    NewTerm.

%% make it easier to combine annotations when the second
%% argument may be a list of nodes (possibly empty)
anno(Left, []) -> ?anno(Left);
anno(Left, [_ | _] = Right) -> merge_anno(?anno(Left), ?anno(lists:last(Right)));
anno(Left, Right) -> merge_anno(?anno(Left), ?anno(Right)).

merge_anno(Left, Right) ->
    New = filter_anno(Left),
    LeftLoc = erl_anno:location(New),
    case get_end_location(Right) of
        LeftLoc ->
            New;
        undefined ->
            case erl_anno:location(Right) of
                LeftLoc -> New;
                undefined -> New;
                RightLoc -> [{end_location, RightLoc} | ensure_anno_list(New)]
            end;
        RightLoc ->
            [{end_location, RightLoc} | ensure_anno_list(New)]
    end.

ensure_anno_list(L) when is_integer(L) ->
    [{location, L}];
ensure_anno_list({L, C} = Loc) when is_integer(L), is_integer(C) ->
    [{location, Loc}];
ensure_anno_list(L) when is_list(L) ->
    L.

%% keep annotations to be propagated to parent
filter_anno([{location, _} = A | As]) ->
    [A | filter_anno(As)];
filter_anno([{file, _} = A | As]) ->
    [A | filter_anno(As)];
filter_anno([_ | As]) ->
    filter_anno(As);
filter_anno([]) ->
    [];
filter_anno(Pos) ->
    % Line or {Line,Col}
    Pos.

get_end_location(Anno) when is_list(Anno) ->
    %% use existing end_location annotation if present
    case lists:keyfind(end_location, 1, Anno) of
        false ->
            erl_anno:end_location(Anno);
        {end_location, Pos} ->
            Pos
    end;
get_end_location(Anno) ->
    erl_anno:end_location(Anno).

%% Forms.
modify_anno1({function, F, A}, Ac, _Mf) ->
    {{function, F, A}, Ac};
modify_anno1({function, M, F, A}, Ac, Mf) ->
    {M1, Ac1} = modify_anno1(M, Ac, Mf),
    {F1, Ac2} = modify_anno1(F, Ac1, Mf),
    {A1, Ac3} = modify_anno1(A, Ac2, Mf),
    {{function, M1, F1, A1}, Ac3};
modify_anno1({attribute, A, record, {Name, Fields}}, Ac, Mf) ->
    {A1, Ac1} = Mf(A, Ac),
    {Fields1, Ac2} = modify_anno1(Fields, Ac1, Mf),
    {{attribute, A1, record, {Name, Fields1}}, Ac2};
modify_anno1({attribute, A, exception, {Name, Fields}}, Ac, Mf) ->
    {A1, Ac1} = Mf(A, Ac),
    {Fields1, Ac2} = modify_anno1(Fields, Ac1, Mf),
    {{attribute, A1, exception, {Name, Fields1}}, Ac2};
modify_anno1({attribute, A, message, {Name, Fields}}, Ac, Mf) ->
    {A1, Ac1} = Mf(A, Ac),
    {Fields1, Ac2} = modify_anno1(Fields, Ac1, Mf),
    {{attribute, A1, message, {Name, Fields1}}, Ac2};
modify_anno1({attribute, A, spec, {Fun, Types}}, Ac, Mf) ->
    {A1, Ac1} = Mf(A, Ac),
    {Types1, Ac2} = modify_anno1(Types, Ac1, Mf),
    {{attribute, A1, spec, {Fun, Types1}}, Ac2};
modify_anno1({attribute, A, callback, {Fun, Types}}, Ac, Mf) ->
    {A1, Ac1} = Mf(A, Ac),
    {Types1, Ac2} = modify_anno1(Types, Ac1, Mf),
    {{attribute, A1, callback, {Fun, Types1}}, Ac2};
modify_anno1({attribute, A, type, {TypeName, TypeDef, Args}}, Ac, Mf) ->
    {A1, Ac1} = Mf(A, Ac),
    {TypeDef1, Ac2} = modify_anno1(TypeDef, Ac1, Mf),
    {Args1, Ac3} = modify_anno1(Args, Ac2, Mf),
    {{attribute, A1, type, {TypeName, TypeDef1, Args1}}, Ac3};
modify_anno1({attribute, A, opaque, {TypeName, TypeDef, Args}}, Ac, Mf) ->
    {A1, Ac1} = Mf(A, Ac),
    {TypeDef1, Ac2} = modify_anno1(TypeDef, Ac1, Mf),
    {Args1, Ac3} = modify_anno1(Args, Ac2, Mf),
    {{attribute, A1, opaque, {TypeName, TypeDef1, Args1}}, Ac3};
modify_anno1({attribute, A, enum, {TypeName, TypeDef, Args}}, Ac, Mf) ->
    {A1, Ac1} = Mf(A, Ac),
    {TypeDef1, Ac2} = modify_anno1(TypeDef, Ac1, Mf),
    {Args1, Ac3} = modify_anno1(Args, Ac2, Mf),
    {{attribute, A1, enum, {TypeName, TypeDef1, Args1}}, Ac3};
modify_anno1({attribute, A, Attr, Val}, Ac, Mf) ->
    {A1, Ac1} = Mf(A, Ac),
    {{attribute, A1, Attr, Val}, Ac1};
modify_anno1({warning, W}, Ac, _Mf) ->
    {{warning, W}, Ac};
modify_anno1({error, W}, Ac, _Mf) ->
    {{error, W}, Ac};
modify_anno1({eof, L}, Ac, _Mf) ->
    {{eof, L}, Ac};
%% Expressions.
modify_anno1({clauses, Cs}, Ac, Mf) ->
    {Cs1, Ac1} = modify_anno1(Cs, Ac, Mf),
    {{clauses, Cs1}, Ac1};
modify_anno1({typed_record_field, Field, Type}, Ac, Mf) ->
    {Field1, Ac1} = modify_anno1(Field, Ac, Mf),
    {Type1, Ac2} = modify_anno1(Type, Ac1, Mf),
    {{typed_record_field, Field1, Type1}, Ac2};
modify_anno1({Tag, A}, Ac, Mf) ->
    {A1, Ac1} = Mf(A, Ac),
    {{Tag, A1}, Ac1};
modify_anno1({Tag, A, E1}, Ac, Mf) ->
    {A1, Ac1} = Mf(A, Ac),
    {E11, Ac2} = modify_anno1(E1, Ac1, Mf),
    {{Tag, A1, E11}, Ac2};
modify_anno1({Tag, A, E1, E2}, Ac, Mf) ->
    {A1, Ac1} = Mf(A, Ac),
    {E11, Ac2} = modify_anno1(E1, Ac1, Mf),
    {E21, Ac3} = modify_anno1(E2, Ac2, Mf),
    {{Tag, A1, E11, E21}, Ac3};
modify_anno1({bin_element, A, E1, E2, TSL}, Ac, Mf) ->
    {A1, Ac1} = Mf(A, Ac),
    {E11, Ac2} = modify_anno1(E1, Ac1, Mf),
    {E21, Ac3} = modify_anno1(E2, Ac2, Mf),
    {{bin_element, A1, E11, E21, TSL}, Ac3};
modify_anno1({Tag, A, E1, E2, E3}, Ac, Mf) ->
    {A1, Ac1} = Mf(A, Ac),
    {E11, Ac2} = modify_anno1(E1, Ac1, Mf),
    {E21, Ac3} = modify_anno1(E2, Ac2, Mf),
    {E31, Ac4} = modify_anno1(E3, Ac3, Mf),
    {{Tag, A1, E11, E21, E31}, Ac4};
modify_anno1({Tag, A, E1, E2, E3, E4}, Ac, Mf) ->
    {A1, Ac1} = Mf(A, Ac),
    {E11, Ac2} = modify_anno1(E1, Ac1, Mf),
    {E21, Ac3} = modify_anno1(E2, Ac2, Mf),
    {E31, Ac4} = modify_anno1(E3, Ac3, Mf),
    {E41, Ac5} = modify_anno1(E4, Ac4, Mf),
    {{Tag, A1, E11, E21, E31, E41}, Ac5};
modify_anno1([H | T], Ac, Mf) ->
    {H1, Ac1} = modify_anno1(H, Ac, Mf),
    {T1, Ac2} = modify_anno1(T, Ac1, Mf),
    {[H1 | T1], Ac2};
modify_anno1([], Ac, _Mf) ->
    {[], Ac};
modify_anno1(E, Ac, _Mf) when not is_tuple(E), not is_list(E) ->
    {E, Ac}.

%% support functions for dotted names (move elsewhere later)

%% `concat_dotted' does not insert a leading dot if the first segment is
%% the empty string or empty atom. However, if any of the segments after
%% the first are empty, the result may contain leading, consecutive or
%% dangling dot characters. Use 'is_valid_dotted' afterwards if needed.

-spec concat_dotted([dotted_name()]) -> string().
concat_dotted(['' | T]) ->
    concat_dotted_1(T);
concat_dotted(["" | T]) ->
    concat_dotted_1(T);
concat_dotted(L) ->
    concat_dotted_1(L).

concat_dotted_1([H]) when is_atom(H) ->
    atom_to_list(H);
concat_dotted_1([H]) ->
    H;
concat_dotted_1([H | T]) when is_atom(H) ->
    atom_to_list(H) ++ "." ++ concat_dotted_1(T);
concat_dotted_1([H | T]) ->
    H ++ "." ++ concat_dotted_1(T);
concat_dotted_1([]) ->
    "";
concat_dotted_1(Name) ->
    erlang:error({badarg, Name}).

%% dotted names may not begin or end with a dot, or have consecutive dots
-spec is_valid_dotted(dotted_name()) -> boolean().
is_valid_dotted(Name) when is_atom(Name) ->
    is_valid_dotted(atom_to_list(Name));
is_valid_dotted([$. | _]) ->
    false;
is_valid_dotted(Name) ->
    is_valid_dotted_1(Name).

is_valid_dotted_1([$.]) -> false;
is_valid_dotted_1([$., $. | _]) -> false;
is_valid_dotted_1([_ | T]) -> is_valid_dotted_1(T);
is_valid_dotted_1([]) -> true;
is_valid_dotted_1(_) -> false.

-spec split_dotted(dotted_name()) -> [string()].
split_dotted(Name) when is_atom(Name) ->
    split_dotted_1(atom_to_list(Name), []);
split_dotted(Name) ->
    split_dotted_1(Name, []).

split_dotted_1([$. | T], Cs) ->
    [lists:reverse(Cs) | split_dotted_1(T, [])];
split_dotted_1([H | T], Cs) when is_integer(H), H >= 0 ->
    split_dotted_1(T, [H | Cs]);
split_dotted_1([], Cs) ->
    [lists:reverse(Cs)];
split_dotted_1(_, _) ->
    erlang:error(badarg).

%% This is equivalent to testing if `split_dotted(Name)' yields a list of
%% length larger than one (i.e., if the name can be split into two or more
%% segments), but is cheaper.

-spec is_dotted(dotted_name()) -> boolean().
is_dotted(Name) when is_atom(Name) ->
    is_dotted_1(atom_to_list(Name));
is_dotted(Name) ->
    is_dotted_1(Name).

is_dotted_1([$. | _]) -> true;
is_dotted_1([_ | T]) -> is_dotted_1(T);
is_dotted_1([]) -> false;
is_dotted_1(_) -> erlang:error(badarg).

-spec dotted_last(dotted_name()) -> string().
dotted_last(Name) ->
    %% can be done cheaper by not doing a full split
    dotted_last_1(split_dotted(Name)).

dotted_last_1([H]) -> H;
dotted_last_1([_ | T]) -> dotted_last_1(T).

-spec dotted_butlast(dotted_name()) -> [string()].
dotted_butlast(Name) ->
    %% can be done cheaper by not doing a full split
    dotted_butlast_1(split_dotted(Name)).

dotted_butlast_1([H | T]) when T =/= [] -> [H | dotted_butlast_1(T)];
dotted_butlast_1(_) -> [].

-spec dotted_striplast(dotted_name()) -> string().
dotted_striplast(Name) ->
    concat_dotted(dotted_butlast(Name)).

%% vim: ft=erlang
