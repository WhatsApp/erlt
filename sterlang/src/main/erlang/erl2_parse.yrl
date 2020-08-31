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
pat_expr pat_expr_max map_pat_expr record_pat_expr enum_pat_expr
pat_argument_list pat_exprs
list tail
list_comprehension lc_expr lc_exprs
binary_comprehension
tuple enum_expr
record_expr record_tuple record_field record_fields
map_expr map_tuple map_field map_field_assoc map_field_exact map_fields map_key
if_expr if_clause if_clauses case_expr cr_clause cr_clauses receive_expr
fun_expr fun_clause fun_clauses
try_expr try_catch try_clause try_clauses try_opt_stacktrace
function_call argument_list
remote_id
exprs guard
atomic
prefix_op mult_op add_op list_op comp_op
binary bin_elements bin_element bit_expr
opt_bit_size_expr bit_size_expr opt_bit_type_list bit_type_list bit_type
top_type top_types type typed_expr record_def type_def
fun_type
type_spec typed_exprs typed_record_fields
map_pair_types map_pair_type.

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
'!' '=' '::'
'spec' 'callback' % helper
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

attribute -> '-' atom attr_val               : build_attribute('$2', '$3', ?anno('$1','$3')).
attribute -> '-' atom type_def               : build_typed_attribute('$2','$3', ?anno('$1','$3')).
attribute -> '-' atom '(' record_def ')'     : build_typed_attribute('$2','$4', ?anno('$1','$5')).
attribute -> '-' 'spec' type_spec            : build_type_spec('$2', '$3', ?anno('$1','$3')).
attribute -> '-' 'callback' type_spec        : build_type_spec('$2', '$3', ?anno('$1','$3')).

type_spec -> atom fun_type : {type_spec, ?anno('$1','$2'), '$1', ['$2']}.

record_def -> expr ',' typed_record_fields  : {typed_record, ?anno('$1','$3'), '$1', '$3'}.
type_def -> expr '::' top_type              : {type_def, ?anno('$1','$3'), '$1', '$3'}.

typed_record_fields -> '{' typed_exprs '}' : {tuple, ?anno('$1','$3'), '$2'}.

typed_exprs -> typed_expr                 : ['$1'].
typed_exprs -> typed_expr ',' typed_exprs : ['$1'|'$3'].
typed_exprs -> expr ',' typed_exprs       : ['$1'|'$3'].
typed_exprs -> typed_expr ',' exprs       : ['$1'|'$3'].

typed_expr -> atom '::' top_type          : {typed,'$1','$3'}.

top_types -> top_type                     : ['$1'].
top_types -> top_type ',' top_types       : ['$1'|'$3'].

top_type -> var '::' top_type             : {ann_type, ?anno('$1','$3'), ['$1','$3']}.
top_type -> type '|' top_type             : lift_unions('$1','$3').
top_type -> type                          : '$1'.

type -> '(' top_type ')'                  : '$2'.
type -> var                               : '$1'.
type -> atom                              : '$1'.
type -> atom '{' '}'                      : {type, ?anno('$1', '$3'), enum, '$1', []}.
type -> atom '{' top_types '}'            : {type, ?anno('$1', '$4'), enum, '$1', '$3'}.
type -> atom '(' ')'                      : build_gen_type('$1', ?anno('$1', '$3')).
type -> atom '(' top_types ')'            : build_type('$1', '$3', ?anno('$1', '$4')).
type -> atom ':' atom '(' ')'             : {remote_type, ?anno('$1','$5'), ['$1', '$3', []]}.
type -> atom ':' atom '(' top_types ')'   : {remote_type, ?anno('$1','$6'), ['$1', '$3', '$5']}.
type -> '[' top_type ']'                  : {type, ?anno('$1','$3'), list, ['$2']}.
type -> '#' '{' '}'                       : {type, ?anno('$1','$3'), map, []}.
type -> '#' '{' map_pair_types '}'        : {type, ?anno('$1','$4'), map, '$3'}.
type -> '{' '}'                           : {type, ?anno('$1','$2'), tuple, []}.
type -> '{' top_types '}'                 : {type, ?anno('$1','$3'), tuple, '$2'}.
type -> '#' atom '{' '}'                  : {type, ?anno('$1','$4'), record, ['$2']}.
type -> 'fun' '(' fun_type ')'            : '$3'.

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

attr_val -> '(' expr ')'             : ['$2'].
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
expr -> expr_max : '$1'.

remote_id -> atom ':' atom : {remote, ?anno('$1','$3'), '$1', '$3'}.

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
pat_expr -> enum_pat_expr : '$1'.
pat_expr -> pat_expr_max : '$1'.

pat_expr_max -> var : '$1'.
pat_expr_max -> atomic : '$1'.
pat_expr_max -> list : '$1'.
pat_expr_max -> binary : '$1'.
pat_expr_max -> tuple : '$1'.
pat_expr_max -> '(' pat_expr ')' : '$2'.

enum_pat_expr -> atom '.' atom '{' '}' :
    {enum, ?anno('$1','$5'), '$1', '$3', []}.
enum_pat_expr -> atom '.' atom '{' pat_exprs '}' :
    {enum, ?anno('$1','$6'), '$1', '$3', '$5'}.
enum_pat_expr -> remote_id '.' atom '{' '}' :
    {enum, ?anno('$1','$5'), '$1', '$3', []}.
enum_pat_expr -> remote_id '.' atom '{' pat_exprs '}' :
    {enum, ?anno('$1','$6'), '$1', '$3', '$5'}.

map_pat_expr -> '#''#' map_tuple :
	{open_map, ?anno('$1','$2'),strip_map_tuple('$3')}.
map_pat_expr -> '#' map_tuple :
	{map, ?anno('$1','$2'),strip_map_tuple('$2')}.
map_pat_expr -> pat_expr_max '#' map_tuple :
	{map, ?anno('$1','$3'),'$1',strip_map_tuple('$3')}.
map_pat_expr -> map_pat_expr '#' map_tuple :
	{map, ?anno('$1','$3'),'$1',strip_map_tuple('$3')}.

record_pat_expr -> '#' atom '.' atom :
	{record_index,?anno('$1','$4'),element(3, '$2'),'$4'}.
record_pat_expr -> '#' atom record_tuple :
	{record,?anno('$1','$3'),element(3, '$2'),'$3'}.

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
enum_expr -> atom '.' atom '{' '}' :
    {enum, ?anno('$1','$5'), '$1', '$3', []}.
enum_expr -> atom '.' atom '{' exprs '}' :
    {enum, ?anno('$1','$6'), '$1', '$3', '$5'}.
enum_expr -> remote_id '.' atom '{' '}' :
    {enum, ?anno('$1','$5'), '$1', '$3', []}.
enum_expr -> remote_id '.' atom '{' exprs '}' :
    {enum, ?anno('$1','$6'), '$1', '$3', '$5'}.

map_expr -> '#''#' map_tuple :
	{open_map, ?anno('$1','$2'),strip_map_tuple('$3')}.
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
	{record_index,?anno('$1','$4'),element(3, '$2'),'$4'}.
record_expr -> '#' atom record_tuple :
	{record,?anno('$1','$3'),element(3, '$2'),'$3'}.
record_expr -> expr_max '#' atom '.' atom :
	{record_field,?anno('$2','$5'),'$1',element(3, '$3'),'$5'}.
record_expr -> expr_max '#' atom record_tuple :
	{record,?anno('$2','$4'),'$1',element(3, '$3'),'$4'}.
record_expr -> record_expr '#' atom '.' atom :
	{record_field,?anno('$2','$5'),'$1',element(3, '$3'),'$5'}.
record_expr -> record_expr '#' atom record_tuple :
	{record,?anno('$2','$4'),'$1',element(3, hd('$3')),'$4'}.

record_tuple -> '{' '}' : [].
record_tuple -> '{' record_fields '}' : '$2'.

record_fields -> record_field : ['$1'].
record_fields -> record_field ',' record_fields : ['$1' | '$3'].

record_field -> var '=' expr : {record_field,?anno('$1','$3'),'$1','$3'}.
record_field -> atom '=' expr : {record_field,?anno('$1','$3'),'$1','$3'}.

%% N.B. This is called from expr.
function_call -> remote_id argument_list :
	{call, ?anno('$1','$2'), '$1', element(1, '$2')}.
function_call -> expr_max argument_list :
	{call, ?anno('$1','$2'), '$1', element(1, '$2')}.

if_expr -> 'if' if_clauses 'end' : {'if',?anno('$1','$3'),'$2'}.

if_clauses -> if_clause : ['$1'].
if_clauses -> if_clause ';' if_clauses : ['$1' | '$3'].

if_clause -> guard clause_body :
	{clause,?anno(hd(hd('$1')),'$2'),[],'$1','$2'}.


case_expr -> 'case' expr 'of' cr_clauses 'end' :
	{'case',?anno('$1','$5'),'$2','$4'}.

cr_clauses -> cr_clause : ['$1'].
cr_clauses -> cr_clause ';' cr_clauses : ['$1' | '$3'].

cr_clause -> pat_expr clause_guard clause_body :
	{clause,?anno('$1','$3'),['$1'],'$2','$3'}.

receive_expr -> 'receive' cr_clauses 'end' :
	{'receive',?anno('$1','$3'),'$2'}.
receive_expr -> 'receive' 'after' expr clause_body 'end' :
	{'receive',?anno('$1','$5'),[],'$3','$4'}.
receive_expr -> 'receive' cr_clauses 'after' expr clause_body 'end' :
	{'receive',?anno('$1','$6'),'$2','$4','$5'}.


fun_expr -> 'fun' atom '/' integer :
    {'fun',?anno('$1', '$4'),{function,element(3, '$2'),element(3, '$4')}}.
fun_expr -> 'fun' atom ':' atom '/' integer :
	{'fun',?anno('$1','$6'),{function,'$2','$4','$6'}}.
fun_expr -> 'fun' fun_clauses 'end' :
	build_fun(?anno('$1','$3'), '$2').

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
try_clause -> atom ':' pat_expr try_opt_stacktrace clause_guard clause_body :
	A = ?anno('$1','$6'),
	{clause,A,[{tuple,A,['$1','$3',{var,A,'$4'}]}],'$5','$6'}.
try_clause -> var ':' pat_expr try_opt_stacktrace clause_guard clause_body :
	A = ?anno('$1','$6'),
	{clause,A,[{tuple,A,['$1','$3',{var,A,'$4'}]}],'$5','$6'}.

try_opt_stacktrace -> ':' var : element(3, '$2').
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

-export([parse_form/1]).
-export([map_anno/2]).
-export([get_end_location/1]).

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

%% Entry points compatible to old erl_parse.
%% These really suck and are only here until Calle gets multiple
%% entry points working.

parse_form([{'-', A1}, {atom, A2, spec} | Tokens]) ->
    NewTokens = [{'-', A1}, {'spec', A2} | Tokens],
    parse(NewTokens);
parse_form([{'-', A1}, {atom, A2, callback} | Tokens]) ->
    NewTokens = [{'-', A1}, {'callback', A2} | Tokens],
    parse(NewTokens);
parse_form(Tokens) ->
    parse(Tokens).

build_typed_attribute(
    {atom, _, Attr},
    {typed_record, _TRA, {atom, _An, RecordName}, RecTuple},
    Aa
) when Attr =:= 'record'; Attr =:= 'exception'; Attr =:= 'message' ->
    {attribute, Aa, Attr, {RecordName, record_tuple(RecTuple)}};
build_typed_attribute(
    {atom, _, Attr},
    {type_def, _TDA, {call, _, {atom, _, TypeName}, Args}, Type},
    Aa
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
build_typed_attribute({atom, _, Attr}, _, Aa) ->
    if
        Attr =:= 'record'; Attr =:= 'exception'; Attr =:= 'message'; Attr =:= 'type'; Attr =:= 'opaque'; Attr =:= 'enum' ->
            error_bad_decl(Aa, Attr);
        true ->
            ret_err(Aa, "bad attribute")
    end.

build_type_spec({Kind, _}, {type_spec, _TA, SpecFun, TypeSpecs}, Aa) when Kind =:= spec; Kind =:= callback ->
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

lift_unions(T1, {type, _Aa, union, List}) ->
    {type, ?anno(T1), union, [T1 | List]};
lift_unions(T1, T2) ->
    {type, ?anno(T1), union, [T1, T2]}.

build_gen_type({atom, _, tuple}, Aa) ->
    {type, Aa, tuple, any};
build_gen_type({atom, _, map}, Aa) ->
    {type, Aa, map, any};
build_gen_type(Name, Aa) ->
    build_type(Name, [], Aa).

build_type({atom, _, Name}, Types, A) ->
    Tag = type_tag(Name, length(Types)),
    {Tag, A, Name, Types}.

type_tag(TypeName, NumberOfTypeVariables) ->
    case erl_internal:is_type(TypeName, NumberOfTypeVariables) of
        true -> type;
        false -> user_type
    end.

build_attribute({atom, _, module}, Val, Aa) ->
    case Val of
        [{atom, _, Module}] ->
            {attribute, Aa, module, Module};
        [{atom, _, Module}, ExpList] ->
            {attribute, Aa, module, {Module, var_list(ExpList)}};
        _Other ->
            error_bad_decl(Aa, module)
    end;
build_attribute({atom, _, export}, Val, Aa) ->
    case Val of
        [ExpList] ->
            {attribute, Aa, export, farity_list(ExpList)};
        _Other ->
            error_bad_decl(Aa, export)
    end;
build_attribute({atom, _, import}, Val, Aa) ->
    case Val of
        [{atom, _, Mod}, ImpList] ->
            {attribute, Aa, import, {Mod, farity_list(ImpList)}};
        _Other ->
            error_bad_decl(Aa, import)
    end;
build_attribute({atom, _, import_type}, Val, Aa) ->
    case Val of
        [{atom, _, Mod}, ImpList] ->
            {attribute, Aa, import_type, {Mod, farity_list(ImpList)}};
        _Other ->
            error_bad_decl(Aa, import_type)
    end;
build_attribute({atom, _, exception}, Val, Aa) ->
    case Val of
        [{atom, _, Record}, RecTuple] ->
            {attribute, Aa, exception, {Record, record_tuple(RecTuple)}};
        _Other ->
            error_bad_decl(Aa, exception)
    end;
build_attribute({atom, _, message}, Val, Aa) ->
    case Val of
        [{atom, _, Record}, RecTuple] ->
            {attribute, Aa, message, {Record, record_tuple(RecTuple)}};
        _Other ->
            error_bad_decl(Aa, message)
    end;
build_attribute({atom, _, record}, Val, Aa) ->
    case Val of
        [{atom, _, Record}, RecTuple] ->
            {attribute, Aa, record, {Record, record_tuple(RecTuple)}};
        _Other ->
            error_bad_decl(Aa, record)
    end;
build_attribute({atom, _, file}, Val, Aa) ->
    case Val of
        [{string, _, Name}, {integer, _, Line}] ->
            {attribute, Aa, file, {Name, Line}};
        _Other ->
            error_bad_decl(Aa, file)
    end;
build_attribute({atom, _, Attr}, Val, Aa) ->
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

ret_err(Anno, S) ->
    return_error(erl_anno:location(Anno), S).

%%  Convert between the abstract form of a term and a term.
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

map_anno(F0, Abstr) ->
    F = fun (A, Acc) -> {F0(A), Acc} end,
    {NewAbstr, []} = modify_anno1(Abstr, [], F),
    NewAbstr.

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
