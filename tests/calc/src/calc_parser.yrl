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

Nonterminals
expression term factor.

Terminals
number plus minus mult divd lparen rparen var.

Rootsymbol expression.

expression -> term: '$1'.
expression -> expression plus term: calc_core:expr_add('$1', '$3').
expression -> expression minus term: calc_core:expr_subtr( '$1', '$3' ).

term -> term mult factor: calc_core:expr_mult( '$1', '$3' ).
term -> term divd factor: calc_core:expr_divd( '$1', '$3' ).
term -> factor: '$1'.

factor -> number: calc_core:expr_number( unwrap('$1') ).
factor -> var: calc_core:expr_var( unwrap('$1') ).
factor -> lparen expression rparen: '$2'.

Erlang code.

unwrap({_,_,V}) -> V.
