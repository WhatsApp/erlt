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

Definitions.

NUMBER = [0-9]+
WS = [\s\t]
LB = \n|\r\n|\r
VAR = [A-Za-z][A-Za-z0-9]*

Rules.
{VAR}      : {token, {var, TokenLine, list_to_atom(TokenChars)}}.
{NUMBER}    : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
\+          : {token, {plus, TokenLine}}.
\-          : {token, {minus, TokenLine}}.
\*          : {token, {mult, TokenLine}}.
\/          : {token, {divd, TokenLine}}.
\(          : {token, {lparen, TokenLine}}.
\)          : {token, {rparen, TokenLine}}.
{WS}        : skip_token.
{LB}        : skip_token.

Erlang code.
