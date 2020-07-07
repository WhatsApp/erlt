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

-lang([erl2, st]).
-module(mix).

%% Two selectors
two_selectors(X) -> X.person.name.

% sequencial calls of functions
seq_call(G1, G2, X, Y) ->
    G1(X),
    G2(Y).

%% "Rich records"

sum_fr(F, R) ->
    X01 = F (R.l01),
    X02 = F (R.l02),
    X03 = F (R.l03),
    X04 = F (R.l04),
    X05 = F (R.l05),
    X06 = F (R.l06),
    X07 = F (R.l07),
    X08 = F (R.l08),
    X09 = F (R.l09),
    X10 = F (R.l10),
    X11 = F (R.l11),
    X12 = F (R.l12),
    X13 = F (R.l13),
    X14 = F (R.l14),
    X15 = F (R.l15),
    X16 = F (R.l16),
    X17 = F (R.l17),
    X18 = F (R.l18),
    X19 = F (R.l19),
    ((((((((((((((((((X01 + X02) + X03) + X04) + X05) + X06) + X07) + X08) + X09) + X10) +
       X11) + X12) + X13) + X14) + X15) + X16) + X17) + X18) + X19).

sum_fr_proxy(F, R) -> sum_fr(F, R).
add_f(X) -> X + 1.

rich_record_example() ->
    sum_fr_proxy(
        fun add_f/1,
        #{
            l01 => 01,
            l02 => 02,
            l03 => 03,
            l04 => 04,
            l05 => 05,
            l06 => 06,
            l07 => 07,
            l08 => 08,
            l09 => 09,
            l10 => 10,
            l11 => 11,
            l12 => 12,
            l13 => 13,
            l14 => 14,
            l15 => 15,
            l16 => 16,
            l17 => 17,
            l18 => 18,
            l19 => 19
           }
    ).

-enum rec_rec2(A) :: rec_rec2{A, core:option(rec_rec2(A))}.

name(RecRec) ->
    case RecRec of
        rec_rec2.rec_rec2{X, _} -> X
    end.

next(RecRec) ->
     case RecRec of
         rec_rec2.rec_rec2{_, Y} -> Y
     end.

unBox(Rec) ->
    core:getOrElse(next(Rec), Rec).


-enum rec(A) :: rec{#{name := A, next := core:option(rec(A))}}.

unRec(Rec) ->
    case Rec of rec.rec{X} -> X end.

unRec2(Rec) ->
    core:getOrElse((unRec(Rec)).next, Rec).

apply_add_id(Add_id, Rec) ->
    Rec1 = Add_id(Rec),
    Unused_id = Rec1.id,
    Rec1.

add_id_empty_rec(#{}) ->
    #{id => "ID"}.

add_id_rec_f(#{f := X}) ->
    #{f => X, id => "ID"}.


rec_tricky() ->
    Rec1 = fun (X) -> apply_add_id(fun add_id_empty_rec/1, X) end,
    Rec2 = fun (X) -> apply_add_id(fun add_id_rec_f/1, X) end,
    {}.

even(I) -> (I == 0) orelse odd(I - 1).
odd(I) -> (I <> 0) andalso even(I - 1).

loop({}) ->
    Foo = fun (X) -> X.a end,
    Foo.

dummy() -> loop({}).

%% some stupid funs

id(I) -> I.
x_fun() -> id(2).
y_fun() -> id({}).
