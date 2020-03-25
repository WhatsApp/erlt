-module(mod01).

-compile(export_all).

id(X) ->
  X.

arg13(X, Y, Z) ->
  X.
arg23(X, Y, Z) ->
  Y.
arg33(X, Y, Z) ->
  Z.

mk_int() ->
  1.

mk_float() ->
  2.1231231.

mk_string() ->
  "Erlang String".

mk_char() ->
  $c.

mk_nil() ->
  [].

mk_cons(H,T) ->
  [H|T].

mk_tuple(A, B, C) ->
  {A, B, C}.

mk_tuple2(A) ->
  {1, 2, 3, A}.

mk_map(A, B) ->
  #{a => A, b => B}.

update_map(M, A, B) ->
  M#{a := {A, B}, b := {B, A}}.

access_map(M) ->
  {maps:get(id, M), maps:get(location, M)}.

mk_seq() ->
  X = 1,
  Y = 2,
  3,
  4,
  {X, Y}.

mk_seq(X, Y) ->
  3,
  4,
  {X, Y}.

is_empty([]) ->
  true;
is_empty([H|T]) ->
  false.

is_empty2([]) ->
  true;
is_empty2(_) ->
  false.

with_as(1 = X) ->
  X;
with_as(_ = Y) ->
  Y.

block(X, Y) ->
  {
    begin
      Z = [X | Y],
      Z
    end,
    begin
      [X | Y]
    end
  }.

is_empty_case(L) ->
  case L of
     [] -> true;
     _ -> false
  end.

both_empty(L1, L2) ->
  case L1 of
     [] ->
       case L2 of
         [] -> true;
         _ -> false
       end;
     _ -> false
  end.

call(L1, L2) ->
  both_empty(L1, L2).

remote_call(L) ->
  ocaml_list:rev(L).

fun_to_var() ->
  F = fun (X, Y) -> {X, Y} end,
  F.

local_fun_to_var(A, B) ->
  F = fun call/2,
  F.

remote_fun_to_var(A, B) ->
  F = fun ocaml_list:rev/1,
  F.

local_n_fun() ->
  F = fun
      Local([_|T]) -> Local(T);
      Local([]) -> []
      end,
  F.

mod01F(X) ->
  X.

%% Unary operations
unary_plus(X) ->
  + (+ X).

unary_minus(X) ->
  - (- X).

unary_not(X) ->
  not (not X).

unary_bnot(X) ->
  bnot (bnot X).
