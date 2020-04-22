-lang([erl2, st]).
-module(calc_core).

-export_type([expr/0]).
-export([expr_number/1, expr_add/2, expr_subtr/2, expr_mult/2, expr_divd/2, expr_var/1]).

-enum expr() :: number{integer()}
              | add{expr(), expr()}
              | subtr{expr(), expr()}
              | mult{expr(), expr()}
              | divd{expr(), expr() }
              | var{atom()}.

%% helper functions to be invoked from erl1
-spec expr_number(integer()) -> expr().
expr_number(N) -> expr.number{N}.

-spec expr_add(expr(), expr()) -> expr().
expr_add(E1, E2) -> expr.add{E1, E2}.

-spec expr_subtr(expr(), expr()) -> expr().
expr_subtr(E1, E2) -> expr.subtr{E1, E2}.

-spec expr_mult(expr(), expr()) -> expr().
expr_mult(E1, E2) -> expr.mult{E1, E2}.

-spec expr_divd(expr(), expr()) -> expr().
expr_divd(E1, E2) -> expr.divd{E1, E2}.

-spec expr_var(atom()) -> expr().
expr_var(A) -> expr.var{A}.
