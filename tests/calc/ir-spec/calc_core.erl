-file("calc/src/calc_core.erlt", 1).

-module(calc_core).

-eqwalizer_unchecked([]).

-export_type([expr/0]).

-export([expr_number/1,
         expr_add/2,
         expr_subtr/2,
         expr_mult/2,
         expr_divd/2,
         expr_var/1]).

-type expr() :: {'$#calc_core:expr.number', integer()} |
                {'$#calc_core:expr.add', expr(), expr()} |
                {'$#calc_core:expr.subtr', expr(), expr()} |
                {'$#calc_core:expr.mult', expr(), expr()} |
                {'$#calc_core:expr.divd', expr(), expr()} |
                {'$#calc_core:expr.var', atom()}.

-spec expr_number(integer()) -> expr().

expr_number(N) -> {'$#calc_core:expr.number', N}.

-spec expr_add(expr(), expr()) -> expr().

expr_add(E1, E2) -> {'$#calc_core:expr.add', E1, E2}.

-spec expr_subtr(expr(), expr()) -> expr().

expr_subtr(E1, E2) ->
    {'$#calc_core:expr.subtr', E1, E2}.

-spec expr_mult(expr(), expr()) -> expr().

expr_mult(E1, E2) -> {'$#calc_core:expr.mult', E1, E2}.

-spec expr_divd(expr(), expr()) -> expr().

expr_divd(E1, E2) -> {'$#calc_core:expr.divd', E1, E2}.

-spec expr_var(atom()) -> expr().

expr_var(A) -> {'$#calc_core:expr.var', A}.



