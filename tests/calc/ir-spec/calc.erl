-file("calc/src/calc.erlt", 1).

-module(calc).

-unchecked([]).

-export([calculate/2]).

-export_type([env/0]).

-type env() :: [{atom(), integer()}].

-spec calculate(env(), string()) -> integer().

calculate(Env, Input) ->
    eval(Env, calc_parser_ffi:parse(Input)).

eval(Env, Expr) ->
    case Expr of
        {'$#calc_core:expr.number', N} -> N;
        {'$#calc_core:expr.add', E1, E2} ->
            eval(Env, E1) + eval(Env, E2);
        {'$#calc_core:expr.subtr', E1, E2} ->
            eval(Env, E1) - eval(Env, E2);
        {'$#calc_core:expr.mult', E1, E2} ->
            eval(Env, E1) * eval(Env, E2);
        {'$#calc_core:expr.divd', E1, E2} ->
            eval(Env, E1) / eval(Env, E2);
        {'$#calc_core:expr.var', Name} -> lookup(Name, Env)
    end.

lookup(A, [{A, V} | _]) -> V;
lookup(A, [_ | Rest]) -> lookup(A, Rest).



