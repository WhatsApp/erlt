-lang([erl2, ffi]).
-module(calc_parser_ffi).

-export([parse/1]).

-spec parse(string()) -> calc_core:expr().
parse(Str) ->
    {ok, Tokens, _} = calc_lexer:string(Str),
    {ok, Expr} = calc_parser:parse(Tokens),
    Expr.

