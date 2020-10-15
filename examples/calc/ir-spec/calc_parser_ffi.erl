-file("calc/src/calc_parser_ffi.erlt", 1).


-module(calc_parser_ffi).


-export([parse/1]).


-spec parse(string()) -> calc_core:expr().


parse(Str) ->
    {ok, Tokens, _} = calc_lexer:string(Str),
    {ok, Expr} = calc_parser:parse(Tokens),
    Expr.





