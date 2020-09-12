-lang(st).

-module(etf09).

-export([mk_record/2, update_record/2, update_record/3]).

-spec mk_record(A, B) -> #{a := A, b := B}.
mk_record(A, B) ->
    #{a => A, b => B}.

-spec update_record(#{a := A}, A) -> #{a := A}.
update_record(M, A) ->
    M#{a := A}.

-spec update_record(#{a := A, b := B}, A, B) -> #{a := A, b := B}.
update_record(M, A, B) ->
    M#{a := A}#{b := B}.
