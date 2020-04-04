-lang([erl2, st]).
-module(polymorphic_records).

-export_type([rec1/0, rec2/1, someRec/1, someRecWithId/2]).

-type rec1() :: #{}.
-type rec2(A) :: #{ a := A }.
-type someRec(R) :: #{ _:= R}.
-type idRec(IdType) :: #{id := IdType}.
-type someRecWithId(IdType, Rest) :: #{ id := IdType, _ := Rest}.
