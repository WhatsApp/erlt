-lang([erl2, st]).
-module(polymorphic_records).

-export_type([rec1/0, rec2/1, idRec/1]).

-type rec1() :: #{}.

%% This is (correctly) is not allowed.
%%-type someRec(R) :: #{ _:= _}.

-type rec2(A) :: #{ a := A }.
-type idRec(IdType) :: #{id := IdType}.

%% This is (correctly) is not allowed.
%%-type someRecWithId(IdType) :: #{ id := IdType, _ := _}.
