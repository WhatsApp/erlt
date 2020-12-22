-file("dev/src/polymorphic_records.erlt", 1).

-module(polymorphic_records).

-eqwalizer_unchecked([]).

-export_type([rec1/0, rec2/1, idRec/1]).

-type rec1() :: #{}.

-type rec2(A) :: #{a := A}.

-type idRec(IdType) :: #{id := IdType}.



