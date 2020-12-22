-file("dev/src/import_enum.erlt", 1).

-module(import_enum).

-eqwalizer_unchecked([]).

-compile([warn_unused_import, warnings_as_errors]).

-export([swap/1]).

-import_type({mod03, [{pair, 2}]}).

-spec swap(mod03:pair(A, B)) -> mod03:pair(B, A).

swap({'$#mod03:pair.pair', A, B}) ->
    {'$#mod03:pair.pair', B, A}.



