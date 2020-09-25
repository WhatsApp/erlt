-lang([erl2, st]).
-module('import_enum_in_enum').
-compile([warn_unused_import,warnings_as_errors]).

-export([mk_ordered_pair/1, mk_pair/2]).
-export_type([maybe_pair/0]).

-import_type(mod03, [pair/2]).

-enum maybe_pair() :: really_pair{pair(integer(),integer())} | empty{}.


-spec mk_pair(integer(),integer()) -> pair(integer(), integer()).

mk_pair(A, B) -> pair.pair{A,B}.

-spec mk_ordered_pair(pair(integer(), integer())) -> maybe_pair().

mk_ordered_pair(pair.pair{A, B} =  P) when A >= B -> maybe_pair.really_pair{P};
mk_ordered_pair(pair.pair{A, B}) when A < B -> maybe_pair.empty{}.
