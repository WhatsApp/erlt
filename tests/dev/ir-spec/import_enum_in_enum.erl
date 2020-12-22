-file("dev/src/import_enum_in_enum.erlt", 1).

-module(import_enum_in_enum).

-eqwalizer_unchecked([]).

-compile([warn_unused_import, warnings_as_errors]).

-export([mk_ordered_pair/1, mk_pair/2]).

-export_type([maybe_pair/0]).

-import_type({mod03, [{pair, 2}]}).

-type
     maybe_pair() :: {'$#import_enum_in_enum:maybe_pair.really_pair',
                      mod03:pair(integer(), integer())} |
                     {'$#import_enum_in_enum:maybe_pair.empty'}.

-spec mk_pair(integer(),
              integer()) -> mod03:pair(integer(), integer()).

mk_pair(A, B) -> {'$#mod03:pair.pair', A, B}.

-spec mk_ordered_pair(mod03:pair(integer(),
                                 integer())) -> maybe_pair().

mk_ordered_pair({'$#mod03:pair.pair', A, B} = P)
    when A >= B ->
    {'$#import_enum_in_enum:maybe_pair.really_pair', P};
mk_ordered_pair({'$#mod03:pair.pair', A, B})
    when A < B ->
    {'$#import_enum_in_enum:maybe_pair.empty'}.



