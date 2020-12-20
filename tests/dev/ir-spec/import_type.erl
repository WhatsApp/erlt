-file("dev/src/import_type.erlt", 1).

-module(import_type).

-unchecked([]).

-compile([warn_unused_import, warnings_as_errors]).

-export([f/1]).

-import_type({mod01, [{my_pair, 2}]}).

-spec f(mod01:my_pair(boolean(),
                      integer())) -> integer().

f(X) ->
    case X of
        {true, N} -> N;
        {false, _} -> 0
    end.



