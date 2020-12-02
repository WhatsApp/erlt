-file("typed_libs/src/types.erlt", 1).

-module(types).

-export_type([option/1]).

-type option(V) :: {'$#types:option.none'} |
                   {'$#types:option.some', V}.



