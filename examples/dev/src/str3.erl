-lang([erlt]).
-module(str3).

-export_type([str3/0]).
-export([mk_str2/0]).

-struct str3 :: (str2 = #str2:str2{} :: str2:str2()).
mk_str2() -> #str2:str2{}.
mk_str3() -> #str3{}.
