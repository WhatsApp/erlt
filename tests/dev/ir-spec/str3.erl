-file("dev/src/str3.erlt", 1).

-module(str3).

-unchecked([{mk_str2, 0}, {mk_str3, 0}]).

-export_type([str3/0]).

-export([mk_str2/0, mk_str3/0]).

-type str3() :: {'$#str3:str3', str2:str2()}.

mk_str2() -> {'$#str2:str2', {'$#str1:str1', 0}}.

mk_str3() ->
    {'$#str3:str3', {'$#str2:str2', {'$#str1:str1', 0}}}.



