-file("dev/src/str1.erlt", 1).

-module(str1).

-export_type([str1/0]).

-export([mk_str2/0]).

-type str1() :: {'$#str1:str1', integer()}.

mk_str2() -> {'$#str2:str2', {'$#str1:str1', 0}}.



