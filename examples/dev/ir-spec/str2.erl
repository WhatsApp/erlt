-file("dev/src/str2.erlt", 1).

-module(str2).

-export_type([str2/0]).

-export([mk_str1/0]).

-type str2() :: {'$#str2:str2', str1:str1()}.

mk_str1() -> {'$#str1:str1', 0}.



