-file("dev_enum/src/enum_mod06.erlt", 1).

-module(enum_mod06).

-export_type([enum/0]).

-export([local/0, remote/0]).

-type enum() :: {'$#enum_mod06:enum.x',
                 integer(),
                 float()}.

local() ->
    {{'$#enum_mod06:enum.x', 5, 1.0},
     {'$#enum_mod06:enum.x', 10, 1.0}}.

remote() ->
    {{'$#enum_mod06:enum.x', 5, 1.0},
     {'$#enum_mod06:enum.x', 10, 1.0}}.


