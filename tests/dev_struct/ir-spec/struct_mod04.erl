-file("dev_struct/src/struct_mod04.erlt", 1).

-module(struct_mod04).

-eqwalizer_unchecked([]).

-export_type([struct/0]).

-export([local/0, remote/0]).

-type struct() :: {'$#struct_mod04:struct',
                   integer(),
                   float()}.

-spec local() -> {struct(), struct()}.

local() ->
    {{'$#struct_mod04:struct', 5, 1.0},
     {'$#struct_mod04:struct', 10, 1.0}}.

-spec remote() -> {struct(), struct()}.

remote() ->
    {{'$#struct_mod04:struct', 5, 1.0},
     {'$#struct_mod04:struct', 10, 1.0}}.



