-lang([erl2]).
-module(undefined_struct).

-export([local/0, remote/0]).

local() ->
    #does_not_exist{}.

remote() ->
    #some_module:does_not_exist{}.
