-lang([erl2, st]).

-module(etf14).

-export([id/1, local/0, remote/0]).

id(X) -> X.

local() ->
    fun id/1.

remote() ->
    fun etf14:id/1.

%%remote1(M, F, A) ->
%% This is illegal
%%    fun M:F/A.
%%
%% Following examples are legal Erlang:
%%remote2(M, F, A) ->
%%    fun M:id/A.
%%
%%remote3(F, A) ->
%%    fun etf14:F/A.
%%
%%remote4(A) ->
%%    fun etf14:remote/A.
%%
%%remote5(M, F, A) ->
%%    fun M:id/1.
