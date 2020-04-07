-lang([erl2, ffi]).
-module(string_ffi).

-export_type([string_ffi/0, string_alias/0]).
-export([length/1, length_ffi/1]).

-spec length(string()) -> integer().
length(S) ->
    erlang:length(S).

-opaque string_ffi() :: string().
-type string_alias() :: string().

-spec length_ffi(string_ffi()) -> integer().
length_ffi(S) ->
    length_ffi_help(S).

-spec length_ffi_help(string_ffi()) -> integer().
length_ffi_help(S) ->
    erlang:length(S).
