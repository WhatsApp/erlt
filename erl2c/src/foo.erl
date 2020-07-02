%% =====================================================================
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright (C) 2020, Richard Carlsson
%% @doc

-module('foo').

-include_lib("eunit/include/eunit.hrl").

-export([f/1]).


%% hello
%% world
f(X) ->
    {ok, {}, X}. %% bye
                 %% love
