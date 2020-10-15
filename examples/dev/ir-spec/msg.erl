-file("dev/src/msg.erlt", 1).

-module(msg).

-export([mk_reply/1, mk_ping/0]).

-type ping() :: {'$#msg:ping', pid()}.

-type pong() :: {'$#msg:pong',
                 pid(),
                 wrapped_message()}.

-type wrapped_message() :: {969696,
                            msg,
                            wrapped_message,
                            wrapped_message,
                            any()}.

-spec mk_ping() -> any().

mk_ping() -> {'$#msg:ping', erlang:self()}.

-spec mk_reply(any()) -> any().

mk_reply(Ping) ->
    {'$#msg:pong',
     erlang:self(),
     {969696, msg, wrapped_message, wrapped_message, Ping}}.


