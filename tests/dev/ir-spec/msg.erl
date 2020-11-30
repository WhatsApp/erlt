-file("dev/src/msg.erlt", 1).

-module(msg).

-export([mk_reply_1/1, mk_reply_2/1, mk_ping/0]).

-type ping() :: {'$#msg:ping', pid()}.

-type pong() :: {'$#msg:pong',
                 pid(),
                 wrapped_message()}.

-type
     wrapped_message() :: {'$#msg:wrapped_message.wrapped_message',
                           any()}.

-spec mk_ping() -> any().

mk_ping() -> {'$#msg:ping', self_t()}.

-spec mk_reply_1(any()) -> any().

mk_reply_1(Ping) ->
    {'$#msg:pong',
     self_t(),
     {'$#msg:wrapped_message.wrapped_message', Ping}}.

-spec mk_reply_2(any()) -> any().

mk_reply_2(Ping) ->
    A = {'$#msg:wrapped_message.wrapped_message', Ping},
    {'$#msg:pong', self_t(), A}.

-spec self_t() -> pid().

self_t() -> erlang:self().



