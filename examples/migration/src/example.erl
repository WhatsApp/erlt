-lang([erl2, st]).
-module(example).

-export([main/0]).
-import_type(example_ffi, [msg/0]).
-import_type(erlang2, [timer_dst/0]).

-spec main() -> {}.
main() ->
    Pid = erlang:spawn(fun example_ffi:loop/0),
    erlang2:start_timer(1000, timer_dst.pid{Pid}, msg.log{"Log1"}),
    erlang2:start_timer(2000, timer_dst.pid{Pid}, msg.log{"Log2"}),
    erlang2:start_timer(3000, timer_dst.pid{Pid}, msg.stop{}),
    {}.
