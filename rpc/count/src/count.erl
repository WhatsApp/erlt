-module(count).

-export([main/1]).

main(_Args) ->
    count_server:start(two, 9),
    count_server:start(one, 10),
    true = count_server:equal(one, 10),
    true = count_server:equal(two, 9),
    count_server:inc(one, 3),
    count_server:inc(one, 2),
    false = count_server:equal(one, 10),
    true = count_server:equal(one, 15),
    count_server:dec(one, 3),
    false = count_server:equal(one, 15),
    true = count_server:equal(one, 12),
    11 = count_server:closer(one, 11, 17),
    count_server:stop(one),
    io:format("[ok]~n", []).
