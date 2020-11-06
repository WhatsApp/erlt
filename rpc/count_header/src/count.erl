-module(count).

-export([main/1]).

main(_Args) ->
    count_server:start(myname, 10),
    true = count_server:equal(myname, 10),
    count_server:inc(myname, 5),
    false = count_server:equal(myname, 10),
    true = count_server:equal(myname, 15),
    count_server:dec(myname, 3),
    false = count_server:equal(myname, 15),
    true = count_server:equal(myname, 12),
    11 = count_server:closer(myname, 11, 17),
    count_server:stop(myname),
    io:format("[ok]~n", []).
