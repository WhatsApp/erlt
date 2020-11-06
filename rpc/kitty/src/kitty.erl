%% kitty is an example from
%% https://learnyousomeerlang.com/clients-and-servers#callback-to-the-future
%% It has been adjusted to make all calls synchronous
-module(kitty).

-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    order_cat/2,
    return_cat/2,
    close_shop/1
]).

start_link() -> gen_server:start_link(?MODULE, [], []).

order_cat(Pid, Name) ->
    gen_server:call(Pid, {order, Name}).

return_cat(Pid, Name) ->
    gen_server:cast(Pid, {return, Name}).

close_shop(Pid) ->
    gen_server:call(Pid, terminate).

%%% Server functions

%% no treatment of info here!
init([]) -> {ok, []}.

handle_call({order, Name}, _From, Cats) ->
    if
        Cats =:= [] ->
            {reply, Name, Cats};
        Cats =/= [] ->
            {reply, hd(Cats), tl(Cats)}
    end;
handle_call(terminate, _From, Cats) ->
    {stop, normal, ok, Cats}.

handle_cast({return, Cat}, Cats) ->
    {noreply, [Cat | Cats]}.

handle_info(Msg, Cats) ->
    io:format("Unexpected info: ~p~n", [Msg]),
    {noreply, Cats}.

terminate(normal, Cats) ->
    [io:format("~p was set free.~n", [C]) || C <- Cats],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
