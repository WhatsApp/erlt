-module(count_server).

-behavior(gen_server).

-export([handle_call/3, handle_cast/2, init/1]).

-export([start/2, stop/1, equal/2, closer/3, inc/2, dec/2]).

-type input() :: integer().
-type state() :: integer().

-spec start(gen_server:name(), input()) -> {ok, pid()}.
start(Name, InitArgs) ->
    gen_server:start_link({local, Name}, ?MODULE, InitArgs, []).

-spec stop(gen_server:name()) -> ok.
stop(Name) ->
    gen_server:stop(Name).

-spec equal(gen_server:name(), integer()) -> boolean().
equal(Name, N) ->
    gen_server:call(Name, {equal, N}).

-spec closer(gen_server:name(), integer(), integer()) -> integer().
closer(Name, I, J) ->
    gen_server:call(Name, {closer, I, J}).

-spec inc(gen_server:name(), integer()) -> ok.
inc(Name, N) ->
    gen_server:cast(Name, {inc, N}).

-spec dec(gen_server:name(), integer()) -> ok.
dec(Name, N) ->
    gen_server:cast(Name, {dec, N}).

-spec init(input()) -> {ok, state()}.
init(Input) ->
    {ok, Input}.

-spec handle_call
    ({equal, integer()}, pid(), state()) -> {reply, boolean(), state()};
    ({closer, integer(), integer()}, pid(), state()) -> {reply, integer(), state()}.
handle_call({equal, N}, _From, S) -> {reply, N =:= S, S};
handle_call({closer, I, J}, _From, S) -> {reply, handle_closer(I, J, S), S}.

-spec handle_cast
    ({inc, integer()}, state()) -> {noreply, state()};
    ({dec, integer()}, state()) -> {noreply, state()}.
handle_cast({inc, N}, S) -> {noreply, S + N};
handle_cast({dec, N}, S) -> {noreply, S - N}.

-spec handle_closer(integer(), integer(), state()) -> integer().
handle_closer(I, J, S) ->
    case abs(I - S) < abs(J - S) of
        true -> I;
        false -> J
    end.
