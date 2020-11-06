%% generated by rpctc header
-behaviour(gen_server).

-export([equal/2, closer/3, inc/2, dec/2]).

-export([handle_equal/2, handle_closer/3, handle_inc/2, handle_dec/2]).

-export([handle_call/3, handle_cast/2, init/1]).

-callback handle_init(input()) -> state().
-callback handle_equal(integer(), state()) -> {boolean(), state()}.
-callback handle_closer(integer(), integer(), state()) -> {integer(), state()}.
-callback handle_inc(integer(), state()) -> state().
-callback handle_dec(integer(), state()) -> state().

-spec service_start({atom, gen_server:name()}, input()) -> {ok, pid()} | ignore | {error, term()}.
service_start(RegistrationScheme, InitArgs) ->
    gen_server:start_link(RegistrationScheme, ?MODULE, InitArgs, []).

-spec service_stop(gen_server:name()) -> ok.
service_stop(Name) ->
    gen_server:stop(Name).

-spec equal(gen_server:name(), integer()) -> boolean().
equal(Name, Arg0) ->
    gen_server:call(Name, {handle_equal, Arg0}).

-spec closer(gen_server:name(), integer(), integer()) -> integer().
closer(Name, Arg0, Arg1) ->
    gen_server:call(Name, {handle_closer, Arg0, Arg1}).

-spec inc(gen_server:name(), integer()) -> ok.
inc(Name, Arg0) ->
    gen_server:cast(Name, {handle_inc, Arg0}).

-spec dec(gen_server:name(), integer()) -> ok.
dec(Name, Arg0) ->
    gen_server:cast(Name, {handle_dec, Arg0}).

-spec init(input()) -> {ok, state()}.
init(InitState) ->
    State = handle_init(InitState),
    {ok, State}.

-spec handle_cast
    ({handle_inc, integer()}, state()) -> {noreply, state()};
    ({handle_dec, integer()}, state()) -> {noreply, state()}.
handle_cast({handle_inc, Arg0}, State) ->
    {noreply, (?MODULE:handle_inc)(Arg0, State)};
handle_cast({handle_dec, Arg0}, State) ->
    {noreply, (?MODULE:handle_dec)(Arg0, State)}.

-spec handle_call
    ({handle_equal, integer()}, pid(), state()) -> {reply, boolean(), state()};
    ({handle_closer, integer(), integer()}, pid(), state()) -> {reply, integer(), state()}.
handle_call({handle_equal, Arg0}, _From, State) ->
    {Result, NewState} = (?MODULE:handle_equal)(Arg0, State),
    {reply, Result, NewState};
handle_call({handle_closer, Arg0, Arg1}, _From, State) ->
    {Result, NewState} = (?MODULE:handle_closer)(Arg0, Arg1, State),
    {reply, Result, NewState}.
