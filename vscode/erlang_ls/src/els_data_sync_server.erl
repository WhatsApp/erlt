%%%=============================================================================
%%% @doc The data sync gen_server.
%%% @end
%%%=============================================================================

-module(els_data_sync_server).

%%==============================================================================
%% API
%%==============================================================================
-export([ start_link/0
        , file_changed/1
        , wait_for_lens_info/1
        , new_diagnostics/1
        ]).

-include("erlang_ls.hrl").

%%==============================================================================
%% Callbacks for the gen_server behaviour
%%==============================================================================
-behaviour(gen_server).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).
-type state() :: #{}.

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(SERVER, ?MODULE).
-define(TIMEOUT, 60_000).

%%==============================================================================
%% API
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, unused, []).

%% @doc Register that the given file has changed, and diagnostics requested
%% against it. Called from diagnostics provider
-spec file_changed(uri()) -> ok.
file_changed(Uri) ->
  gen_server:call(?SERVER, {file_changed, Uri}).

%% @doc Request the lens info. If a change is pending, wait for the diagnostics
%% to be provided before responding.
-spec wait_for_lens_info(uri()) -> ok.
wait_for_lens_info(Uri) ->
  gen_server:call(?SERVER, {wait_for_lens_info, Uri}, ?TIMEOUT).

%% @doc Provide latest diagnostics for the given file, triggering the release of
%% any waiting 'get_lens_info' calls. Called from diagnostics provider
-spec new_diagnostics(uri()) -> ok.
new_diagnostics(Uri) ->
  gen_server:call(?SERVER, {new_diagnostics, Uri}).


%%==============================================================================
%% Callbacks for the gen_server behaviour
%%==============================================================================
-spec init(unused) -> {ok, state()}.
init(unused) ->
  {ok, maps:new()}.

-spec handle_call(any(), {pid(), any()}, state()) ->
        {reply, any(), state()} | {noreply, state()}.
handle_call({file_changed, Uri}, _From, State) ->
  case maps:get(Uri, State, notpresent) of
    notpresent -> ok;
    {_T, undefined} ->
      ok;
    {_T, OriginalFrom} ->
      %% We have a new change, and the prior one has not finished yet.
      %% Release the waiting call
      gen_server:reply(OriginalFrom, ok),
      ok
  end,
  State1 = maps:put(Uri, {changed, undefined}, State),
  {reply, ok, State1};
handle_call({wait_for_lens_info, Uri}, From, State) ->
  case maps:get(Uri, State, notpresent) of
    notpresent ->
      %% We have no record of this URI, do not block.
      {reply, ok, State};
    {changed, undefined} ->
      %% We know diagnostics are running, wait for them to finish before sending
      %% a reply.
      State1 = maps:put(Uri, {changed, From}, State),
      {noreply, State1};
    {ready, undefined} ->
      {reply, ok, State};
    {S, OriginalFrom} ->
      %% We have a new change, and the prior one has not finished yet.
      %% Release the waiting call
      gen_server:reply(OriginalFrom, ok),
      State1 = maps:put(Uri, {S, From}, State),
      {noreply, State1}
  end;
handle_call({new_diagnostics, Uri}, _From, State) ->
  case maps:get(Uri, State, notpresent) of
    notpresent ->
      %% We have no record of this URI, record that diagnostics are available
      State1 = maps:put(Uri, {ready, undefined}),
      {reply, ok, State1};
    {_S, undefined} ->
      %% We have not received a lens request yet.
      State1 = maps:put(Uri, {ready, undefined}, State),
      {reply, ok, State1};
    {_S, OriginalFrom} ->
      %% Normal case: waiting for diagnostics
      gen_server:reply(OriginalFrom, ok),
      State1 = maps:put(Uri, {ready, undefined}, State),
      {reply, ok, State1}
  end.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Request, State) ->
  {noreply, State}.
