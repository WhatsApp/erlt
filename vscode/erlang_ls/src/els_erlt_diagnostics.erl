%%==============================================================================
%% Erlt diagnostics
%%==============================================================================
-module(els_erlt_diagnostics).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ compile/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Type Definitions
%%==============================================================================

%%==============================================================================
%% API Functions
%%==============================================================================

%% -spec source() -> binary().
%% source() ->
%%   <<"ErlT">>.

-spec compile(uri()) -> [els_diagnostics:diagnostic()].
compile(Uri) ->
  %% Path = els_utils:to_list(els_uri:path(Uri)),
  %% WS = [],
  TempFile = temporary_file(),
  filelib:ensure_dir(TempFile),
  Cmd = lists:flatten(io_lib:format("ERLT_LANGUAGE_SERVER=~s rebar3 compile",
                      [TempFile])),
  os:cmd(Cmd),
  {ok, FileName} = get_els_file(TempFile, Uri),
  {ok, [{_TS, WS, ES}]} = file:consult(FileName),
  Path = els_utils:to_list(els_uri:path(Uri)),

  els_compiler_diagnostics:diagnostics(Path, WS, ?DIAGNOSTIC_WARNING) ++
       els_compiler_diagnostics:diagnostics(Path, ES, ?DIAGNOSTIC_ERROR).

  %% lager:info("compile:[~p]", {Uri, Cmd, Diags}),
  %% Message = io_lib:format("Cmd, FileName, Diags: [~p]",
  %%                         [{Cmd, FileName, Diags}]),
  %% [diagnostic(#{ from => {1, 1}, to => {2, 1} }
  %%            , blah, Message, ?DIAGNOSTIC_WARNING)].


-spec get_els_file(file:filename(), uri())
                  -> {ok, els_uri:path()} | {error, term()} .
get_els_file(TempFile, Uri) ->
  UriFileName = els_utils:to_list(els_uri:path(Uri)),
  case file:consult(TempFile) of
    {error, Reason} -> {error, Reason};
    {ok, Mapping} ->
      case lists:keyfind(UriFileName, 1, Mapping) of
        false -> {error, {uri_not_found, UriFileName}};
        {_, {_TS, FileName}} -> {ok, FileName}
      end
  end.

%%==============================================================================
%% Private Functions
%%==============================================================================

%% -spec diagnostic(poi_range(), module(), string(), integer()) ->
%%         els_diagnostics:diagnostic().
%% diagnostic(Range, _Module, Desc, Severity) ->
%%   %% Message0 = lists:flatten(Module:format_error(Desc)),
%%   Message0 = Desc,
%%   Message  = els_utils:to_binary(Message0),
%%   #{ range    => els_protocol:range(Range)
%%    , message  => Message
%%    , severity => Severity
%%    , source   => source()
%%    }.

-spec temporary_file() -> file:filename().
temporary_file() ->
  CacheDir = filename:basedir(user_cache, "erlang_ls"),
  [Unique] = io_lib:format("~p", [erlang:phash2(erlang:timestamp())]),
  filename:join([CacheDir, "diagnostics", Unique ++ ".diags"]).
