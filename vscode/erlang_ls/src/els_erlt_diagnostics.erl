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

-spec compile(uri()) -> [els_diagnostics:diagnostic()].
compile(Uri) ->
  TempFile = temporary_file(),
  filelib:ensure_dir(TempFile),
  %% Cmd should come from config, see #375
  Cmd = lists:flatten(io_lib:format("ERLT_LANGUAGE_SERVER=~s rebar3 compile",
  %% Cmd = lists:flatten(io_lib:format("ERLT_LANGUAGE_SERVER=~s make test-others",
                      [TempFile])),
  os:cmd(Cmd),
  case get_els_file(TempFile, Uri) of
    {ok, FileName} ->
      get_els_file(TempFile, Uri),
      {ok, [{_TS, Diags}]} = file:consult(FileName),
      Diags;
    {error, Reason} ->
      Range = #{ from => {1, 1}, to => {2, 1} },
      Desc = lists:flatten(
        io_lib:format("els_erlt_diagnostics: could not read temp file [~p]",
                           [Reason])),
      Diag =
        #{ range    => els_protocol:range(Range)
         , message  => els_utils:to_binary(Desc)
         , severity => ?DIAGNOSTIC_ERROR
         , source   => <<"ErlT">>
         },
      [Diag]
  end.


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

-spec temporary_file() -> file:filename().
temporary_file() ->
  CacheDir = filename:basedir(user_cache, "erlang_ls"),
  [Unique] = io_lib:format("~p", [erlang:phash2(erlang:timestamp())]),
  filename:join([CacheDir, "diagnostics", Unique ++ ".diags"]).
