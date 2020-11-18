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
  CompileCmd = els_config:get(erlt_command),
  Cmd = lists:flatten(io_lib:format("ERLT_LANGUAGE_SERVER=~s ~s",
                      [TempFile, CompileCmd])),
  os:cmd(Cmd),
  case get_els_file(TempFile, Uri) of
    {ok, FileName} ->
      get_els_file(TempFile, Uri),
      {ok, [{_TS, Diags, Hovers, Lenses}]} = file:consult(FileName),
      store_pois(Uri, Hovers++Lenses),
      Diags;
    {error, Reason} ->
      Range = #{ from => {1, 1}, to => {2, 1} },
      Desc = lists:flatten(
        io_lib:format(
          "Have you set the correct 'erlt_command' in erlang_ls.config?~n"
          "els_erlt_diagnostics: could not read temp file [~p]",
          [Reason])),
      Diag =
        #{ range    => els_protocol:range(Range)
         , message  => els_utils:to_binary(Desc)
         , severity => ?DIAGNOSTIC_ERROR
         , source   => <<"ErlT">>
         },
      [Diag]
  end.

%%==============================================================================
%% Private Functions
%%==============================================================================

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

-spec temporary_file() -> file:filename().
temporary_file() ->
  CacheDir = filename:basedir(user_cache, "erlang_ls"),
  [Unique] = io_lib:format("~p", [erlang:phash2(erlang:timestamp())]),
  filename:join([CacheDir, "diagnostics", Unique ++ ".diags"]).

-spec store_pois(uri(), [poi()]) -> ok.
store_pois(Uri, Hovers) ->
  {ok, Document} = els_utils:lookup_document(Uri),
  %% TODO: probably need an intelligent merge of pois here
  F = fun() ->
          els_dt_document:insert(Document#{pois => Hovers})
      end,
  els_db:transaction(F),
  ok.
