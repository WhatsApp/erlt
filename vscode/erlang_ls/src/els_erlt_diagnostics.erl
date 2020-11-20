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
  R = case CompileCmd of
    "node" ->
      lager:info("ErlT: compiling via rpc to 'rebar3 shell' node"),
      try
        erpc:call('erltc@localhost', os, set_env_var,
                  ["ERLT_LANGUAGE_SERVER", TempFile]),
        erpc:call('erltc@localhost', rebar3, run, [["compile"]]),
        get_els_file(TempFile, Uri)
      catch
        C:E:_ -> {{rpcerror, {C, E}}, []}
      end;
    _ ->
      {Cmd, Diags0} =
            case net_adm:ping('sterlangd@localhost') of
              pong ->
                lager:info("ErlT: compiling using sterlangd via command [~s]",
                           [CompileCmd]),
                {lists:flatten(io_lib:format(
                                "ERL_FLAGS='-args_file dev.vm.args' "
                                "ERLT_LANGUAGE_SERVER=~s ~s",
                                [TempFile, CompileCmd]))
                 , []};
              _ ->
                lager:info("ErlT: compiling  WITHOUT sterlngd via command [~s]",
                           [CompileCmd]),
                Desc0 = lists:flatten(io_lib:format(
                          "ErlT: compiling  WITHOUT sterlngd via command [~s]",
                                        [CompileCmd])),
                {lists:flatten(io_lib:format(
                                "ERLT_LANGUAGE_SERVER=~s ~s",
                                [TempFile, CompileCmd]))
                , make_error_diag(Desc0)}
            end,
      os:cmd(Cmd),
      {get_els_file(TempFile, Uri), Diags0}
  end,
  case R of
    {{ok, FileName}, Ds} ->
      get_els_file(TempFile, Uri),
      {ok, [{_TS, Diags, Hovers, Lenses}]} = file:consult(FileName),
      store_pois(Uri, Hovers ++ Lenses),
      Diags ++ Ds;
    {{rpcerror, Reason}, Ds} ->
      Desc = lists:flatten(
        io_lib:format(
          "Have you launched the rebar shell?~n"
          "els_erlt_diagnostics: could not contact rebar node [~p]",
          [Reason])),
      make_error_diag(Desc) ++ Ds;
    {{error, Reason}, Ds} ->
      Desc = lists:flatten(
        io_lib:format(
          "Have you set the correct 'erlt_command' in erlang_ls.config?~n"
          "Current value from config is [~s]~n"
          "els_erlt_diagnostics: could not read temp file [~p]",
          [CompileCmd, Reason])),
      make_error_diag(Desc) ++ Ds
  end.

-spec make_error_diag(string()) -> [els_diagnostics:diagnostic()].
make_error_diag(Desc) ->
  Range = #{ from => {1, 1}, to => {2, 1} },
  Diag =
    #{ range    => els_protocol:range(Range)
     , message  => els_utils:to_binary(Desc)
     , severity => ?DIAGNOSTIC_ERROR
     , source   => <<"ErlT">>
     },
  [Diag].

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
