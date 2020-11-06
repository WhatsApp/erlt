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

-spec source() -> binary().
source() ->
  <<"ErlT">>.

-spec compile(uri()) -> [els_diagnostics:diagnostic()].
compile(_Uri) ->
  %% Path = els_utils:to_list(els_uri:path(Uri)),
  %% WS = [],
  [diagnostic(#{ from => {1, 1}, to => {2, 1} }
             , blah, "Test warning", ?DIAGNOSTIC_WARNING)].

%%==============================================================================
%% Private Functions
%%==============================================================================

-spec diagnostic(poi_range(), module(), string(), integer()) ->
        els_diagnostics:diagnostic().
diagnostic(Range, _Module, Desc, Severity) ->
  %% Message0 = lists:flatten(Module:format_error(Desc)),
  Message0 = Desc,
  Message  = els_utils:to_binary(Message0),
  #{ range    => els_protocol:range(Range)
   , message  => Message
   , severity => Severity
   , source   => source()
   }.
