%%==============================================================================
%% Code Lens: ErlT info
%%==============================================================================

-module(els_code_lens_erlt).

-behaviour(els_code_lens).
-export([ command/1
        , command_args/2
        , is_default/0
        , pois/1
        , precondition/1
        , title/1
        ]).

-include("erlang_ls.hrl").

-spec command(poi()) -> els_command:command_id().
command(_POI) ->
  <<"replace-lines">>.

-spec command_args(els_dt_document:item(), poi()) -> [any()].
command_args(#{ uri := Uri }, #{ data := Spec, range := Range } = _POI) ->
  #{ from := {StartLine, _}} = Range,
  Line = StartLine - 1,
  [#{ uri   => Uri
    , lines => [<<Spec/binary, <<"\n">>/binary>>]
    , from  => Line
    , to    => Line }].

-spec is_default() -> boolean().
is_default() ->
  true.

-spec precondition(els_dt_document:item()) -> boolean().
precondition(_Document) ->
  true.

-spec pois(els_dt_document:item()) -> [poi()].
pois(Document) ->
  POIs = els_dt_document:pois(Document, [lens]),
  POIs.

-spec title(poi()) -> binary().
title(#{ data := Spec} = _POI) ->
  Spec.
