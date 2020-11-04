-module(els_completion_provider).

-behaviour(els_provider).

-include("erlang_ls.hrl").

-export([ handle_request/2
        , is_enabled/0
        ]).

%% Exported to ease testing.
-export([ bifs/2
        , keywords/0
        ]).

-type options() :: #{ trigger  := binary()
                    , document := els_dt_document:item()
                    , line     := line()
                    , column   := column()
                    }.
-type state() :: any().

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec handle_request(els_provider:request(), state()) -> {any(), state()}.
handle_request({completion, Params}, State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  {ok, #{text := Text} = Document} = els_utils:lookup_document(Uri),
  Context = maps:get( <<"context">>
                    , Params
                    , #{ <<"triggerKind">> => ?COMPLETION_TRIGGER_KIND_INVOKED }
                    ),
  TriggerKind = maps:get(<<"triggerKind">>, Context),
  TriggerCharacter = maps:get(<<"triggerCharacter">>, Context, <<>>),
  %% We subtract 1 to strip the character that triggered the
  %% completion from the string.
  Length = case Character > 0 of true -> 1; false -> 0 end,
  Prefix = case TriggerKind of
             ?COMPLETION_TRIGGER_KIND_CHARACTER ->
               els_text:line(Text, Line, Character - Length);
             ?COMPLETION_TRIGGER_KIND_INVOKED ->
               els_text:line(Text, Line, Character);
             ?COMPLETION_TRIGGER_KIND_FOR_INCOMPLETE_COMPLETIONS ->
               els_text:line(Text, Line, Character)
           end,
  Opts   = #{ trigger  => TriggerCharacter
              , document => Document
              , line     => Line + 1
              , column   => Character
            },
  {find_completion(Prefix, TriggerKind, Opts), State}.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec find_completion(binary(), integer(), options()) -> any().
find_completion( Prefix
               , ?COMPLETION_TRIGGER_KIND_CHARACTER
               , #{ trigger  := <<":">>
                  , document := Document
                  , line     := Line
                  , column   := Column
                  }
               ) ->
  case els_text:last_token(Prefix) of
    {atom, _, Module} ->
      {ExportFormat, TypeOrFun} = completion_context(Document, Line, Column),
      exported_definitions(Module, TypeOrFun, ExportFormat);
    _ ->
      null
  end;
find_completion( _Prefix
               , ?COMPLETION_TRIGGER_KIND_CHARACTER
               , #{trigger := <<"?">>, document := Document}
               ) ->
  definitions(Document, define);
find_completion( _Prefix
               , ?COMPLETION_TRIGGER_KIND_CHARACTER
               , #{trigger := <<"#">>, document := Document}
               ) ->
  definitions(Document, record);
find_completion( Prefix
               , ?COMPLETION_TRIGGER_KIND_CHARACTER
               , #{trigger := <<".">>, document := Document}
               ) ->
  case lists:reverse(els_text:tokens(Prefix)) of
    [{atom, _, RecordName}, {'#', _} | _] ->
      record_fields(Document, RecordName);
    _ ->
      []
    end;
find_completion( Prefix
               , ?COMPLETION_TRIGGER_KIND_INVOKED
               , #{ document := Document
                  , line     := Line
                  , column   := Column
                  }
               ) ->
  case lists:reverse(els_text:tokens(Prefix)) of
    %% Check for "[...] fun atom:"
    [{':', _}, {atom, _, Module}, {'fun', _} | _] ->
      exported_definitions(Module, function, _ExportFormat = true);
    %% Check for "[...] fun atom:atom"
    [{atom, _, _}, {':', _}, {atom, _, Module}, {'fun', _} | _] ->
      exported_definitions(Module, function, _ExportFormat = true);
    %% Check for "[...] atom:"
    [{':', _}, {atom, _, Module} | _] ->
      {ExportFormat, TypeOrFun} = completion_context(Document, Line, Column),
      exported_definitions(Module, TypeOrFun, ExportFormat);
    %% Check for "[...] atom:atom"
    [{atom, _, _}, {':', _}, {atom, _, Module} | _] ->
      {ExportFormat, TypeOrFun} = completion_context(Document, Line, Column),
      exported_definitions(Module, TypeOrFun, ExportFormat);
    %% Check for "[...] ?"
    [{'?', _} | _] ->
      definitions(Document, define);
    %% Check for "[...] ?anything"
    [_, {'?', _} | _] ->
      definitions(Document, define);
    %% Check for "[...] #anything."
    [{'.', _}, {atom, _, RecordName}, {'#', _} | _] ->
      record_fields(Document, RecordName);
    %% Check for "[...] #anything.something"
    [_, {'.', _}, {atom, _, RecordName}, {'#', _} | _] ->
      record_fields(Document, RecordName);
    %% Check for "[...] #"
    [{'#', _} | _] ->
      definitions(Document, record);
    %% Check for "[...] #anything"
    [_, {'#', _} | _] ->
      definitions(Document, record);
    %% Check for "[...] Variable"
    [{var, _, _} | _] ->
      variables(Document);
    %% Check for "[...] fun atom"
    [{atom, _, _}, {'fun', _} | _] ->
      bifs(function, _ExportFormat = true)
        ++ definitions(Document, function, _ExportFormat = true);
    %% Check for "[...] atom"
    [{atom, _, Name} | _] ->
      NameBinary = atom_to_binary(Name, utf8),
      {ExportFormat, POIKind} = completion_context(Document, Line, Column),
      keywords()
        ++ bifs(POIKind, ExportFormat)
        ++ atoms(Document, NameBinary)
        ++ modules(NameBinary)
        ++ definitions(Document, POIKind, ExportFormat);
    _ ->
      []
  end;
find_completion(_Prefix, _TriggerKind, _Opts) ->
  null.

%%==============================================================================
%% Atoms
%%==============================================================================

-spec atoms(els_dt_document:item(), binary()) -> [map()].
atoms(Document, Prefix) ->
  POIs   = local_and_included_pois(Document, atom),
  Atoms  = [Id || #{id := Id} <- POIs],
  Unique = lists:usort(Atoms),
  filter_by_prefix(Prefix, Unique, fun to_binary/1, fun item_kind_atom/1).

-spec item_kind_atom(binary()) -> map().
item_kind_atom(Module) ->
  #{ label            => Module
   , kind             => ?COMPLETION_ITEM_KIND_CONSTANT
   , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
   }.

%%==============================================================================
%% Modules
%%==============================================================================

-spec modules(binary()) -> [map()].
modules(Prefix) ->
  {ok, Items} = els_dt_document_index:find_by_kind(module),
  Modules = [Id || #{id := Id} <- Items],
  filter_by_prefix(Prefix, Modules, fun to_binary/1, fun item_kind_module/1).

-spec item_kind_module(binary()) -> map().
item_kind_module(Module) ->
  #{ label            => Module
   , kind             => ?COMPLETION_ITEM_KIND_MODULE
   , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
   }.

%%==============================================================================
%% Functions, Types, Macros and Records
%%==============================================================================

-spec definitions(els_dt_document:item(), poi_kind()) -> [map()].
definitions(Document, POIKind) ->
  definitions(Document, POIKind, _ExportFormat = false, _ExportedOnly = false).

-spec definitions(els_dt_document:item(), poi_kind(), boolean()) -> [map()].
definitions(Document, POIKind, ExportFormat) ->
  definitions(Document, POIKind, ExportFormat, _ExportedOnly = false).

-spec definitions(els_dt_document:item(), poi_kind(), boolean(), boolean()) ->
  [map()].
definitions(Document, POIKind, ExportFormat, ExportedOnly) ->
  POIs     = local_and_included_pois(Document, POIKind),

  %% Find exported entries when there is an export_entry kind available
  FAs      = case export_entry_kind(POIKind) of
               {error, no_export_entry_kind} -> [];
               ExportKind ->
                 Exports = local_and_included_pois(Document, ExportKind),
                 [FA || #{id := FA} <- Exports]
             end,

  Items = resolve_definitions(POIs, FAs, ExportedOnly, ExportFormat),
  lists:usort(Items).

-spec completion_context(els_dt_document:item(), line(), column()) ->
  {boolean(), poi_kind()}.
completion_context(Document, Line, Column) ->
  ExportFormat = is_in(Document, Line, Column, [export, export_type]),
  POIKind      = case is_in(Document, Line, Column, [spec, export_type]) of
                   true -> type_definition;
                   false -> function
                 end,
  {ExportFormat, POIKind}.

-spec resolve_definitions([poi()], [{atom(), arity()}], boolean(), boolean()) ->
  [map()].
resolve_definitions(Functions, ExportsFA, ExportedOnly, ArityOnly) ->
  [ completion_item(POI, ArityOnly)
    || #{id := FA} = POI <- Functions,
        not ExportedOnly orelse lists:member(FA, ExportsFA)
  ].

-spec exported_definitions(module(), poi_kind(), boolean()) -> [map()].
exported_definitions(Module, POIKind, ExportFormat) ->
  case els_utils:find_module(Module) of
    {ok, Uri} ->
      {ok, Document} = els_utils:lookup_document(Uri),
      definitions(Document, POIKind, ExportFormat, true);
    {error, _Error} ->
      []
  end.

%%==============================================================================
%% Variables
%%==============================================================================

-spec variables(els_dt_document:item()) -> [map()].
variables(Document) ->
  POIs = els_dt_document:pois(Document, [variable]),
  Vars = [ #{ label => atom_to_binary(Name, utf8)
            , kind  => ?COMPLETION_ITEM_KIND_VARIABLE
            }
           || #{id := Name} <- POIs
         ],
  lists:usort(Vars).

%%==============================================================================
%%  Record Fields
%%==============================================================================

-spec record_fields(els_dt_document:item(), atom()) -> [map()].
record_fields(Document, RecordName) ->
  case find_record_definition(Document, RecordName) of
    [] -> [];
    POIs ->
      [#{data := Fields} | _] = els_poi:sort(POIs),
      [ #{ label => atom_to_binary(Name, utf8)
         , kind  => ?COMPLETION_ITEM_KIND_FIELD
         }
        || {Name, _} <- Fields
      ]
  end.

-spec find_record_definition(els_dt_document:item(), atom()) -> [poi()].
find_record_definition(Document, RecordName) ->
  POIs = local_and_included_pois(Document, record),
  [X || X = #{id := Name} <- POIs, Name =:= RecordName].

%%==============================================================================
%% Keywords
%%==============================================================================

-spec keywords() -> [map()].
keywords() ->
  Keywords = [ 'after', 'and', 'andalso', 'band', 'begin', 'bnot', 'bor', 'bsl'
             , 'bsr', 'bxor', 'case', 'catch', 'cond', 'div', 'end', 'fun'
             , 'if', 'let', 'not', 'of', 'or', 'orelse', 'receive', 'rem'
             , 'try', 'when', 'xor'],

  [ #{ label => atom_to_binary(K, utf8)
     , kind  => ?COMPLETION_ITEM_KIND_KEYWORD
     } || K <- Keywords ].

%%==============================================================================
%% Built-in functions
%%==============================================================================

-spec bifs(poi_kind(), boolean()) -> [map()].
bifs(function, ExportFormat) ->
  Range = #{from => {0, 0}, to => {0, 0}},
  Exports = erlang:module_info(exports),
  BIFs = [ #{ kind  => function
            , id    => X
            , range => Range
            , data  => generate_arguments("Arg", A)
            }
           || {F, A} = X <- Exports, erl_internal:bif(F, A)
         ],
  [completion_item(X, ExportFormat) || X <- BIFs];
bifs(type_definition, true = _ExportFormat) ->
  %% We don't want to include the built-in types when we are in
  %% a -export_types(). context.
  [];
bifs(type_definition, false = ExportFormat) ->
  Types = [ {'any', 0}, {'arity', 0}, {'atom', 0}, {'binary', 0}
          , {'bitstring', 0}, {'boolean', 0}, {'byte', 0}, {'char', 0}
          , {'float', 0}, {'fun', 0}, {'fun', 1}, {'function', 0}
          , {'identifier', 0}, {'integer', 0}, {'iodata', 0}, {'iolist', 0}
          , {'list', 0}, {'list', 1}, {'map', 0}, {'maybe_improper_list', 0}
          , {'maybe_improper_list', 2}, {'mfa', 0}, {'module', 0}
          , {'neg_integer', 0}, {'nil', 0}, {'no_return', 0}, {'node', 0}
          , {'nonempty_improper_list', 2}, {'nonempty_list', 1}
          , {'non_neg_integer', 0}, {'none', 0}, {'nonempty_list', 0}
          , {'nonempty_string', 0}, {'number', 0}, {'pid', 0}, {'port', 0}
          , {'pos_integer', 0}, {'reference', 0}, {'string', 0}, {'term', 0}
          , {'timeout', 0}
          ],
  Range = #{from => {0, 0}, to => {0, 0}},
  POIs = [ #{ kind  => type_definition
            , id    => X
            , range => Range
            , data  => generate_arguments("Type", A)
            }
           || {_, A} = X <- Types
         ],
  [completion_item(X, ExportFormat) || X <- POIs].

-spec generate_arguments(string(), integer()) -> [{integer(), string()}].
generate_arguments(Prefix, Arity) ->
  [{N, Prefix ++ integer_to_list(N)} || N <- lists:seq(1, Arity)].

%%==============================================================================
%% Filter by prefix
%%==============================================================================

%% TODO: Implement as select
-spec filter_by_prefix(binary(), [binary()], function(), function()) -> [map()].
filter_by_prefix(Prefix, List, ToBinary, ItemFun) ->
  FilterMapFun = fun(X) ->
                     Str = ToBinary(X),
                     case string:prefix(Str, Prefix)  of
                       nomatch -> false;
                       _       -> {true, ItemFun(Str)}
                     end
                 end,
  lists:filtermap(FilterMapFun, List).

-spec to_binary(any()) -> binary().
to_binary(X) when is_atom(X) ->
  atom_to_binary(X, utf8);
to_binary(X) when is_binary(X) ->
  X.

%%==============================================================================
%% Helper functions
%%==============================================================================

-spec completion_item(poi(), ExportFormat :: boolean()) -> map().
completion_item(#{kind := Kind, id := {F, A}, data := ArgsNames}, false)
  when Kind =:= function;
       Kind =:= type_definition ->
  Label = io_lib:format("~p/~p", [F, A]),
  #{ label            => els_utils:to_binary(Label)
   , kind             => completion_item_kind(Kind)
   , insertText       => snippet_function_call(F, ArgsNames)
   , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
   };
completion_item(#{kind := Kind, id := {F, A}}, true)
  when Kind =:= function;
       Kind =:= type_definition ->
  Label = io_lib:format("~p/~p", [F, A]),
  #{ label            => els_utils:to_binary(Label)
   , kind             => completion_item_kind(Kind)
   , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
   };
completion_item(#{kind := Kind, id := Name}, _)
  when Kind =:= record;
       Kind =:= define ->
  #{ label            => atom_to_binary(Name, utf8)
   , kind             => completion_item_kind(Kind)
   }.

-spec snippet_function_call(atom(), [{integer(), string()}]) -> binary().
snippet_function_call(Function, Args0) ->
  Args    = [ ["${", integer_to_list(N), ":", A, "}"]
              || {N, A} <- Args0
            ],
  Snippet = [atom_to_list(Function), "(", string:join(Args, ", "), ")"],
  els_utils:to_binary(Snippet).

-spec is_in(els_dt_document:item(), line(), column(), [poi_kind()]) ->
  boolean().
is_in(Document, Line, Column, POIKinds) ->
  POIs = els_dt_document:get_element_at_pos(Document, Line, Column),
  IsKind = fun(#{kind := Kind}) -> lists:member(Kind, POIKinds) end,
  lists:any(IsKind, POIs).

%% @doc Maps a POI kind to its completion item kind
-spec completion_item_kind(poi_kind()) -> completion_item_kind().
completion_item_kind(define) ->
  ?COMPLETION_ITEM_KIND_CONSTANT;
completion_item_kind(record) ->
  ?COMPLETION_ITEM_KIND_STRUCT;
completion_item_kind(type_definition) ->
  ?COMPLETION_ITEM_KIND_TYPE_PARAM;
completion_item_kind(function) ->
  ?COMPLETION_ITEM_KIND_FUNCTION.

%% @doc Maps a POI kind to its export entry POI kind
-spec export_entry_kind(poi_kind()) ->
  poi_kind() | {error, no_export_entry_kind}.
export_entry_kind(type_definition) -> export_type_entry;
export_entry_kind(function) -> export_entry;
export_entry_kind(_) -> {error, no_export_entry_kind}.

%% @doc Returns POIs of the provided `Kind' in the document and included files
-spec local_and_included_pois(els_dt_document:item(), poi_kind()) -> [poi()].
local_and_included_pois(Document, Kind) ->
  lists:flatten([ els_dt_document:pois(Document, [Kind])
                , included_pois(Document, Kind)
                ]).

%% @doc Returns POIs of the provided `Kind' in included files from `Document'
-spec included_pois(els_dt_document:item(), poi_kind()) -> [[map()]].
included_pois(Document, Type) ->
  POIs  = els_dt_document:pois(Document, [include, include_lib]),
  [include_file_pois(Name, Type) || #{id := Name} <- POIs].

%% @doc Returns POIs of the provided `Kind' in the included file
-spec include_file_pois(string(), poi_kind()) -> [map()].
include_file_pois(Name, Kind) ->
  Filename = filename:basename(Name, filename:extension(Name)),
  H = list_to_atom(Filename),
  case els_utils:find_header(H) of
    {ok, Uri} ->
      {ok, IncludeDocument} = els_utils:lookup_document(Uri),
      els_dt_document:pois(IncludeDocument, [Kind]);
    {error, _} ->
      []
  end.
