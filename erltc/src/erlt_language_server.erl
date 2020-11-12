%% Copyright (c) 2020 Facebook, Inc. and its affiliates.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%%-------------------------------------------------------------------
%%% @doc
%%% Provide the interface to share information with the language server
%%% @end
%%%-------------------------------------------------------------------

-module(erlt_language_server).

-include("erlt_common.hrl").
-include("erlt_compile.hrl").

-export([
         maybe_output_ls_diagnostics/3
]).

%% ---------------------------------------------------------------------
%% Copied directly from erlang_ls els_diagnostics, for now

-type uri()       :: binary().
-type line()     :: number().
-type column()   :: number().
-type position() :: #{ line      := line()
                     , character := column()
                     }.

-type range() :: #{ start := position()
                  , 'end' := position()
                  }.

-type location() :: #{ uri   := uri()
                     , range := range()
                     }.

-type diagnostic() :: #{ range              := range()
                       , severity           => severity()
                       , code               => number() | binary()
                       , source             => binary()
                       , message            := binary()
                       , relatedInformation => [related_info()]
                       }.

-type diagnostic_id() :: binary().
-type related_info() :: #{ location := location()
                         , message  := binary()
                         }.

-define(DIAGNOSTIC_ERROR   , 1).
-define(DIAGNOSTIC_WARNING , 2).
-define(DIAGNOSTIC_INFO    , 3).
-define(DIAGNOSTIC_HINT    , 4).

-type severity() :: ?DIAGNOSTIC_ERROR
                  | ?DIAGNOSTIC_WARNING
                  | ?DIAGNOSTIC_INFO
                  | ?DIAGNOSTIC_HINT.

-export_type([ diagnostic/0
             , diagnostic_id/0
             , severity/0
             ]).

%% ---------------------------------------------------------------------

-define(LsFileSuffix, ".els").

-spec source() -> binary().
source() ->
  <<"ErlT">>.

maybe_output_ls_diagnostics(Warn, Err, St) ->
    case os:getenv("ERLT_LANGUAGE_SERVER") of
        false -> ok;
        OutFile ->
            output_ls_diagnostics(OutFile, Warn, Err, St)
end.

output_ls_diagnostics(OutFile, Warn, Err
                     , #compile{build_dir = BuildDir, base = Base,
                                ifile = SourceFile} = St) ->
    %% Output = term_to_binary(normalize_for_typecheck(Code)),

    %% We include a timestamp, so that the language server can check
    %% if the diagnostics are fresh or not. This will be used for
    %% equality test only, to invalidate the cache.  Possibly ignore
    %% older files in future?
    TS = erlang:timestamp(),
    Diags = convert_to_diagnostics(Warn, Err, St),
    Output = io_lib:format("~p.", [{TS, Diags}]),
    FileName = filename:join(
            BuildDir,
            Base ++ ?LsFileSuffix),
    case FileName of
      undefined -> error("undefined FileName");
      _ ->
            io:format("###LANGSERVER: ~0p~n", [{OutFile,filename:absname(SourceFile), FileName}]),
            Mapping = io_lib:format("~0p.~n", [{filename:absname(SourceFile), {TS, FileName}}]),
            file:write_file(OutFile, Mapping, [append]),
            file:write_file(FileName, Output, [sync])
    end,
    ok.

convert_to_diagnostics(Warn, Err, St) ->
    report_errors(Err, St) ++
      report_warnings(Warn, St).

%% ---------------------------------------------------------------------

report_errors(Errors, #compile{options = Opts}) ->
    case lists:member(report_errors, Opts) of
        true ->
            lists:map(
                fun
                    ({{F, _L}, Eds}) -> list_errors(F, Eds, Opts);
                    ({F, Eds}) -> list_errors(F, Eds, Opts)
                end,
                Errors
            );
        false ->
            []
    end.

%% ---------------------------------------------------------------------

report_warnings(Ws0, #compile{options = Opts}) ->
    Werror = lists:member(warnings_as_errors, Opts),
    P =
        case Werror of
            true -> "";
            false -> "Warning: "
        end,
    ReportWerror = Werror andalso lists:member(report_errors, Opts),
    case lists:member(report_warnings, Opts) orelse ReportWerror of
        %% true ->
        _ ->
            Ws1 = lists:flatmap(
                fun
                    ({{F, _L}, Eds}) -> format_message(F, P, Eds, Opts);
                    ({F, Eds}) -> format_message(F, P, Eds, Opts)
                end,
                Ws0
            ),
            Ws = lists:sort(Ws1),
            %% lists:foreach(fun({_, Str}) -> io:put_chars(Str) end, Ws);
            [ Diag || {_,Diag} <- Ws];
            %% Ws0;
        false ->
            []
    end.

%% ---------------------------------------------------------------------

%% NOTE: based on erlt_messages:list_errors/3
%% list_errors(File, ErrorDescriptors, Opts) -> [diagnostic()]
list_errors(F, [{none, Mod, E} | Es], Opts) ->
    Range = 1, %% Just put it at the top of the file, no other info
    Diag = diagnostic(Range, Mod, E, ?DIAGNOSTIC_ERROR),
    [Diag | list_errors(F, Es, Opts)];
list_errors(F, [{{{_, _} = StartLoc, {_, _} = EndLoc}, Mod, E} | Es], Opts) ->
    %% this is the location format used in the type analysis pass
    %% Src = quote_source(F, StartLoc, EndLoc, Opts),
    Range = {StartLoc, EndLoc},
    Diag = diagnostic(Range, Mod, E, ?DIAGNOSTIC_ERROR),
    [Diag | list_errors(F, Es, Opts)];
list_errors(F, [{Loc, Mod, E} | Es], Opts) ->
    StartLoc = erl_anno:location(Loc),
    EndLoc =
        case erlt_parse:get_end_location(Loc) of
            undefined -> StartLoc;
            Loc2 -> Loc2
        end,
    list_errors(F, [{{StartLoc, EndLoc}, Mod, E} | Es], Opts);
list_errors(_F, [], _Opts) ->
    [].

%% ---------------------------------------------------------------------

%% NOTE: based on erlt_messages:format_message/4
format_message(F, P, [{none, Mod, E} | Es], Opts) ->
    %% M = {none, io_lib:format("~ts: ~s~ts\n", [F, P, Mod:format_error(E)])},
    Range = 1, %% Just put it at the top of the file, no other info
    Diag = diagnostic(Range, Mod, E, ?DIAGNOSTIC_WARNING),
    [Diag | format_message(F, P, Es, Opts)];
format_message(F, P, [{Loc, Mod, E} | Es], Opts) ->
    StartLoc = erl_anno:location(Loc),
    EndLoc =
        case erlt_parse:get_end_location(Loc) of
            undefined -> StartLoc;
            Loc2 -> Loc2
        end,

    Range = {StartLoc, EndLoc},
    Diag = diagnostic(Range, Mod, E, ?DIAGNOSTIC_WARNING),
    Pos =
        if
            is_integer(StartLoc) -> {StartLoc, 0};
            true -> StartLoc
        end,
    [{{F, Pos}, Diag} | format_message(F, P, Es, Opts)];
format_message(_, _, [], _Opts) ->
    [].

%% -spec diagnostic(poi_range(), module(), string(), integer()) ->
%%         els_diagnostics:diagnostic().
diagnostic(Range, Module, Desc, Severity) ->
  Message0 = lists:flatten(Module:format_error(Desc)),
  Message  = to_binary(Message0),
  #{ range    => lsp_range(range(Range))
   , message  => Message
   , severity => Severity
   , source   => source()
   }.

%% -spec range(erl_anno:line() | none) -> poi_range().
range(Line) when is_integer(Line) ->
  #{from => {Line, 1}, to => {Line + 1, 1}};
range({{LineF,ColF},{LineT,ColT}}) ->
  #{from => {LineF, ColF}, to => {LineT, ColT}};
range({Line, Col}) when is_integer(Line) andalso is_integer(Col) ->
  #{from => {Line, Col}, to => {Line + 1, 1}};
range([{location,{LineF,ColF}},{end_location,{LineT,ColT}}]) ->
  #{from => {LineF, ColF}, to => {LineT, ColT}};
range(none) ->
  range(1).

%% -spec range(poi_range()) -> range().
lsp_range(#{ from := {FromL, FromC}, to := {ToL, ToC} }) ->
  #{ start => #{line => FromL - 1, character => FromC - 1}
   , 'end' => #{line => ToL - 1,   character => ToC - 1}
   }.

%% ---------------------------------------------------------------------
%% Copied from els_utils

-spec to_binary(unicode:chardata()) -> binary().
to_binary(X) when is_binary(X) ->
  X;
to_binary(X) when is_list(X) ->
  case unicode:characters_to_binary(X) of
    Result when is_binary(Result) -> Result;
    _ -> iolist_to_binary(X)
  end.

%% -spec to_list(unicode:chardata()) -> string().
%% to_list(X) when is_list(X) ->
%%   X;
%% to_list(X) when is_binary(X) ->
%%   case unicode:characters_to_list(X) of
%%     Result when is_list(Result) -> Result;
%%     _ -> binary_to_list(X)
%%   end.

%% ---------------------------------------------------------------------
