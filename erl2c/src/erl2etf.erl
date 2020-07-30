%% Copyright (c) Facebook, Inc. and its affiliates.
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
-module(erl2etf).

-export([main/1]).

main(["-erl", Filename, "-etf", EtfFileName]) ->
    {ok, Forms} = erl2_epp:parse_file(Filename, [{location, {1, 1}}, {scan_opts, [text]}]),
    Lang = parse_lang(Forms),
    Ffi = lists:member(ffi, Lang),
    Forms1 = normalize_for_typecheck(Forms, Ffi),
    CodeETF = erlang:term_to_binary(Forms1),
    ok = filelib:ensure_dir(EtfFileName),
    ok = file:write_file(EtfFileName, CodeETF);
main(["-ast1", File]) ->
    {ok, Forms} = epp:parse_file(File, []),
    io:format("Forms:\n~p\n", [Forms]);
main(["-ast2", Filename]) ->
    {ok, Forms} = erl2_epp:parse_file(Filename, [{location, {1, 1}}, {scan_opts, [text]}]),
    Lang = parse_lang(Forms),
    Ffi = lists:member(ffi, Lang),
    Forms1 = normalize_for_typecheck(Forms, Ffi),
    io:format("Forms:\n~p\n", [Forms1]).

parse_lang(Forms) ->
    lists:nth(1, [Lang || {attribute, _, lang, Lang} <- Forms]).

%% Turn annotation fields into a uniform format for export to the type checker
normalize_for_typecheck(Forms, Ffi) ->
    Forms1 =
        case Ffi of
            false -> Forms;
            true -> [F || F <- Forms, not is_fun_form(F)]
        end,
    [erl2_parse:map_anno(fun normalize_loc/1, F) || F <- Forms1].

%% returns {{StartLine,StartColumn},{EndLine,EndColumn}}
normalize_loc(Line) when is_integer(Line) ->
    % only start line known
    {{Line, 0}, {Line, 0}};
normalize_loc({Line, Col} = Loc) when is_integer(Line), is_integer(Col) ->
    % only start position known
    {Loc, Loc};
normalize_loc(As) when is_list(As) ->
    Start = loc(erl_anno:location(As)),
    End =
        case erl2_parse:get_end_location(As) of
            undefined -> Start;
            Loc -> loc(Loc)
        end,
    case lists:member(open_rec, As) of
        true -> {Start, End, open_rec};
        false -> {Start, End}
    end;
normalize_loc(_Other) ->
    % unknown position
    {{0, 0}, {0, 0}}.

loc({Line, Col} = Loc) when is_integer(Line), is_integer(Col) ->
    Loc;
loc(Line) when is_integer(Line) ->
    {Line, 0};
loc(_Other) ->
    {0, 0}.

is_fun_form({function, _, _, _, _}) -> true;
is_fun_form(_) -> false.
