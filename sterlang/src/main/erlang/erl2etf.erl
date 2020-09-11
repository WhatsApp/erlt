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

main(["-ifile", IFile, "-ofile", OFile]) ->
    Forms = parse_file(IFile),
    Lang = parse_lang(Forms),
    Ffi = lists:member(ffi, Lang),
    Forms1 = normalize_for_typecheck(Forms, Ffi),
    CodeETF = erlang:term_to_binary(Forms1),
    ok = filelib:ensure_dir(OFile),
    ok = file:write_file(OFile, CodeETF);
main(["-ast", Filename]) ->
    Forms = parse_file(Filename),
    Lang = parse_lang(Forms),
    Ffi = lists:member(ffi, Lang),
    Forms1 = normalize_for_typecheck(Forms, Ffi),
    io:format("Forms:\n~p\n", [Forms1]);
main(["-idir", IDir, "-odir", ODir]) ->
    {ok, Files} = file:list_dir(IDir),
    SortedFiles = lists:sort(Files),
    ErlFiles = lists:filter(
        fun(Name) -> filename:extension(Name) == ".erl" end,
        SortedFiles
    ),
    lists:foreach(
        fun(ErlFile) ->
            EtfFile = filename:basename(ErlFile, ".erl") ++ ".etf",
            IFile = filename:join(IDir, ErlFile),
            OFile = filename:join(ODir, EtfFile),
            ok = main(["-ifile", IFile, "-ofile", OFile])
        end,
        ErlFiles
    ),
    ok.

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
normalize_loc(As) when is_list(As) ->
    Start = erl2_parse:location(As),
    End = erl2_parse:get_end_location(As),
    {Start, End}.

is_fun_form({function, _, _, _, _}) -> true;
is_fun_form(_) -> false.

parse_file(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Chars = unicode:characters_to_list(Data, utf8),
    parse_chars(Chars, {1, 1}).

parse_chars(Chars, Location) ->
    case erl_scan:tokens([], Chars, Location, ['text']) of
        {done, Result, Chars1} ->
            case Result of
                {ok, Tokens, Location1} ->
                    case erl2_parse:parse_form(Tokens) of
                        {ok, Form} ->
                            [Form | parse_chars(Chars1, Location1)];
                        {error, E} ->
                            [{error, E}]
                    end;
                {error, E, _Location1} ->
                    [{error, E}];
                {eof, EndLocation} ->
                    [{eof, EndLocation}]
            end;
        {more, _} ->
            case erl_scan:tokens([], Chars ++ eof, Location, ['text']) of
                {done, Result, _} ->
                    case Result of
                        {ok, Tokens = [FirstToken | _], _} ->
                            case erl2_parse:parse_form(Tokens) of
                                {ok, Form} ->
                                    [Form];
                                {error, _} ->
                                    [{error, {erl_scan:location(FirstToken)}}]
                            end;
                        {error, _, _} ->
                            [{error, {Location}}];
                        {eof, EndLocation} ->
                            [{eof, EndLocation}]
                    end;
                {more, _} ->
                    [{error, Location}]
            end
    end.
