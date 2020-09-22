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

% some useful utility functions
-module(erlbuild_util).

-compile(export_all).
-compile(nowarn_export_all).

shell_command(Command) ->
    shell_command(Command, _ExtraSettings = []).

% run shell command and handle its output one line at a time by either:
%
%   - mirroring them to stdout, if 'mirror_line_output' option is set
%   - or returning the list of output lines in the order received, each
%     represented as a binary without trailing '\n'
%
% allowed extra settings: {env, ...}, {cd, ...}, {line, L}
shell_command(Command, ExtraSettings_0) ->
    {ExtraSettings, Acc} =
        case proplists:get_bool(mirror_line_output, ExtraSettings_0) of
            false ->
                {ExtraSettings_0, _Acc = []};
            true ->
                NewExtraSettings_ = [
                    {line, 4096}
                    | proplists:delete(mirror_line_output, ExtraSettings_0)
                ],
                {NewExtraSettings_, _Acc = 'undefined'}
        end,
    Port = open_spawn_port(Command, ExtraSettings),
    read_port_output(Port, _CurrentLine = <<>>, Acc).

open_spawn_port(Command, ExtraSettings) ->
    PortSettings =
        [
            binary,
            stream,
            use_stdio,
            stderr_to_stdout,
            exit_status
        ] ++ ExtraSettings,

    _Port = open_port({spawn, Command}, PortSettings).

read_port_output(Port, CurrentLine, Acc) ->
    receive
        {Port, {data, {eol, LineSegment}}} ->
            NewLine = <<CurrentLine/binary, LineSegment/binary>>,
            NewAcc = update_port_output_acc(NewLine, Acc),
            read_port_output(Port, _NewCurrentLine = <<>>, NewAcc);
        {Port, {data, {noeol, LineSegment}}} ->
            NewCurrentLine = <<CurrentLine/binary, LineSegment/binary>>,
            read_port_output(Port, NewCurrentLine, Acc);
        {Port, {data, Data}} ->
            read_port_output(Port, CurrentLine, [Data | Acc]);
        {Port, {exit_status, N}} ->
            Res = return_port_output_acc(CurrentLine, Acc),
            {N, Res}
    end.

update_port_output_acc(Line, Acc) ->
    case Acc of
        'undefined' ->
            % mirror port output to stdout
            io:fwrite(standard_io, "~s~n", [Line]),
            Acc;
        _ ->
            % collect port output
            [Line | Acc]
    end.

return_port_output_acc(CurrentLine, Acc) ->
    case Acc of
        'undefined' ->
            'undefined';
        _ ->
            FinalAcc = [CurrentLine || CurrentLine =/= <<>>] ++ Acc,
            lists:reverse(FinalAcc)
    end.

throw_error(Format, Args) ->
    Str = format(Format, Args),
    throw_error(Str).

throw_error(Str) ->
    throw({error, Str}).

format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).
