#!/usr/bin/env escript

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

-mode(compile).

main(Files) ->
    Run =
    fun(ErltFile) ->
        Builddir = tmpdirname(ErltFile),
        ok = file:make_dir(Builddir),
        try
            compile_file(ErltFile, Builddir)
        after
            file:del_dir_r(Builddir)
        end
    end,

    case parallel(Run, Files) of
        [] ->
            ok;
        Errors ->
            lists:foreach(fun({error, Fmt, Args}) -> io:format(Fmt, Args) end, Errors)
    end.

tmpdirname(ErltFile) ->
    "_build_"++ErltFile++integer_to_list(erlang:system_time()).

compile_file(ErltFile, Builddir) ->
    ExpOutputFile = ErltFile ++ ".exp",
    Command = "erltc compile +warnings_as_errors --build-dir " ++ Builddir ++ " -o " ++ Builddir ++ " --build " ++ ErltFile,
    case eunit_lib:command(Command) of
        {0, Output} ->
            {error, "compilation succeeded unexpectedly, output: ~s", [Output]};
        {_, ActualOutput} ->
            case file:read_file(ExpOutputFile) of
                {error, Reason} ->
                    file:write_file(ExpOutputFile++".new", ActualOutput),
                    {error, "expectation file ~ts not found: ~ts~n", [ExpOutputFile, file:format_error(Reason)]};
                {ok, ExpOutput} ->
                    case string:equal(string:trim(ActualOutput), string:trim(ExpOutput)) of
                        false ->
                            file:write_file(ExpOutputFile++".new", ActualOutput),
                            {error,
                                "`~ts`~nExpected to see an output with:~n~n~s~nGot:~n~n~s~n",
                                [Command, ExpOutput, ActualOutput]};
                        true ->
                            io:format("OK (~s)~n", [ErltFile]),
                            ok
                    end
            end
    end.

parallel(Fun, List) ->
    N = erlang:system_info(schedulers) * 2,
    parallel_loop(Fun, List, N, [], []).

parallel_loop(_, [], _, [], Errors) ->
    Errors;
parallel_loop(Fun, [Elem | Rest], N, Refs, Errors) when length(Refs) < N ->
    {_, Ref} = erlang:spawn_monitor(fun() -> exit(Fun(Elem)) end),
    parallel_loop(Fun, Rest, N, [Ref | Refs], Errors);
parallel_loop(Fun, List, N, Refs0, Errors) ->
    receive
        {'DOWN', Ref, process, _, ok} ->
            Refs = Refs0 -- [Ref],
            parallel_loop(Fun, List, N, Refs, Errors);
        {'DOWN', Ref, process, _, {error, _, _} = Error} ->
            Refs = Refs0 -- [Ref],
            parallel_loop(Fun, List, N, Refs, [Error | Errors]);
        {'DOWN', _Ref, process, _, Crash} ->
            exit(Crash)
    end.
