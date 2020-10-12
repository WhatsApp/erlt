#!/usr/bin/env escript

%% -*- erlang -*-

-mode(compile).

main([BeamDir, OutDir]) ->
    BeamFilenames = filelib:wildcard(BeamDir ++ "/*.beam"),
    [ beam_to_erl(Filename, OutDir) || Filename <-  BeamFilenames],
    ok.

beam_to_erl(BeamFilename, OutDir) ->
    Base = filename:basename(BeamFilename, ".beam"),
    OutFilename = filename:join(OutDir, Base ++ ".erl"),
    {ok, {_, [{abstract_code, {_, Forms}}]}} = beam_lib:chunks(BeamFilename, [abstract_code]),
    {ok, File} = file:open(OutFilename, [write]),
    try
        [
            begin
                Chars = erl_prettypr:format(Form),
                io:put_chars(File, Chars),
                io:nl(File),
                io:nl(File)
            end
            || Form <- Forms
        ],
        ok
    after
        file:close(File)
    end.

