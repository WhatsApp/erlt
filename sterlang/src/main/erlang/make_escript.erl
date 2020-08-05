-module(make_escript).

main([Escript, MainMod | Beams]) when Beams =/= [] ->
    make_escript(Escript, MainMod, Beams).

make_escript(Escript, MainMod, Beams) ->
    %io:format("creating escript '~s' with body ~p~n", [Escript, Beams]),

    Archive = [ {filename:basename(X), read_file(X)} || X <- Beams ],

    % make escript args -- copied from erlc.c
    %
    % NOTE: original args from erlang-thrift:
    % Args = "-noshell -noinput -escript main " ++ MainMod
    %
    Args = string:join(
        [
            "+sbtu",
            "+A0",
            "-noinput",
            "-mode", "minimal",
            "-escript", "main", MainMod
        ],
        " "
    ),

    ok = filelib:ensure_dir(Escript),

    ok = escript:create(Escript, [
            shebang, comment,
            {emu_args, Args},
            {archive, Archive, []}
    ]),

    [] = os:cmd("chmod u+x " ++ Escript),
    ok.


read_file(Name) ->
    {ok, Bin} = file:read_file(Name),
    Bin.

