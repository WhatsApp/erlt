-module(make_escript).

main([]) ->
    {ok, Beam} = file:read_file("parser.beam"),
    ok =
        escript:create("parser", [
            shebang,
            comment,
            {emu_args, "+sbtu +A0 -noinput -mode minimal -escript main parser"},
            {beam, Beam}
        ]).
