-module(make_escript).

main([]) ->
    Beams = ["erl2_parse.beam", "erl2etf.beam"],
    Archive = [{filename:basename(X), read_file(X)} || X <- Beams],
    ok =
        escript:create("erl2etf", [
            shebang,
            comment,
            {emu_args, "+sbtu +A0 -noinput -mode minimal -escript main erl2etf"},
            {archive, Archive, []}
        ]).

read_file(Name) ->
    {ok, Bin} = file:read_file(Name),
    Bin.
