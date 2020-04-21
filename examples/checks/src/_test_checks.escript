#!/usr/bin/env escript

main([Erl2File]) ->
    Erl2cBin = "../../../erl2c/bin/erl2c",
    ExpOutputFile = Erl2File ++ ".exp",
    {ok, ExpOutput} =  file:read_file(ExpOutputFile),
    ExpOutPutStr = binary_to_list(ExpOutput),
    {ExitCode, ActualOutPut} = eunit_lib:command(Erl2cBin ++ " " ++ Erl2File),
    case ExitCode of
        0 ->
            io:format("`erl2c ~s` has not failed", [Erl2File]),
            halt(2);
        _ ->
            ok
    end,
    case string:str(ActualOutPut, ExpOutPutStr) of
        0 ->
            io:format(
                "`erl2c ~s`~nExpected to see an output with:~n  ~s~nGot:~n  ~s",
                [Erl2File, ExpOutPutStr, ActualOutPut]
            ),
            halt(2);
        _ ->
            io:format("OK (~s)~n", [Erl2File]),
            ok
    end.
