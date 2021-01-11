#!/usr/bin/env escript

main([OutFile]) ->
    Text = io_lib:format("otp_lib_root=~s~n", [code:lib_dir()]),
    file:write_file(OutFile, Text, [write]).
