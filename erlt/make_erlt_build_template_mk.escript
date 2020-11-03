#!/usr/bin/env escript

main(_Args) ->
    {ok, S} = file:read_file("src/erlt_build.template.mk"),
    Erl = io_lib:format(
        "-module(erlt_build_template_mk).\n"
        "-export([get_template/0]).\n"
        "get_template() -> ~p.\n", [S]
    ),
    ok = file:write_file("src/erlt_build_template_mk.erl", Erl).
