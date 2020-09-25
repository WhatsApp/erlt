#!/usr/bin/env escript

main(_Args) ->
    {ok, S} = file:read_file("src/erlbuild.template.mk"),
    Erl = io_lib:format(
        "-module(erlbuild_template_mk).\n"
        "-export([get_template/0]).\n"
        "get_template() -> ~p.\n", [S]
    ),
    ok = file:write_file("src/erlbuild_template_mk.erl", Erl).
