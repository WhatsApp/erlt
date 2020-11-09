#!/usr/bin/env escript

%% This file creates ./src/erlt_build_statics.erl,
%% which can be used for accessing the contents of
%% each static file as a string.
%%
%% erlt_build incremental mode uses the following
%% - erlt_build_mk_template is a Makefile template for incremental builds
%%
%% erlt_build_basic_mode.erl uses the following:
%% - defs files, defs.etf files, and beams from ./built_ins.
%%   See ./built_ins/README.md
%%
-module(make_templates).

-mode(compile).

main(_Args) ->
    Funs = [get_make_template() | get_built_ins(["lists"], ["t_io"])],
    write_module("erlt_build_statics", Funs).

get_make_template() ->
    {mk_template, read_or_error("erlt_build.template.mk")}.

to_name_and_contents(Ext) ->
        fun(Mod) ->
            Basename = Mod ++ Ext,
            FileName = filename:join(["built_ins", "build", Basename]),
            {Basename, read_or_error(FileName)}
        end.

get_built_ins(BuiltInTypes, BuiltInModules) ->
    Ebins = lists:map(to_name_and_contents(".beam"), BuiltInModules),

    TypesMods = BuiltInTypes ++ BuiltInModules,
    Defs = lists:map(to_name_and_contents(".defs"), TypesMods),
    DefsEtfs = lists:map(to_name_and_contents(".defs.etf"), TypesMods),
    [
        {built_in_ebins, Ebins},
        {built_in_defs, Defs ++ DefsEtfs}
    ].


%% @doc for each {FunName, Contents} in Funs, add a function to erlt_build_templates.erl
%% called <FunName> for getting the contents of ../templates/<FileName> as a string
write_module(ModuleName, Funs) ->
    JoinMap = fun(F) -> string:join(lists:map(F, Funs), "\n") end,
    Erl =
        io_lib:format("-module(~s).~n", [ModuleName]) ++
            JoinMap(fun to_export_statement/1) ++
            "\n" ++ 
            JoinMap(fun to_function_def/1),
    Dest = filename:join("src", ModuleName ++ ".erl"),
    ok = file:write_file(Dest, Erl).

to_export_statement({Name, _Contents}) ->
    io_lib:format(
        "~n-export([~s/0]).",
        [Name]
    ).

to_function_def({Name, Contents}) ->
    io_lib:format(
        "~n~s() -> ~0p.  ",
        [Name, Contents]
    ).

read_or_error(Path) ->
    case file:read_file(Path) of
        {error, Reason} ->
            io:format(standard_error, "error reading '~p': ~p  ", [
                Path,
                Reason
            ]),
            erlang:error(Reason);
        {ok, Contents} ->
            Contents
    end.
