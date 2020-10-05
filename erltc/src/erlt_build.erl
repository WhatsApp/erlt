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

% This is our support for multi-file builds.
% Currently our only public API for multi-file builds is the rebar3 plugin
% in ./rebar_prv_erlt.erl
%
% erlt_build is based on earlier work on erlbuild, a low-level build tool for Erlang
% See https://github.com/WhatsApp/erlt/blob/80877ae15b4a9300c69ce34ffb2e844bc9acc74b/erlbuild/README.md

-module(erlt_build).

-export([main/1]).

% These were used when we had a programmatic public API
% specific to builds.
-compile([{nowarn_unused_function, [
    compile/1,
    clean/1,
    run_command_as_api_function/3,
    get_letter_option/1,
    read_file/1,
    print_warning/1,
    print_warning/2
]}]).

-include("erlt_build_types.hrl").

command_name() ->
    "erlt_build".

print_usage() ->
    io:format("~s", [
        [
            "Usage: ",
            command_name(),
            " [command] ...\n"
            "\n",
            command_name(),
            " is a build system for Erlang.\n"
            "\n"
            "commands (Defaults to 'compile'):\n"
            "\n"
            "  compile [options] [erlc options] [--] <.erl files>\n"
            "      -- compile .erl, .yrl, and .xrl files in one src directory\n"
            "\n"
            "  clean [options]\n"
            "      -- clean compile results and incremental compilation state stored in <build_dir>\n"
            "\n"
            "\n"
            "options:\n"
            "  --incremental            do an incremental build. Defaults to false\n"
            "  -o <output_dir>          directory where the compiler is to place the output files. Defaults to ../ebin\n"
            "  --build-dir <build_dir>  directory for storing intermediate compilation state. Defaults to <output_dir>/../build\n"
            "  --src-dir <build_dir>    source directory. Defaults to the current working directory\n"
            "  --makefile <filename>    name for the generated makefile. Defaults to '<build_dir>/erlbuild.mk'. Use '-' for stdout.\n"
            "  --erlbuild <command>     erlbuild command. Defaults to the current command (argv[0]).\n"
            "  --gen-only               generate makefile, but don't run make\n"
            "\n"
            "  --erlc <command>           erlc command. Defaults to 'erlc'.\n"
            "  --erlbuild-erlc <command>  erlbuild erlc command. Defaults to '<erlbuild>-erlc'.\n"
            "\n"
            "  -j[jobs]       specifies the number of jobs (commands) to run simultaneously. -j defaults to the number of available CPU cores\n"
            "  -v[level]      verbose build output, <level> is a non-negative integer. -v defaults to -v9\n"
            "  -h, --help     display this list of options.\n"
            "\n"
            "erlc options:\n"
            "  -Dname         define name\n"
            "  -Dname=value   define name to have value\n"
            "  -I path        where to search for include files\n"
            "  -pa path       add path to the front of Erlang's code path\n"
            "  -pz path       add path to the end of Erlang's code path\n"
            "  -Werror        make all warnings into errors\n"
            "  -W0            disable warnings\n"
            "  -Wnumber       set warning level to number\n"
            "  -Wall          enable all warnings\n"
            "  -W             enable warnings (default; same as -W1)\n"
            "  +term          pass the Erlang term unchanged to the compiler\n"
        ]
    ]).

% API entry point
%
% returns ok | {error, string()}
compile(Argv) ->
    run_command_as_api_function(compile, fun run_compile_command/1, Argv).

% API entry point
%
% returns ok | {error, string()}
clean(Argv) ->
    run_command_as_api_function(clean, fun run_clean_command/1, Argv).

% entry point when called from erlt
main(Argv) ->
    case run_command(Argv) of
        ok ->
            ok;
        {error, ErrorStr} ->
            print_error(ErrorStr),
            erlang:halt(1)
    end.

run_command_as_api_function(Name, Fun, Argv) ->
    try
        Fun(Argv)
    catch
        {error, Reason} when is_list(Reason) ->
            {error, Reason};
        Class:Error:Stacktrace ->
            ErrorStr =
                erlt_build_util:format(
                    "internal error while running erlt_build:~s(~p):~n~p~nStacktrace:~n~p~n",
                    [
                        Name,
                        Argv,
                        {Class, Error},
                        Stacktrace
                    ]
                ),
            {error, ErrorStr}
    end.

run_command(Argv) ->
    try
        do_run_command(Argv)
    catch
        % thrown by throw_error()
        {error, Reason} when is_list(Reason) ->
            {error, Reason};
        Class:Error:Stacktrace ->
            ErrorStr =
                erlt_build_util:format("internal error while running ~s:~n\t~p~nStacktrace:~n~p~n", [
                    command_name(),
                    {Class, Error},
                    Stacktrace
                ]),
            {error, ErrorStr}
    end.

do_run_command(Argv0) ->
    {Command, Argv} = parse_command(Argv0),

    case Command of
        "compile" ->
            Erlbuild = filename:absname(escript:script_name()),
            CompileArgv = ["--erlbuild", Erlbuild | Argv],
            run_compile_command(CompileArgv);
        "clean" ->
            run_clean_command(Argv);
        _ ->
            erlt_build_util:throw_error("unknown command '~s'; see '~s -h' for list of commands", [
                Command,
                command_name()
            ])
    end.

parse_command([H]) when H == "help" orelse H == "-h" orelse H == "--help" ->
    print_usage(),
    erlang:halt(0);
parse_command([] = Argv) ->
    % Defautls to compile, when there are no aruments at all
    {"compile", Argv};
parse_command(["-" ++ _ | _] = Argv) ->
    % Defautls to compile, when first argument is an option
    {"compile", Argv};
parse_command([Arg | Rest] = Argv) ->
    case filename:extension(Arg) =/= "" of
        true ->
            % Defautls to compile, when the first argument looks like a file name
            {"compile", Argv};
        _ ->
            {_Command = Arg, Rest}
    end.

run_compile_command(Argv) ->
    Args = parse_command_args(compile, Argv),
    do_compile(Args).

run_clean_command(Argv) ->
    Args = parse_command_args(clean, Argv),
    do_clean(Args).

parse_command_args(Command, Args) ->
    Args0 = parse_args(Args, #args{command = Command}),
    check_option_compat(Args0),
    ensure_input_filenames_valid(Args0),

    Args1 = update_args_output_dir(Args0),
    Args2 = update_args_build_dir(Args1),
    Args3 = update_args_makefile(Args2),

    Args3.

% Defaults to ../ebin. Add -o <output_dir> to ERLC_FLAGS if missing
update_args_output_dir(Args) ->
    case Args#args.output_dir of
        undefined ->
            DefaultOutputDir = "../ebin",
            Args#args{
                output_dir = DefaultOutputDir,
                erlc_argv = ["-o", DefaultOutputDir | Args#args.erlc_argv]
            };
        _ ->
            Args
    end.

% Defaults to <output_dir>/../build
update_args_build_dir(Args) ->
    case Args#args.build_dir of
        undefined ->
            BaseDir = filename:dirname(Args#args.output_dir),
            DefaultBuildDir = filename:join(BaseDir, "build"),
            Args#args{
                build_dir = DefaultBuildDir
            };
        _ ->
            Args
    end.

% set makefile name if missing
update_args_makefile(Args) ->
    case Args#args.makefile of
        undefined ->
            Args#args{
                makefile = filename:join(Args#args.build_dir, "erlt_build.mk")
            };
        _ ->
            Args
    end.

parse_args(["--incremental" | T], Args) ->
    NewArgs = Args#args{
        incremental = true
    },
    parse_args(T, NewArgs);
parse_args(["-v" ++ Value | T], Args) ->
    Level =
        case Value of
            "" ->
                9;
            Str ->
                try
                    Int = list_to_integer(Str),
                    case Int >= 0 of
                        true -> Int;
                        false -> 0
                    end
                catch
                    _:_ ->
                        % set to max on arbitrary string
                        9
                end
        end,
    NewArgs = Args#args{
        verbose = Level
    },
    parse_args(T, NewArgs);
parse_args(["--gen-only" | T], Args) ->
    NewArgs = Args#args{
        gen_only = true
    },
    parse_args(T, NewArgs);
parse_args(["-j" ++ Value | T], Args) ->
    case Value of
        % empty string correponds to the number of CPU cores
        "" ->
            ok;
        _ ->
            try
                ValueInt = list_to_integer(Value),
                ValueInt > 0 orelse throw(positive_integer_expected),
                ValueInt
            catch
                _:_ ->
                    args_value_error("-j", Value, "Value must be a positive integer")
            end
    end,
    NewArgs = Args#args{
        jobs = Value
    },
    parse_args(T, NewArgs);
parse_args(["--makefile" | _] = Argv, Args) ->
    {Makefile, T} = get_long_option(Argv),
    NewArgs = Args#args{
        makefile = Makefile
    },
    parse_args(T, NewArgs);
parse_args(["--erlc" | _] = Argv, Args) ->
    {Erlc, T} = get_long_option(Argv),
    NewArgs = Args#args{
        erlc = Erlc
    },
    parse_args(T, NewArgs);
parse_args(["--erlbuild-erlc" | _] = Argv, Args) ->
    {ErlbuildErlc, T} = get_long_option(Argv),
    NewArgs = Args#args{
        erlbuild_erlc = ErlbuildErlc
    },
    parse_args(T, NewArgs);
parse_args(["--erlbuild" | _] = Argv, Args) ->
    {Erlbuild, T} = get_long_option(Argv),
    NewArgs = Args#args{
        erlbuild = Erlbuild
    },
    parse_args(T, NewArgs);
parse_args(["--build-dir" | _] = Argv, Args) ->
    {BuildDir, T} = get_long_option(Argv),
    NewArgs = Args#args{
        build_dir = BuildDir
    },
    parse_args(T, NewArgs);
parse_args(["--src-dir" | _] = Argv, Args) ->
    {SrcDir, T} = get_long_option(Argv),
    NewArgs = Args#args{
        src_dir = SrcDir
    },
    parse_args(T, NewArgs);
parse_args(["-o" ++ _ | _] = Argv, Args) ->
    {OutputDir, NewErlcArgv, T} = get_letter_option_and_copy(Argv, Args#args.erlc_argv),
    NewArgs = Args#args{
        output_dir = OutputDir,
        erlc_argv = NewErlcArgv
    },
    parse_args(T, NewArgs);
parse_args(Argv, Args) when Args#args.command =:= clean ->
    % at this point all valid 'clean' options should be parsed
    case Argv of
        [] -> Args;
        ["-" ++ _ = Option | _] -> erlt_build_util:throw_error("unknown option: '~s'", [Option]);
        [Arg | _] -> erlt_build_util:throw_error("unexpected positional argument: '~s'", [Arg])
    end;
% all options below are erlc options
parse_args(["-I" ++ _ | _] = Argv, Args) ->
    {_Option, NewErlcArgv, T} = get_letter_option_and_copy(Argv, Args#args.erlc_argv),
    NewArgs = Args#args{
        erlc_argv = NewErlcArgv
    },
    parse_args(T, NewArgs);
parse_args(["-D" ++ _ | _] = Argv, Args) ->
    {_Option, NewErlcArgv, T} = get_letter_option_and_copy(Argv, Args#args.erlc_argv),
    NewArgs = Args#args{
        erlc_argv = NewErlcArgv
    },
    parse_args(T, NewArgs);
parse_args(["-W" ++ _ = Option | T], Args) ->
    % NOTE: not doing any validation for single-word -W options
    NewArgs = Args#args{
        erlc_argv = [Option | Args#args.erlc_argv]
    },
    parse_args(T, NewArgs);
parse_args(["-pa" | _] = Argv, Args) ->
    {_Option, NewErlcArgv, T} = get_long_option_and_copy(Argv, Args#args.erlc_argv),
    NewArgs = Args#args{
        erlc_argv = NewErlcArgv
    },
    parse_args(T, NewArgs);
parse_args(["-pz" | _] = Argv, Args) ->
    {_Option, NewErlcArgv, T} = get_long_option_and_copy(Argv, Args#args.erlc_argv),
    NewArgs = Args#args{
        erlc_argv = NewErlcArgv
    },
    parse_args(T, NewArgs);
parse_args([("+" ++ _) = Arg | T], Args) ->
    % passing Erlang compiler option through unchanged
    NewArgs = Args#args{
        erlc_argv = [Arg | Args#args.erlc_argv]
    },
    parse_args(T, NewArgs);
parse_args(["--" = Arg | T], Args) ->
    % following erlc behavior here
    NewArgs = Args#args{
        erlc_argv = [Arg | Args#args.erlc_argv]
    },
    parse_file_args(T, NewArgs);
parse_args(["-" ++ _ = Option | _], _Args) ->
    erlt_build_util:throw_error("unknown option: '~s'", [Option]);
parse_args(Argv, Args) ->
    % following erlc behavior here
    parse_file_args(Argv, Args).

% all remaining Argv correspond to input file names
parse_file_args(_Argv = InputFiles, Args) ->
    [check_file_arg(X) || X <- InputFiles],

    case InputFiles =:= [] of
        false -> ok;
        true -> erlt_build_util:throw_error("no input files were given")
    end,

    Args#args{
        input_files = InputFiles,
        erlc_argv = lists:reverse(Args#args.erlc_argv)
    }.

check_file_arg(Filename) ->
    % NOTE: generated makefiles don't support source files with directory
    % components in them, because of various automatic rules, e.g. %.d: %.erl
    case lists:member($/, Filename) of
        false ->
            ok;
        true ->
            erlt_build_util:throw_error("invalid input file name '~s'. Name can't contain '/'", [
                Filename
            ])
    end,
    case Filename of
        "-" ++ _ ->
            erlt_build_util:throw_error("invalid input file name '~s'. Name can't start with '-'", [
                Filename
            ]);
        _ ->
            ok
    end,
    case filename:extension(Filename) of
        ?SOURCE_FILE_EXTENSION ->
            ok;
        _ ->
            erlt_build_util:throw_error(
                "invalid input file name '~s'. Extension must be " ++ ?SOURCE_FILE_EXTENSION,
                [Filename]
            )
    end.

get_letter_option(Argv) ->
    {Value, _Copy, T} = get_letter_option_value(Argv),
    {Value, T}.

get_letter_option_and_copy(Argv, OutputArgv) ->
    {Value, Copy, T} = get_letter_option_value(Argv),
    {Value, Copy ++ OutputArgv, T}.

% letter <space> then value
get_letter_option_value([[$-, _Letter] = Name | T]) ->
    get_option_value(Name, T);
% letter immediately followed by value
get_letter_option_value([[$-, Letter | Value] = Arg | T]) ->
    Name = [$-, Letter],
    check_option_value(Name, Value),
    {Value, _Copy = [Arg], T}.

get_option_value(Name, []) ->
    erlt_build_util:throw_error("no value given for option ~s", [Name]);
get_option_value(Name, [Value | T]) ->
    check_option_value(Name, Value),
    {Value, _Copy = [Value, Name], T}.

get_long_option([Name | T0]) ->
    {Value, _Copy, T} = get_option_value(Name, T0),
    {Value, T}.

get_long_option_and_copy([Name | T0] = _Argv, OutputArgv) ->
    {Value, Copy, T} = get_option_value(Name, T0),
    {Value, Copy ++ OutputArgv, T}.

check_option_value(Name, Value) ->
    case Value of
        % e.g. --makefile -
        "-" ->
            ok;
        "-" ++ _ ->
            args_value_error(Name, Value, "Value can't start with '-'");
        _ ->
            ok
    end.

check_option_compat(#args{incremental = false, makefile = Makefile}) when Makefile =/= undefined ->
    throw_needs_incremental("--makefile");
check_option_compat(#args{incremental = false, erlbuild_erlc = ErlbuildErlc}) when
    ErlbuildErlc =/= undefined
->
    throw_needs_incremental("--erlbuild-erlc");
check_option_compat(#args{incremental = false, jobs = Jobs}) when Jobs =/= undefined ->
    throw_needs_incremental("--jobs");
check_option_compat(#args{incremental = false, gen_only = true}) ->
    throw_needs_incremental("--gen-only");
check_option_compat(_) ->
    ok.

% in non-incremental mode, we are currently only supporting source erlt files: no yrl, xrl, etc.
ensure_input_filenames_valid(#args{incremental = false, input_files = InputFiles}) ->
    BadFilenames = [
        Filename
        || Filename <- InputFiles, filename:extension(Filename) =/= ?SOURCE_FILE_EXTENSION
    ],
    case length(BadFilenames) of
        0 ->
            ok;
        _ ->
            erlt_build_util:throw_error(
                "All input files must have extension ~p. Please try again without: ~p",
                [?SOURCE_FILE_EXTENSION, BadFilenames]
            )
    end;
ensure_input_filenames_valid(_) ->
    ok.

throw_needs_incremental(Opt) ->
    erlt_build_util:throw_error("to use option '~s', you must also use option --incremental", [Opt]).

args_value_error(Name, Value, ErrorStr) ->
    erlt_build_util:throw_error("invalid value for option ~s: '~s'. ~s", [Name, Value, ErrorStr]).

do_compile(#args{incremental = false} = Args) ->
    erlt_build_basic_mode:invoke(Args);
do_compile(#args{incremental = true, makefile = Makefile, gen_only = GenOnly} = Args) ->
    Makefile = generate_makefile(Args),
    case GenOnly orelse Makefile =:= "-" of
        true ->
            ok;
        false ->
            % use of ERL_COMPILER_OPTIONS and ERL_LIBS is not allowed in hermetic/reproducible builds
            %
            % TODO: make this configurable
            check_and_unset_environment_variable("ERL_COMPILER_OPTIONS"),
            check_and_unset_environment_variable("ERL_LIBS"),

            run_make(Makefile, Args, _Goal = undefined)
    end.

do_clean(#args{incremental = false} = Args) ->
    erlt_build_basic_mode:invoke(Args);
do_clean(Args) ->
    Makefile = Args#args.makefile,
    case filelib:is_regular(Makefile) of
        % nothing to do; this could a subsequent clean
        false ->
            ok;
        true ->
            run_make(Makefile, Args, _Goal = "clean"),
            % NOTE: ignoring file/dir deletion errors
            file:delete(Makefile),
            file:del_dir(Args#args.build_dir),
            ok
    end.

% XXX: add a mode for making all paths absolute?
generate_makefile(Args) ->
    Makefile =
        case Args#args.makefile of
            "-" ->
                standard_io;
            Filename ->
                Filename
        end,

    % TODO: error message
    ok = filelib:ensure_dir(Makefile),

    Erlbuild = Args#args.erlbuild,

    Erlc =
        case Args#args.erlc of
            'undefined' ->
                "erlc";
            Erlc_ ->
                Erlc_
        end,

    ErlbuildErlc =
        case Args#args.erlbuild_erlc of
            'undefined' ->
                Erlbuild ++ "-erlc";
            ErlbuildErlc_ ->
                ErlbuildErlc_
        end,

    % see ./erlbuild.template.mk for complete list of input parameters
    InputParameters = [
        gen_input_parameter("EBIN", Args#args.output_dir),
        gen_input_parameter("BUILD_DIR", Args#args.build_dir),
        gen_input_parameter(
            "ERLC_FLAGS",
            lists:join(" ", [quote_shell_arg(X) || X <- Args#args.erlc_argv])
        ),
        gen_input_parameter("ERLC", Erlc),
        gen_input_parameter("ERLBUILD_ERLC", ErlbuildErlc),
        gen_input_parameter("SOURCES", lists:join(" ", Args#args.input_files)),
        "\n"
    ],

    write_file(Makefile, [InputParameters, erlt_build_template_mk:get_template()]),
    Makefile.

quote_shell_arg(("+" ++ _) = Arg) ->
    ["\"", escape_double_quotes(Arg), "\""];
% XXX: anything else we need to taker care of?
quote_shell_arg(Arg) ->
    Arg.

%% "escape inside these"
escape_double_quotes(Str) ->
    re:replace(Str, "([\"\\\\`!$&*;])", "\\\\&", [global, {return, list}, unicode]).

gen_input_parameter(Name, false) ->
    % representing boolean false as empty
    gen_input_parameter(Name, "");
gen_input_parameter(Name, Value) ->
    [Name, " := ", to_string(Value), "\n"].

to_string(X) when is_list(X) ->
    X;
to_string(X) when is_atom(X) ->
    atom_to_list(X).

run_make(Makefile, Args, Goal) ->
    % TODO: allow specifiying 'make' command explicitly through a parameter or
    % environment variable
    Make =
        case os:type() of
            {unix, BSD} when BSD =:= freebsd; BSD =:= openbsd ->
                "gmake";
            % e.g. Linux and Max OS X
            {unix, _} ->
                "make";
            OsType ->
                erlt_build_util:throw_error("unsupported os type for running GNU make: ~w", [OsType])
        end,

    VerboseOption =
        case Args#args.verbose of
            0 -> undefined;
            VerboseLevel -> "VERBOSE=" ++ integer_to_list(VerboseLevel)
        end,

    JobsOption =
        case Args#args.jobs of
            undefined -> undefined;
            JobsValue -> " -j" ++ JobsValue
        end,

    ChangeDirOption =
        case Args#args.src_dir of
            undefined -> undefined;
            SrcDir -> "-C " ++ SrcDir
        end,

    GoalArg =
        case Goal of
            undefined -> undefined;
            _ -> Goal
        end,

    Command =
        make_shell_command_line([
            Make,
            ChangeDirOption,
            "--no-print-directory",
            "-f",
            Makefile,
            JobsOption,
            VerboseOption,
            GoalArg
        ]),

    Args#args.verbose >= 2 andalso print_log(Command),

    % NOTE: mirroring output to stdout line by line, not capturing any
    {ReturnCode, _Output = undefined} =
        erlt_build_util:shell_command(Command, [mirror_line_output]),

    case ReturnCode of
        0 ->
            ok;
        _ ->
            % TODO, XXX: print Command that failed when running in non-verbose mode
            erlt_build_util:throw_error("make failed with code ~w", [ReturnCode])
    end.

% produce a ' '-separated string from Argv
make_shell_command_line(Argv) ->
    % NOTE: skipping undefined Args
    lists:join(" ", [X || X <- Argv, X =/= 'undefined']).

write_file(File, Body) when File =:= standard_io; File =:= standard_error ->
    ok = file:write(File, iolist_to_binary(Body));
write_file(Filename, Body) ->
    % TODO: error message
    ok = file:write_file(Filename, iolist_to_binary(Body)).

read_file(Filename) ->
    % TODO: error message
    {ok, FileContents} = file:read_file(Filename),
    FileContents.

check_and_unset_environment_variable(Name) ->
    case os:getenv(Name) of
        false ->
            ok;
        _ ->
            % NOTE: disabling warning for now to avoid noise if users have these setup
            %print_warning("disabling environment variable ~s; it is not allowed by current configuration", [Name]),

            os:unsetenv(Name)
    end.

% utility functions
print_error(Str) ->
    print_error("~s", [Str]).

print_error(Format, Args) ->
    io:format(standard_error, "Error: " ++ Format ++ "\n\n", Args).

print_warning(Str) ->
    print_error("~s", [Str]).

print_warning(Format, Args) ->
    io:format(standard_error, "Warning: " ++ Format ++ "\n\n", Args).

print_log(Str) ->
    print_log("~s", [Str]).

print_log(Format, Args) ->
    io:format(standard_io, Format ++ "\n", Args).
