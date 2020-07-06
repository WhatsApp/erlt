%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
%%
%% %CopyrightEnd%
%%
-module(erlbuild_erlc).

% NOTE: what you see below is based on copy-pasted erlang/lib/stdlib-3.7/src/erl_compile.erl (R21)
-include_lib("kernel/include/file.hrl").

-export([main/1]).

%Macro to avoid misspellings.
-define(STDERR, standard_error).

% NOTE: this record definition was copy-pasted from erlang/lib/stdlib-3.7/include/erl_compile.hrl (R21)
-record(options, {
    includes = [] :: [file:filename()],
    % Include paths (list of
    % absolute directory names).
    outdir = "." :: file:filename(),
    % Directory for result
    % (absolute path).

    % Type of output file.
    output_type = undefined :: atom(),
    defines = [] :: [atom() | {atom(), _}],
    % Preprocessor defines.  Each
    % element is an atom
    % (the name to define), or 
    % a {Name, Value} tuple.
    warning = 1 :: non_neg_integer(),
    % Warning level (0 - no
    % warnings, 1 - standard level,
    % 2, 3, ... - more warnings).

    % Verbose (true/false).
    verbose = false :: boolean(),
    % Optimize options.
    optimize = 999,
    % Compiler specific options.
    specific = [] :: [_],
    outfile = "" :: file:filename(),
    % Name of output file (internal
    % use in erl_compile.erl).
    cwd :: file:filename()
    % Current working directory
    % for erlc.
}).

%% Converts generic compiler options to specific options.
%%
%% NOTE: this piece was copy-pasted from erlang/lib/compiler-7.3/src/compile.erl (R21)

-import(lists, [
    member/2,
    reverse/1,
    reverse/2,
    keyfind/3,
    last/1,
    map/2,
    flatmap/2,
    foreach/2,
    foldr/3,
    any/2
]).

make_erl_options(Opts) ->
    #options{
        includes = Includes,
        defines = Defines,
        outdir = Outdir,
        warning = Warning,
        verbose = Verbose,
        specific = Specific,
        output_type = OutputType,
        cwd = Cwd
    } = Opts,
    Options =
        [verbose || Verbose] ++
            [report_warnings || Warning =/= 0] ++
            map(
                fun
                    ({Name, Value}) ->
                        {d, Name, Value};
                    (Name) ->
                        {d, Name}
                end,
                Defines
            ) ++
            case OutputType of
                undefined -> [];
                jam -> [jam];
                beam -> [beam];
                native -> [native]
            end,
    Options ++
        [report_errors, {cwd, Cwd}, {outdir, Outdir} | [{i, Dir} || Dir <- Includes]] ++
        Specific.

% Escript entry point
main(Args0) ->
    % handle -pa, -pz and remove them from the original args
    % 
    % NOTE, TODO: for now, only handling well-formed args, if things happen to
    % be misplaced things won't be handled correctly, e.g. -o -pa ...
    Args = handle_path_args(Args0),

    %io:format("Args0: ~tp\n", [Args0]),
    %io:format("Args: ~tp\n", [Args]),

    case compile(Args) of
        ok -> my_halt(0);
        error -> my_halt(1);
        _ -> my_halt(2)
    end.

handle_path_args([]) ->
    [];
handle_path_args(["-pa", Path | T]) ->
    code:add_patha(Path),
    handle_path_args(T);
handle_path_args(["-pz", Path | T]) ->
    code:add_pathz(Path),
    handle_path_args(T);
handle_path_args([H | T]) ->
    [H | handle_path_args(T)].

-type cmd_line_arg() :: atom() | string().

-spec my_halt(_) -> no_return().
my_halt(Reason) ->
    erlang:halt(Reason).

%% Run the the compiler in a separate process, trapping EXITs.

compile(List) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(compiler_runner(List)),
    receive
        {'EXIT', Pid, {compiler_result, Result}} ->
            Result;
        {'EXIT', Pid, {compiler_error, Error}} ->
            io:put_chars(?STDERR, Error),
            io:nl(?STDERR),
            error;
        {'EXIT', Pid, Reason} ->
            io:format(?STDERR, "Runtime error: ~tp~n", [Reason]),
            error
    end.

-spec compiler_runner([cmd_line_arg()]) -> fun(() -> no_return()).
compiler_runner(List) ->
    fun () ->
        %% We don't want the current directory in the code path.
        %% Remove it.
        Path = [D || D <- code:get_path(), D =/= "."],
        true = code:set_path(Path),
        exit({compiler_result, compile1(List)})
    end.

%% Parses the first part of the option list.

compile1(Args) ->
    {ok, Cwd} = file:get_cwd(),
    compile1(Args, #options{outdir = Cwd, cwd = Cwd}).

%% Parses all options.

compile1(["--" | Files], Opts) ->
    compile2(Files, Opts);
compile1(["-" ++ Option | T], Opts) ->
    parse_generic_option(Option, T, Opts);
compile1(["+" ++ Option | Rest], Opts) ->
    Term = make_term(Option),
    Specific = Opts#options.specific,
    compile1(Rest, Opts#options{specific = [Term | Specific]});
compile1(Files, Opts) ->
    compile2(Files, Opts).

parse_generic_option("b" ++ Opt, T0, Opts) ->
    {OutputType, T} = get_option("b", Opt, T0),
    compile1(T, Opts#options{output_type = list_to_atom(OutputType)});
parse_generic_option("D" ++ Opt, T0, #options{defines = Defs} = Opts) ->
    {Val0, T} = get_option("D", Opt, T0),
    {Key0, Val1} = split_at_equals(Val0, []),
    Key = list_to_atom(Key0),
    case Val1 of
        [] ->
            compile1(T, Opts#options{defines = [Key | Defs]});
        Val2 ->
            Val = make_term(Val2),
            compile1(T, Opts#options{defines = [{Key, Val} | Defs]})
    end;
parse_generic_option("help", _, _Opts) ->
    usage();
parse_generic_option("I" ++ Opt, T0, #options{cwd = Cwd} = Opts) ->
    {Dir, T} = get_option("I", Opt, T0),
    AbsDir = filename:absname(Dir, Cwd),
    compile1(T, Opts#options{includes = [AbsDir | Opts#options.includes]});
parse_generic_option("M" ++ Opt, T0, #options{specific = Spec} = Opts) ->
    case parse_dep_option(Opt, T0) of
        error ->
            error;
        {SpecOpts, T} ->
            compile1(T, Opts#options{specific = SpecOpts ++ Spec})
    end;
parse_generic_option("o" ++ Opt, T0, #options{cwd = Cwd} = Opts) ->
    {Dir, T} = get_option("o", Opt, T0),
    AbsName = filename:absname(Dir, Cwd),
    case file_or_directory(AbsName) of
        file ->
            compile1(T, Opts#options{outfile = AbsName});
        directory ->
            compile1(T, Opts#options{outdir = AbsName})
    end;
parse_generic_option("O" ++ Opt, T, Opts) ->
    case Opt of
        "" ->
            compile1(T, Opts#options{optimize = 1});
        _ ->
            Term = make_term(Opt),
            compile1(T, Opts#options{optimize = Term})
    end;
parse_generic_option("v", T, Opts) ->
    compile1(T, Opts#options{verbose = true});
parse_generic_option("W" ++ Warn, T, #options{specific = Spec} = Opts) ->
    case Warn of
        "all" ->
            compile1(T, Opts#options{warning = 999});
        "error" ->
            compile1(T, Opts#options{specific = [warnings_as_errors | Spec]});
        "" ->
            compile1(T, Opts#options{warning = 1});
        _ ->
            try list_to_integer(Warn) of
                Level ->
                    compile1(T, Opts#options{warning = Level})
            catch
                error:badarg ->
                    usage()
            end
    end;
parse_generic_option("E", T, #options{specific = Spec} = Opts) ->
    compile1(T, Opts#options{specific = ['E' | Spec]});
parse_generic_option("P", T, #options{specific = Spec} = Opts) ->
    compile1(T, Opts#options{specific = ['P' | Spec]});
parse_generic_option("S", T, #options{specific = Spec} = Opts) ->
    compile1(T, Opts#options{specific = ['S' | Spec]});
parse_generic_option("-build-phase", T0, #options{specific = Spec} = Opts) ->
    {PhaseString, T} = get_option("build-phase", "", T0),
    Phase =
        case PhaseString of
            "scan" ->
                scan;
            "compile" ->
                compile;
            _ ->
                exit(
                    {compiler_error,
                        "Invalid --build-phase '" ++
                            PhaseString ++ "'; must be 'scan' or 'compile'"}
                )
        end,
    PhaseOpt = {build_phase, Phase},
    compile1(T, Opts#options{specific = [PhaseOpt | Spec]});
parse_generic_option("-build-dir", T0, #options{specific = Spec} = Opts) ->
    {BuildDir, T} = get_option("-build-dir", "", T0),
    BuildDirOpt = {build_dir, BuildDir},
    compile1(T, Opts#options{specific = [BuildDirOpt | Spec]});
parse_generic_option(Option, _T, _Opts) ->
    io:format(?STDERR, "Unknown option: -~ts\n", [Option]),
    usage().

parse_dep_option("", T) ->
    {[makedep, {makedep_output, standard_io}], T};
parse_dep_option("D", T) ->
    {[makedep], T};
parse_dep_option("2", T) ->
    {[makedep, makedep2], T};
parse_dep_option("2C", T) ->
    {[makedep, makedep2, makedep2_compat], T};
parse_dep_option("MD", T) ->
    {[makedep_side_effect], T};
parse_dep_option("F" ++ Opt, T0) ->
    {File, T} = get_option("MF", Opt, T0),
    {[makedep, {makedep_output, File}], T};
parse_dep_option("G", T) ->
    {[makedep_add_missing], T};
parse_dep_option("P", T) ->
    {[makedep_phony], T};
parse_dep_option("Q" ++ Opt, T0) ->
    {Target, T} = get_option("MT", Opt, T0),
    {[makedep_quote_target, {makedep_target, Target}], T};
parse_dep_option("T" ++ Opt, T0) ->
    {Target, T} = get_option("MT", Opt, T0),
    {[{makedep_target, Target}], T};
parse_dep_option(Opt, _T) ->
    io:format(?STDERR, "Unknown option: -M~ts\n", [Opt]),
    usage().

usage() ->
    H = [
        {"-b type", "type of output file (e.g. beam)"},
        {"-d", "turn on debugging of erlc itself"},
        {"-Dname", "define name"},
        {"-Dname=value", "define name to have value"},
        {"-help", "shows this help text"},
        {"-I path", "where to search for include files"},
        {"-M", "generate a rule for make(1) describing the dependencies"},
        {"-M2",
            "similar to -M, but also includes dependencies on .beam files from behaviors and parse transforms"},
        {"-M2C",
            "-M2 with -M compatibility mode: -M2C means -M2 wihtout .beam dependencies; -M2C output should be identical to -M"},
        {"-MF file", "write the dependencies to 'file'"},
        {"-MT target",
            "change the target of the rule emitted by dependency "
            "generation"},
        {"-MQ target", "same as -MT but quote characters special to make(1)"},
        {"-MG",
            "consider missing headers as generated files and add them to "
            "the dependencies"},
        {"-MP", "add a phony target for each dependency"},
        {"-MD", "same as -M -MT file (with default 'file')"},
        {"-MMD", "generate dependencies as a side-effect"},
        {"-o name", "name output directory or file"},
        {"-pa path", "add path to the front of Erlang's code path"},
        {"-pz path", "add path to the end of Erlang's code path"},
        {"-smp", "compile using SMP emulator"},
        {"-v", "verbose compiler output"},
        {"-Werror", "make all warnings into errors"},
        {"-W0", "disable warnings"},
        {"-Wnumber", "set warning level to number"},
        {"-Wall", "enable all warnings"},
        {"-W", "enable warnings (default; same as -W1)"},
        {"-E", "generate listing of expanded code (Erlang compiler)"},
        {"-S", "generate assembly listing (Erlang compiler)"},
        {"-P", "generate listing of preprocessed code (Erlang compiler)"},
        {"+term", "pass the Erlang term unchanged to the compiler"}
    ],
    io:put_chars(?STDERR, [
        "Usage: erlc [Options] file.ext ...\n",
        "Options:\n",
        [io_lib:format("~-14s ~s\n", [K, D]) || {K, D} <- H]
    ]),
    io:put_chars(?STDERR, [
        "\nOptions used by erlbuild:\n"
        "  --build-phase <build_phase>   scan | compile\n"
        "  --build-dir <build_dir>       <build_dir> passed from erlbuild\n"
    ]),
    error.

get_option(_Name, [], [[C | _] = Option | T]) when C =/= $- ->
    {Option, T};
get_option(_Name, [_ | _] = Option, T) ->
    {Option, T};
get_option(Name, _, _) ->
    exit({compiler_error, "No value given to -" ++ Name ++ " option"}).

split_at_equals([$= | T], Acc) ->
    {lists:reverse(Acc), T};
split_at_equals([H | T], Acc) ->
    split_at_equals(T, [H | Acc]);
split_at_equals([], Acc) ->
    {lists:reverse(Acc), []}.

compile2(Files, #options{cwd = Cwd, includes = Incl} = Opts0) ->
    Opts = Opts0#options{includes = lists:reverse(Incl)},
    case Files of
        [] ->
            % XXX: I don't think this is a good idea to allow no input files,
            % but keeping this consistent with erlc behavior for now
            ok;
        [File] ->
            compile3(File, Cwd, Opts);
        _ ->
            % TODO, XXX: the reason we do not support compiling multiple .erl
            % files in 'erlbuild erlc', is because, as a first step, we need to
            % know the order in which these files should be compiled.
            %
            % While it is possible to perform compile order scan along with
            % compiling multiple .erl files, the compiler won't be able to
            % cache this information for subsequent runs. It feels that it is
            % better to separate compile order scan and compilation steps and
            % have them orchestrated by the build system, which can also
            % implement caching for compile order information.
            io:put_chars(
                ?STDERR,
                "'erlbuild erlc' expects only one input file, "
                "but more than one input files were given.\n"
            ),
            error
    end.

compile3(File, Cwd, Options) ->
    % NOTE: using same transformation of the filename as the original
    % erl_compile.erl code
    Root = filename:rootname(File),
    InFile = filename:absname(Root, Cwd),

    % invoke the Erlang compiler on .erl, use the original erlc code path for
    % any other file
    case filename:extension(File) of
        ".erl" ->
            CompileOptions = make_erl_options(Options),
            case catch erlbuild_compile:compile(InFile, CompileOptions) of
                ok ->
                    ok;
                error ->
                    error;
                {'EXIT', Reason} ->
                    io:format(?STDERR, "'erlbuild erlc' failed:\n~p~n", [Reason]),
                    error;
                Other ->
                    io:format(?STDERR, "'erlbuild erlc' returned:\n~p~n", [Other]),
                    error
            end;
        _ ->
            % TODO: call erlc compiler for non- .erl files
            %
            % it would be great if we could simply call
            % erl_compile:compile_cmdline() at this point, but unfortunately
            % escript args conventions are incompatible with
            % code:get_plain_arguments() used by erl_compile.
            %
            % One thing we could do to make this compatible throughout is,
            % instead of escript, use a shell script or modified erlc.c to call
            % 'erlbuild erlc'
            io:put_chars(
                ?STDERR,
                "erlbuild erlc does not support compiling non .erl files yet\n"
            ),
            error
    end.

%% Guesses if a give name refers to a file or a directory.

file_or_directory(Name) ->
    case file:read_file_info(Name) of
        {ok, #file_info{type = regular}} ->
            file;
        {ok, _} ->
            directory;
        {error, _} ->
            case filename:extension(Name) of
                [] -> directory;
                _Other -> file
            end
    end.

%% Makes an Erlang term given a string.

make_term(Str) ->
    case erl_scan:string(Str) of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens ++ [{dot, erl_anno:new(1)}]) of
                {ok, Term} ->
                    Term;
                {error, {_, _, Reason}} ->
                    io:format(?STDERR, "~ts: ~ts~n", [Reason, Str]),
                    throw(error)
            end;
        {error, {_, _, Reason}, _} ->
            io:format(?STDERR, "~ts: ~ts~n", [Reason, Str]),
            throw(error)
    end.
