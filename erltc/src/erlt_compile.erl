%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
%% Purpose: Run the ErlT compiler.
-module(erlt_compile).

-include("erlt_common.hrl").

%% High-level interface.
%%
%% TODO: implement forms() and potentialy other APIs supported by the standard compile.erl
-export([file/2]).

%% erltc interface.
-export([compile/2]).

-export([format_error/1]).

%
% NOTE: code below is based on copy-pasted pieces from erlang/lib/compiler-7.3/src/compile.erl (R21)
%
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

%Macro to avoid misspellings.
-define(STDERR, standard_error).

-type err_warn_info() :: tuple().

-type option() :: atom() | {atom(), term()} | {'d', atom(), term()}.

% NOTE: slimmed down version of the original compile state
-record(compile, {
    filename = "" :: file:filename(),
    dir = "" :: file:filename(),
    base = "" :: file:filename(),
    ifile = "" :: file:filename(),
    ofile = "" :: file:filename(),
    module = [] :: module() | [],
    %Options for compilation
    options = [] :: [option()],
    encoding = none :: none | erlt_epp:source_encoding(),
    errors = [] :: [err_warn_info()],
    warnings = [] :: [err_warn_info()],
    build_dir :: undefined | file:filename(),
    original_forms,
    global_defs :: undefined | erlt_defs:defs(),
    variable_state :: undefined | erlt:var_state(),
    % path to .defs file (contains specs, types, enums, and structs)
    defs_file :: undefined | file:filename(),
    etf_file :: undefined | file:filename(),
    % whether we have written a defs file to disk
    has_written_defs_file = false :: boolean()
}).

-define(pass(P), {P, fun P/2}).

-define(DefFileSuffix, ".defs").
-define(EtfFileSuffix, ".etf").

% called by erltc.erl
%
% XXX: move to erltc.erl? as it is unlikely to be useful for anything else
compile(File0, Options) ->
    File = shorten_filename(File0),
    case file(File, Options) of
        {ok, _Mod} -> ok;
        _Other -> error
    end.

shorten_filename(Name0) ->
    {ok, Cwd} = file:get_cwd(),
    case lists:prefix(Cwd, Name0) of
        false ->
            Name0;
        true ->
            case lists:nthtail(length(Cwd), Name0) of
                "/" ++ N -> N;
                N -> N
            end
    end.

file(File, Options) ->
    try
        do_file(File, Options)
    catch
        Class:Error:Stk ->
            io:format(
                ?STDERR,
                "internal error while compiling ~s.erl:~n\t~p~nStacktrace:~n~p~n",
                [
                    File,
                    {Class, Error},
                    Stk
                ]
            ),
            error
    end.

do_file(File, Options0) ->
    %io:format("Options: ~tp\n", [Options0]),
    BuildPhase =
        case keyfind(build_phase, 1, Options0) of
            {build_phase, Phase} -> Phase;
            false -> undefined
        end,

    IsMakedep2Mode = is_makedep2_mode(Options0),
    CompileMode =
        case BuildPhase of
            scan ->
                build_scan;
            compile ->
                build_compile;
            undefined when IsMakedep2Mode ->
                makedep2;
            _ ->
                compile
        end,

    % TODO: make sure that "ERL_COMPILER_OPTIONS" environment variable if
    % defined, does not specify parse_transforms. Otherwise, we are risking
    % running parse_transforms twice. Plus, we are going to be restricting how
    % parse transforms can be specified anyway.
    EnvCompilerOptions = compile:env_compiler_options(),
    Options = fix_compile_options(Options0 ++ EnvCompilerOptions, CompileMode),

    Passes =
        case CompileMode of
            makedep2 ->
                % 'erlc -M2' -- a new dependency scanner, alternative to 'erlc -M' aka makedep; among other things, it
                % allows to establish the order in which .erl files should be compiled
                %
                % this mode exists mainly for testing the new dependency scanner
                base_passes() ++
                    [
                        ?pass(erlt_import),
                        ?pass(output_compile_deps)
                    ];
            build_scan ->
                % build scan phase -- generate depfiles; later, we are also going to
                % extract declarations from parsed module, and cache the parse tree
                base_passes() ++
                    [
                        ?pass(erlt_import),
                        ?pass(output_declarations),
                        ?pass(output_compile_deps)
                    ];
            build_compile ->
                % build compile phase -- later, we are going to use this for optimizing
                % compilation by recovering information cached during the "scan" phase
                [
                    % TODO: do not remove the output file unless we know save_binary() is going to run
                    ?pass(remove_file),
                    ?pass(collect_definitions)
                ] ++
                    base_passes() ++
                    [
                        ?pass(output_etf),
                        ?pass(erlt_typecheck),
                        ?pass(erlt_import),
                        ?pass(erlt_to_erl1),
                        ?pass(transform_module),
                        ?pass(compile_erl1_forms),
                        ?pass(maybe_save_binary)
                    ];
            compile ->
                % normal .erl compilation -- for erl1, should be identical to erlc behavior
                [
                    % TODO: do not remove the output file unless we know save_binary() is going to run
                    ?pass(remove_file),
                    {unless, 'etf', {unless, 'defs', ?pass(collect_definitions)}}
                ] ++
                    base_passes() ++
                    [
                        {iff, 'B', {src_listing, "B"}},
                        {iff, 'etf', {binary_listing, "etf"}},
                        {iff, 'defs', {binary_listing, "defs"}},
                        {unless, 'P', {unless, 'E', ?pass(erlt_typecheck)}},
                        ?pass(erlt_import),
                        ?pass(erlt_to_erl1),
                        {iff, 'A', {src_listing, "A"}},
                        ?pass(transform_module),
                        ?pass(compile_erl1_forms),
                        ?pass(maybe_save_binary)
                    ]
        end,
    Passes1 = select_passes(Passes, Options),

    % TODO, XXX: derive makedep_output from it?
    BuildDir =
        case keyfind(build_dir, 1, Options) of
            {build_dir, Dir} -> Dir;
            false -> "build"
        end,

    St0 = #compile{
        options = Options,
        build_dir = BuildDir
    },
    internal_comp(Passes1, _Code0 = unused, File, _Suffix = ?SOURCE_FILE_EXTENSION, St0).

base_passes() ->
    [
        ?pass(parse_module),
        ?pass(check_parse_errors),
        ?pass(extract_options),
        ?pass(erlt_lint),
        ?pass(erlt_track_vars)
    ].

is_makedep2_mode(Options) ->
    member(makedep2, Options) andalso member(makedep, Options).

keep_relative_paths(Deps) ->
    [X || X = {_, Filename} <- Deps, filename:pathtype(Filename) =:= relative].

optionally(true, F, X) -> F(X);
optionally(false, _F, X) -> X.

% TODO: add a mode for printing deps in JSON format (-MJSON ?)
output_compile_deps(Forms, St) ->
    Deps0 = get_compile_deps(Forms, St),

    OptionSet = fun(Option) -> member(Option, St#compile.options) end,
    TargetBeam = shorten_filename(St#compile.ofile),

    Deps = optionally(OptionSet(makedep), fun keep_relative_paths/1, Deps0),
    DepsFilenames = deps_to_unique_filenames(Deps),
    RuleCode = gen_make_rule(TargetBeam, DepsFilenames),

    DefsRuleCode =
        case St#compile.has_written_defs_file of
            true ->
                Erls = lists:filter(fun can_be_dep_of_defs_file/1, DepsFilenames),
                gen_make_rule(shorten_filename(St#compile.defs_file), Erls);
            false ->
                ""
        end,

    PhonyRulesCode =
        case OptionSet(makedep_phony) of
            false -> "";
            true -> [gen_make_phony_rule(St#compile.ifile, X) || X <- DepsFilenames]
        end,

    % adding the rule to rebuild the depfile itself
    MakedepRuleCode =
        case proplists:get_value(makedep_output, St#compile.options) of
            undefined ->
                "";
            MakedepFilename ->
                % NOTE: only including parse transforms as dependency graph
                % dependencies, if they were actually run
                IncludeParseTransforms = member(
                    makedep2_run_parse_transforms,
                    St#compile.options
                ),
                [
                    "\n",
                    gen_depfile_make_rule(MakedepFilename, Deps, IncludeParseTransforms)
                ]
        end,

    Code = [
        RuleCode,
        DefsRuleCode,
        MakedepRuleCode,
        PhonyRulesCode
    ],

    %% Write the Make rules to the selected output.
    %% If no output is specified, the default is to write rules to stdout
    Output0 =
        case proplists:get_value(makedep_output, St#compile.options) of
            undefined ->
                standard_io;
            O ->
                O
        end,

    %% If the caller specified an io_device(), there's nothing to do. If he
    %% specified a filename, we must create it. Furthermore, this created file
    %% must be closed before returning.
    Ret =
        case Output0 of
            _ when is_list(Output0) ->
                case file:delete(Output0) of
                    Ret2 when Ret2 =:= ok; Ret2 =:= {error, enoent} ->
                        case file:open(Output0, [write]) of
                            {ok, IODev} ->
                                {ok, IODev, true};
                            {error, Reason2} ->
                                {error, open, Reason2}
                        end;
                    {error, Reason1} ->
                        {error, delete, Reason1}
                end;
            _ ->
                {ok, Output0, false}
        end,

    case Ret of
        {ok, Output1, CloseOutput} ->
            try
                %% Write the Makefile.
                io:fwrite(Output1, "~ts", [Code]),
                %% Close the file if relevant.
                if
                    CloseOutput -> ok = file:close(Output1);
                    true -> ok
                end,
                {ok, Code, St}
            catch
                error:_ ->
                    %% Couldn't write to output Makefile.
                    Err = {St#compile.ifile, [{none, ?MODULE, write_error}]},
                    {error, St#compile{errors = St#compile.errors ++ [Err]}}
            end;
        {error, open, Reason} ->
            %% Couldn't open output Makefile.
            Err = {St#compile.ifile, [{none, ?MODULE, {open, Reason}}]},
            {error, St#compile{errors = St#compile.errors ++ [Err]}};
        {error, delete, Reason} ->
            %% Couldn't open output Makefile.
            Err = {St#compile.ifile, [{none, ?MODULE, {delete, Output0, Reason}}]},
            {error, St#compile{errors = St#compile.errors ++ [Err]}}
    end.

deps_to_unique_filenames(Deps) ->
    L = [Filename || {_DepType, Filename} <- Deps],
    lists:usort(L).

gen_make_rule(_Target, _DepsFilenames = []) ->
    [];
gen_make_rule(Target, DepsFilenames) ->
    [FirstFilename | RestFilenames] = DepsFilenames,
    RestLines = [[" \\\n  ", X] || X <- RestFilenames],
    [Target, ": ", FirstFilename, RestLines, "\n"].

gen_make_phony_rule(Ifile, Filename) ->
    case Ifile =:= Filename of
        % skip the .erl file being compiled
        true ->
            "";
        false ->
            ["\n", Filename, ":\n"]
    end.

gen_depfile_make_rule(Target, Deps0, IncludeParseTransforms) ->
    % .d makefile depends only on includes and parse_transforms (not behaviors
    % and other stuff)
    Deps1 = [
        X
        || X = {DepType, _} <- Deps0,
           DepType =:= file orelse
               (DepType =:= parse_transform andalso IncludeParseTransforms)
    ],
    DepsFilenames = deps_to_unique_filenames(Deps1),
    gen_make_rule(Target, DepsFilenames).

check_parse_errors(Forms, St0) ->
    do_check_parse_errors(Forms, St0).

do_check_parse_errors(Forms, St0) ->
    % in normal Erlang compilation lexer, parser, and epp errors are reported by
    % erl_lint.erl which is run as one of the later passes inside compile_erl1_forms()
    %
    % under erlt stricter compilation model we no longer allow lexer & parser errors to
    % slip through, because we need valid forms to be able to run erlt to erl1
    % translator and the erlt typechecker
    %
    % under erlt stricter compilation model we no longer allow epp errors,
    % e.g. includes that are not found at the dependency scan stage. For
    % example, adding originally missing include later may result in a
    % different compile order, not including transitive dependencies, or a
    % different parse output, because of conditional compilation. This may lead
    % to inconsistent build.
    case get_parse_errors(Forms) of
        [] ->
            {ok, Forms, St0};
        Errors ->
            {error, St0#compile{errors = Errors}}
    end.

get_parse_errors(Forms) ->
    get_parse_errors(Forms, _File = 'undefined', _Acc = []).

get_parse_errors([], _File, Acc) ->
    lists:reverse(Acc);
get_parse_errors([{attribute, _, file, {NewFile, _}} | Rest], _File, Acc) ->
    % update the name of the current file
    get_parse_errors(Rest, NewFile, Acc);
get_parse_errors([{error, Error} | Rest], File, Acc) ->
    % lexer/parser/epp error
    CompileError = {File, [Error]},
    get_parse_errors(Rest, File, [CompileError | Acc]);
get_parse_errors([_ | Rest], File, Acc) ->
    get_parse_errors(Rest, File, Acc).

get_attr_deps(Forms, St0) ->
    get_attr_deps(Forms, St0, _File = St0#compile.ifile, _Acc = []).

get_attr_deps([], _St, _File, Acc) ->
    % NOTE: returning in the order they were present in the file
    lists:reverse(Acc);
get_attr_deps([{attribute, _, file, {NewFile0, _}} | Rest], St, _File, Acc) ->
    % Remove "./" in front of the dependency filename.
    NewFile = remove_dot_slash(NewFile0),

    Dep = {file, NewFile},
    NewAcc =
        case member(Dep, Acc) of
            true -> Acc;
            false -> [Dep | Acc]
        end,
    % update the name of the current file
    get_attr_deps(Rest, St, NewFile, NewAcc);
get_attr_deps([{attribute, _Line, import, _Value} | Rest], St, File, Acc) ->
    % ignoring for now, since these are runtime, rather than compile-time dependencies
    % Furthermore, imports could be circular, which is likely the reason for
    % not handling them at compile time in the current implementation.
    % Note that *usages* of types from other modules are treated as dependencies.
    get_attr_deps(Rest, St, File, Acc);
get_attr_deps([{attribute, _Line, compile, _Value} | Rest], St, File, Acc) ->
    % in classic Erlang erlc, information about parse transforms is read from the beam file by the compiler,
    % we decided not to support parse transforms for the prototype - we may revisit this
    % to see how this was done before: b928e49f83bd7ab2dcad40e8f6efdefc19319f53
    get_attr_deps(Rest, St, File, Acc);
get_attr_deps([{attribute, _, behavior, _} | Rest], St, File, Acc) ->
    % in classic Erlang erlc, information about behaviors is read from the beam file by the compiler,
    % but in erlT we will likely get these from .defs files - if and when we revisit support of behaviors for erlT
    % to see how this was done before: b928e49f83bd7ab2dcad40e8f6efdefc19319f53
    get_attr_deps(Rest, St, File, Acc);
get_attr_deps([_ | Rest], St, File, Acc) ->
    get_attr_deps(Rest, St, File, Acc).

remove_dot_slash("./" ++ File) -> File;
remove_dot_slash(Other) -> Other.

% not yet handling `ModuleDepType`s: behavior | parse_transform | core_transform
% or dependencies from other directories.
% see this file at b928e49f83bd7ab2dcad40e8f6efdefc19319f53 for an earlier take on how to handle those
resolve_defs_file(Loc, Mod, St) ->
    ModuleDepType = typed_dep,
    Erl = filename:join(St#compile.dir, module_to_erl(Mod)),
    DefsFileBasename = module_to_defs_file(Mod),
    case filelib:is_regular(Erl) of
        true ->
            % Mod's .erl is next to the original source .erl file, i.e. they
            % are in the same application. Make it depend on the .beam file
            % next to the original target .beam
            DefsFilename0 = filename:join(St#compile.build_dir, DefsFileBasename),
            % NOTE: have to use the same shorten_filename() transformation as
            % on the TargetBeam in output_compile_deps()
            DefsFilename = shorten_filename(DefsFilename0),

            {ModuleDepType, DefsFilename};
        false ->
            % it has to be present somewhere for compilation to work
            case code:which(Mod) of
                Path when is_list(Path) ->
                    % one up from ebin
                    ParentDir = filename:dirname(filename:dirname(Path)),
                    % watch out: build dir might not always be called "build"
                    DefsFilename = filename:join([ParentDir, "build", DefsFileBasename]),
                    {ModuleDepType, DefsFilename};
                _ ->
                    {ErrorFile, ErrorLine} = Loc,
                    Error =
                        {ErrorFile, [
                            {ErrorLine, ?MODULE, {module_dependency, ModuleDepType, Mod}}
                        ]},

                    % NOTE: the error thrown here will be caught by internal_comp() -> Run0
                    throw({error, St#compile{errors = [Error]}})
            end
    end.

module_to_erl(Mod) ->
    atom_to_list(Mod) ++ ?SOURCE_FILE_EXTENSION.

module_to_defs_file(Mod) ->
    atom_to_list(Mod) ++ ?DefFileSuffix.

fix_compile_options(Options, CompileMode) ->
    Options1 = lists:filter(
        fun
            (report_warnings) ->
                CompileMode =:= build_compile orelse
                    CompileMode =:= compile;
            (_) ->
                true
        end,
        Options
    ),
    [no_error_module_mismatch | Options1].

compile_erl1_forms(Forms, St0) ->
    % NOTE: using forms_noenv() instead of forms(), because we've already
    % appended env_compiler_options() above
    % Make the compiler return errors and warnings instead of printing them
    %% suppress some warnings in standard compiler because we have already
    %% warned about them in our custom lint pass.
    Opts0 =
        St0#compile.options --
            [nowarn_unused_vars, nowarn_unused_type, report_warnings, report_errors],
    Ret = compile:noenv_forms(Forms, [
        return_errors,
        return_warnings,
        nowarn_unused_type,
        nowarn_unused_vars,
        {source, St0#compile.filename}
        | Opts0
    ]),

    % TODO: handling of ok is not exhaustive, there could also be {ok, ModuleName, Warnings}
    case Ret of
        {ok, ModuleName} ->
            {ok, none, St0#compile{module = ModuleName}};
        {ok, ModuleName, BinaryOrCode} ->
            {ok, BinaryOrCode, St0#compile{module = ModuleName}};
        {ok, ModuleName, BinaryOrCode, Ws} ->
            Ws0 = St0#compile.warnings,
            {ok, BinaryOrCode, St0#compile{module = ModuleName, warnings = Ws0 ++ Ws}};
        {error, Ws, Es} ->
            Ws0 = St0#compile.warnings,
            Es0 = St0#compile.errors,
            {error, St0#compile{errors = Es0 ++ Es, warnings = Ws0 ++ Ws}};
        error ->
            {error, St0}
    end.

maybe_save_binary(Code, St) ->
    case is_binary(Code) of
        true ->
            save_binary(Code, St);
        false ->
            {ok, none, St}
    end.

format_error({sterlang, ExitCode, Output}) ->
    io_lib:format("sterlang exited with error code ~w: ~n~s~n", [ExitCode, Output]);
format_error({module_dependency, defs_dependency, Mod}) ->
    io_lib:format("can't find ~s.~s", [Mod, ?DefFileSuffix]);
format_error({module_dependency, ModuleDepType, Mod}) ->
    io_lib:format("can't find ~s from -~s(~s)", [Mod, ModuleDepType, Mod]);
format_error(X) ->
    % TODO: copy formatters for locally-generated errors from copy-pasted code
    % here to avoid problems with error format compatibility in the future
    compile:format_error(X).

internal_comp(Passes, Code0, File, Suffix, St0) ->
    Dir = filename:dirname(File),
    Base = filename:basename(File, Suffix),
    St1 = St0#compile{
        filename = File,
        dir = Dir,
        base = Base,
        ifile = erlfile(Dir, Base, Suffix),
        ofile = objfile(Base, St0),
        defs_file = filename:join(St0#compile.build_dir, Base ++ ?DefFileSuffix),
        etf_file = filename:join(St0#compile.build_dir, Base ++ ?EtfFileSuffix)
    },
    Opts = St1#compile.options,
    Run0 =
        case member(time, Opts) of
            true ->
                io:format("Compiling ~tp\n", [File]),
                fun run_tc/3;
            false ->
                fun({_Name, Fun}, Code, St) ->
                    catch Fun(Code, St)
                end
        end,
    Run =
        case keyfind(eprof, 1, Opts) of
            {eprof, EprofPass} ->
                fun(P, Code, St) ->
                    run_eprof(P, Code, EprofPass, St)
                end;
            false ->
                Run0
        end,
    case fold_comp(Passes, Run, Code0, St1) of
        {ok, Code, St2} -> comp_ret_ok(Code, St2);
        {error, St2} -> comp_ret_err(St2)
    end.

fold_comp([{Name, Pass} | Ps], Run, Code0, St0) ->
    case Run({Name, Pass}, Code0, St0) of
        {ok, Code, St1} ->
            fold_comp(Ps, Run, Code, St1);
        {error, _St1} = Error ->
            Error;
        {'EXIT', Reason} ->
            Es = [{St0#compile.ifile, [{none, ?MODULE, {crash, Name, Reason}}]}],
            {error, St0#compile{errors = St0#compile.errors ++ Es}};
        Other ->
            Es = [{St0#compile.ifile, [{none, ?MODULE, {bad_return, Name, Other}}]}],
            {error, St0#compile{errors = St0#compile.errors ++ Es}}
    end;
fold_comp([], _Run, Code, St) ->
    {ok, Code, St}.

run_tc({Name, Fun}, Code, St) ->
    T1 = erlang:monotonic_time(),
    Val = (catch Fun(Code, St)),
    T2 = erlang:monotonic_time(),
    Elapsed = erlang:convert_time_unit(T2 - T1, native, millisecond),
    Mem0 = erts_debug:flat_size(Val) * erlang:system_info(wordsize),
    Mem = lists:flatten(io_lib:format("~.1f kB", [Mem0 / 1024])),
    io:format(" ~-30s: ~10.3f s ~12s\n", [Name, Elapsed / 1000, Mem]),
    Val.

run_eprof({Name, Fun}, Code, Name, St) ->
    io:format("~p: Running eprof\n", [Name]),
    c:appcall(tools, eprof, start_profiling, [[self()]]),
    Val = (catch Fun(Code, St)),
    c:appcall(tools, eprof, stop_profiling, []),
    c:appcall(tools, eprof, analyze, []),
    Val;
run_eprof({_, Fun}, Code, _, St) ->
    catch Fun(Code, St).

comp_ret_ok(Code, #compile{warnings = Warn0, module = Mod, options = Opts} = St) ->
    case werror(St) of
        true ->
            case member(report_warnings, Opts) of
                true ->
                    io:format("~p: warnings being treated as errors\n", [?MODULE]);
                false ->
                    ok
            end,
            comp_ret_err(St);
        false ->
            Warn = messages_per_file(Warn0),
            report_warnings(St#compile{warnings = Warn}),
            Ret1 =
                case
                    member(binary, Opts) andalso
                        not member(no_code_generation, Opts)
                of
                    true -> [Code];
                    false -> []
                end,
            Ret2 =
                case member(return_warnings, Opts) of
                    true -> Ret1 ++ [Warn];
                    false -> Ret1
                end,
            list_to_tuple([ok, Mod | Ret2])
    end.

comp_ret_err(#compile{warnings = Warn0, errors = Err0, options = Opts} = St) ->
    Warn = messages_per_file(Warn0),
    Err = messages_per_file(Err0),
    report_errors(St#compile{errors = Err}),
    report_warnings(St#compile{warnings = Warn}),
    case member(return_errors, Opts) of
        true -> {error, Err, Warn};
        false -> error
    end.

werror(#compile{options = Opts, warnings = Ws}) ->
    Ws =/= [] andalso member(warnings_as_errors, Opts).

%% messages_per_file([{File,[Message]}]) -> [{File,[Message]}]
messages_per_file(Ms) ->
    T = lists:sort([{File, M} || {File, Messages} <- Ms, M <- Messages]),
    PrioMs = [erlt_scan, erlt_epp, erlt_parse],
    {Prio0, Rest} = lists:mapfoldl(
        fun(M, A) ->
            lists:partition(
                fun
                    ({_, {_, Mod, _}}) -> Mod =:= M;
                    (_) -> false
                end,
                A
            )
        end,
        T,
        PrioMs
    ),
    Prio = lists:sort(
        fun({_, {L1, _, _}}, {_, {L2, _, _}}) -> L1 =< L2 end,
        lists:append(Prio0)
    ),
    flatmap(fun mpf/1, [Prio, Rest]).

mpf(Ms) ->
    [{File, [M || {F, M} <- Ms, F =:= File]} || File <- lists:usort([F || {F, _} <- Ms])].

select_passes([{pass, Mod} | Ps], Opts) ->
    F = fun(Code0, St) ->
        case catch Mod:module(Code0, St#compile.options) of
            {ok, Code} ->
                {ok, Code, St};
            {ok, Code, Ws} ->
                {ok, Code, St#compile{warnings = St#compile.warnings ++ Ws}};
            {error, Es} ->
                {error, St#compile{errors = St#compile.errors ++ Es}}
        end
    end,
    [{Mod, F} | select_passes(Ps, Opts)];
select_passes([{src_listing, Ext} | _], _Opts) ->
    [{listing, fun(Code, St) -> src_listing(Ext, Code, St) end}];
select_passes([{listing, Ext} | _], _Opts) ->
    [{listing, fun(Code, St) -> listing(Ext, Code, St) end}];
select_passes([{binary_listing, Ext} | _], _Opts) ->
    [{listing, fun(Code, St) -> binary_listing(Ext, Code, St) end}];
select_passes([done | _], _Opts) ->
    [];
select_passes([{done, Ext} | _], Opts) ->
    select_passes([{unless, binary, {listing, Ext}}], Opts);
select_passes([{iff, Flag, Pass} | Ps], Opts) ->
    select_cond(Flag, true, Pass, Ps, Opts);
select_passes([{unless, Flag, Pass} | Ps], Opts) ->
    select_cond(Flag, false, Pass, Ps, Opts);
select_passes([{_, Fun} = P | Ps], Opts) when is_function(Fun) ->
    [P | select_passes(Ps, Opts)];
select_passes([{delay, Passes0} | Ps], Opts) when is_list(Passes0) ->
    %% Delay evaluation of compiler options and which compiler passes to run.
    %% Since we must know beforehand whether a listing will be produced, we
    %% will go through the list of passes and evaluate all conditions that
    %% select a list pass.
    case select_list_passes(Passes0, Opts) of
        {done, Passes} ->
            [{delay, Passes}];
        {not_done, Passes} ->
            [{delay, Passes} | select_passes(Ps, Opts)]
    end;
select_passes([{_, Test, Fun} = P | Ps], Opts) when is_function(Test), is_function(Fun) ->
    [P | select_passes(Ps, Opts)];
select_passes([], _Opts) ->
    [];
select_passes([List | Ps], Opts) when is_list(List) ->
    case select_passes(List, Opts) of
        [] ->
            select_passes(Ps, Opts);
        Nested ->
            case last(Nested) of
                {listing, _Fun} -> Nested;
                _Other -> Nested ++ select_passes(Ps, Opts)
            end
    end.

select_cond(Flag, ShouldBe, Pass, Ps, Opts) ->
    ShouldNotBe = not ShouldBe,
    case member(Flag, Opts) of
        ShouldBe -> select_passes([Pass | Ps], Opts);
        ShouldNotBe -> select_passes(Ps, Opts)
    end.

%% select_list_passes([Pass], Opts) -> {done,[Pass]} | {not_done,[Pass]}
%%  Evaluate all conditions having to do with listings in the list of
%%  passes.
select_list_passes(Ps, Opts) ->
    select_list_passes_1(Ps, Opts, []).

select_list_passes_1([{iff, Flag, {listing, _} = Listing} | Ps], Opts, Acc) ->
    case member(Flag, Opts) of
        true -> {done, reverse(Acc, [Listing])};
        false -> select_list_passes_1(Ps, Opts, Acc)
    end;
select_list_passes_1([{iff, Flag, {done, Ext}} | Ps], Opts, Acc) ->
    case member(Flag, Opts) of
        false ->
            select_list_passes_1(Ps, Opts, Acc);
        true ->
            {done,
                case member(binary, Opts) of
                    false -> reverse(Acc, [{listing, Ext}]);
                    true -> reverse(Acc)
                end}
    end;
select_list_passes_1([{iff = Op, Flag, List0} | Ps], Opts, Acc) when is_list(List0) ->
    case select_list_passes(List0, Opts) of
        {done, List} -> {done, reverse(Acc) ++ List};
        {not_done, List} -> select_list_passes_1(Ps, Opts, [{Op, Flag, List} | Acc])
    end;
select_list_passes_1([{unless = Op, Flag, List0} | Ps], Opts, Acc) when is_list(List0) ->
    case select_list_passes(List0, Opts) of
        {done, List} -> {done, reverse(Acc) ++ List};
        {not_done, List} -> select_list_passes_1(Ps, Opts, [{Op, Flag, List} | Acc])
    end;
select_list_passes_1([P | Ps], Opts, Acc) ->
    select_list_passes_1(Ps, Opts, [P | Acc]);
select_list_passes_1([], _, Acc) ->
    {not_done, reverse(Acc)}.

%% Remove the target file so we don't have an old one if the compilation fail.
remove_file(Code, St) ->
    _ = file:delete(St#compile.ofile),
    {ok, Code, St}.

parse_module(Forms0, St0) ->
    parse_module(Forms0, St0, _EppMod2 = erlt_epp).

parse_module(_Code, St0, EppMod) ->
    case do_parse_module(utf8, St0, EppMod) of
        {ok, Fs, St1} ->
            {ok, Fs, St1#compile{original_forms = Fs}};
        {error, _} = Ret ->
            Ret;
        {invalid_unicode, File, Line} ->
            case do_parse_module(latin1, St0, EppMod) of
                {ok, Code, St} ->
                    Es = [{File, [{Line, ?MODULE, reparsing_invalid_unicode}]}],
                    {ok, Code, St#compile{warnings = Es ++ St#compile.warnings}};
                {error, St} ->
                    Es = [{File, [{Line, ?MODULE, reparsing_invalid_unicode}]}],
                    {error, St#compile{errors = Es ++ St#compile.errors}}
            end
    end.

collect_definitions(Code, #compile{build_dir = BuildDir} = St) ->
    AllDefFiles = filelib:wildcard(filename:join(BuildDir, "*" ++ ?DefFileSuffix)),
    Defs = lists:foldl(
        fun(File, Acc) ->
            {ok, EtfDefs} = file:read_file(File),
            erlt_defs:add_definitions(binary_to_term(EtfDefs), Acc)
        end,
        erlt_defs:new(),
        AllDefFiles
    ),
    {ok, Code, St#compile{global_defs = Defs}}.

output_declarations(Code, #compile{defs_file = FileName} = St) ->
    Output = term_to_binary(erlt_defs:normalise_definitions(Code)),
    file:write_file(FileName, Output, [sync]),
    OutputSterlang = term_to_binary(normalize_for_typecheck(Code)),
    file:write_file(FileName ++ ".etf", OutputSterlang, [sync]),
    {ok, Code, St#compile{has_written_defs_file = true}}.

output_etf(Code, #compile{etf_file = FileName} = St) ->
    Output = term_to_binary(normalize_for_typecheck(Code)),
    file:write_file(FileName, Output, [sync]),
    {ok, Code, St}.

erlt_track_vars(Code, St) ->
    VarState = erlt_vars:initialize_vars(Code),
    {ok, Code, St#compile{variable_state = VarState}}.

erlt_import(Code, St) ->
    Code1 = erlt_import:module(Code),
    {ok, Code1, St}.

erlt_lint(Code, St) ->
    do_erlt_lint(Code, St).

do_erlt_lint(Code, St) ->
    Opts = St#compile.options,
    case erlt_lint:module(Code, St#compile.ifile, St#compile.global_defs, Opts) of
        {ok, Ws} ->
            {ok, Code, St#compile{warnings = St#compile.warnings ++ Ws}};
        {error, Es, Ws} ->
            {error, St#compile{
                warnings = St#compile.warnings ++ Ws,
                errors = St#compile.errors ++ Es
            }}
    end.

do_parse_module(
    DefEncoding,
    #compile{ifile = File, options = Opts, dir = Dir} = St,
    EppMod
) ->
    SourceName0 = proplists:get_value(source, Opts, File),
    SourceName =
        case member(deterministic, Opts) of
            true -> filename:basename(SourceName0);
            false -> SourceName0
        end,
    R = EppMod:parse_file(File, [
        {includes, [".", Dir | inc_paths(Opts)]},
        {source_name, SourceName},
        {macros, pre_defs(Opts)},
        {default_encoding, DefEncoding},
        {location, {1, 1}},
        {scan_opts, [text]},
        extra
    ]),
    case R of
        {ok, Forms, Extra} ->
            Encoding = proplists:get_value(encoding, Extra),
            case find_invalid_unicode(Forms, File) of
                none ->
                    {ok, Forms, St#compile{encoding = Encoding}};
                {invalid_unicode, _, _} = Ret ->
                    case Encoding of
                        none ->
                            Ret;
                        _ ->
                            {ok, Forms, St#compile{encoding = Encoding}}
                    end
            end;
        {error, E} ->
            Es = [{St#compile.ifile, [{none, ?MODULE, {epp, E}}]}],
            {error, St#compile{errors = St#compile.errors ++ Es}}
    end.

find_invalid_unicode([H | T], File0) ->
    case H of
        {attribute, _, file, {File, _}} ->
            find_invalid_unicode(T, File);
        {error, {Line, file_io_server, invalid_unicode}} ->
            {invalid_unicode, File0, Line};
        _Other ->
            find_invalid_unicode(T, File0)
    end;
find_invalid_unicode([], _) ->
    none.

extract_options(Code0, #compile{options = Opt} = St) ->
    %% Extract compile options from code into options field.
    {ok, Code0, St#compile{options = Opt ++ compile_options(Code0)}}.

compile_options([{attribute, _L, compile, C} | Fs]) when is_list(C) ->
    C ++ compile_options(Fs);
compile_options([{attribute, _L, compile, C} | Fs]) ->
    [C | compile_options(Fs)];
compile_options([_F | Fs]) ->
    compile_options(Fs);
compile_options([]) ->
    [].

clean_parse_transforms(Fs) ->
    clean_parse_transforms_1(Fs, []).

clean_parse_transforms_1([{attribute, L, compile, C0} | Fs], Acc) when is_list(C0) ->
    C = lists:filter(
        fun
            ({parse_transform, _}) -> false;
            (_) -> true
        end,
        C0
    ),
    clean_parse_transforms_1(Fs, [{attribute, L, compile, C} | Acc]);
clean_parse_transforms_1([{attribute, _, compile, {parse_transform, _}} | Fs], Acc) ->
    clean_parse_transforms_1(Fs, Acc);
clean_parse_transforms_1([F | Fs], Acc) ->
    clean_parse_transforms_1(Fs, [F | Acc]);
clean_parse_transforms_1([], Acc) ->
    reverse(Acc).

transforms(Os) -> [M || {parse_transform, M} <- Os].

transform_module(Code0, #compile{options = Opt} = St) ->
    case transforms(Opt) of
        [] ->
            %% No parse transforms.
            {ok, Code0, St};
        Ts ->
            %% Remove parse_transform attributes from the abstract code to
            %% prevent parse transforms to be run more than once.
            Code =
                case is_makedep2_mode(Opt) of
                    true ->
                        % keep parse_transform attributes in place -- they are
                        % going to be retrieved by a later
                        % collect_erl1_compile_deps() pass
                        Code0;
                    false ->
                        clean_parse_transforms(Code0)
                end,
            foldl_transform(Ts, Code, St)
    end.

% defs files only depend on source_file
can_be_dep_of_defs_file(Filename) ->
    filename:extension(Filename) =:= ?SOURCE_FILE_EXTENSION.

get_compile_deps(Forms, St) ->
    AttrDeps = get_attr_deps(Forms, St),
    TypedDeps = get_erlt_deps(Forms, St),
    AttrDeps ++ TypedDeps.

get_erlt_deps(Forms, St0) ->
    RawDeps = erlt_deps:dt_deps(Forms),
    F = St0#compile.ifile,
    [resolve_defs_file({F, L}, M, St0) || {L, M} <- RawDeps].

erlt_typecheck(Code, St) ->
    run_sterlang(St),
    {ok, Code, St}.

run_sterlang(St) ->
    Erltc = escript:script_name(),
    BinDir = filename:dirname(Erltc),
    SterlangNative = filename:absname("sterlang"),
    SterlangJar = filename:absname("sterlang.jar"),
    IFile = filename:absname(St#compile.ifile),
    EtfFile = St#compile.etf_file,
    CheckCmd =
        case {filelib:is_regular(SterlangNative), filelib:is_regular(SterlangJar)} of
            {true, _} ->
                lists:append([
                    SterlangNative,
                    " ",
                    IFile,
                    " ",
                    EtfFile
                ]);
            {_, true} ->
                lists:append([
                    "java",
                    " ",
                    "-jar",
                    " ",
                    SterlangJar,
                    " ",
                    IFile,
                    " ",
                    EtfFile
                ]);
            _ ->
                undefined
        end,
    {ExitCode, Output} =
        case CheckCmd of
            undefined ->
                {0, ""};
            _ ->
                io:format("Running sterlang: ~p~n", [CheckCmd]),
                eunit_lib:command(CheckCmd, BinDir)
        end,
    case ExitCode of
        0 ->
            % TODO: check for warnings
            ok;
        _ ->
            ErrorFile = St#compile.filename,
            Error =
                {ErrorFile, [
                    {_ErrorLine = 1, ?MODULE, {sterlang, ExitCode, Output}}
                ]},

            % NOTE: the error thrown here will be caught by internal_comp() -> Run0
            throw({error, St#compile{errors = [Error]}})
    end.

erlt_to_erl1(Code, St) ->
    case erlt_struct:module(Code, St#compile.global_defs) of
        Code1 when is_list(Code1) ->
            case erlt_enum:module(Code1, St#compile.global_defs) of
                Code2 when is_list(Code2) ->
                    do_erlt_to_erl1(Code2, St)
            end
    end.

do_erlt_to_erl1(Code, St0) ->
    Transforms = [erlt_shape, erlt_modifiers, erlt_dots, erlt_pinning],
    case foldl_transform(Transforms, Code, St0) of
        {ok, Erl1Forms, St0} ->
            write_erl1(Erl1Forms, St0),
            {ok, Erl1Forms, St0};
        {error, St1} ->
            {error, St1}
    end.

foldl_transform([T | Ts], Code0, St) ->
    Name = "transform " ++ atom_to_list(T),
    case
        code:ensure_loaded(T) =:= {module, T} andalso
            erlang:function_exported(T, parse_transform, 2)
    of
        true ->
            Fun = fun(Code, S) ->
                T:parse_transform(Code, S#compile.options)
            end,
            Run =
                case member(time, St#compile.options) of
                    true ->
                        fun run_tc/3;
                    false ->
                        fun({_Name, F}, Code, S) ->
                            catch F(Code, S)
                        end
                end,
            case Run({Name, Fun}, Code0, St) of
                {error, Es, Ws} ->
                    {error, St#compile{
                        warnings = St#compile.warnings ++ Ws,
                        errors = St#compile.errors ++ Es
                    }};
                {'EXIT', R} ->
                    Es = [{St#compile.ifile, [{none, compile, {parse_transform, T, R}}]}],
                    {error, St#compile{errors = St#compile.errors ++ Es}};
                {warning, Forms, Ws} ->
                    foldl_transform(Ts, Forms, St#compile{
                        warnings = St#compile.warnings ++ Ws
                    });
                Forms when is_list(Forms) ->
                    foldl_transform(Ts, Forms, St)
            end;
        false ->
            Es = [{St#compile.ifile, [{none, compile, {undef_parse_transform, T}}]}],
            {error, St#compile{errors = St#compile.errors ++ Es}}
    end;
foldl_transform([], Code, St) ->
    {ok, Code, St}.

% @doc We write classic erlang files for humans to read.
% These aren't used later in the compiler.
write_erl1(Forms, #compile{build_dir = BuildDir, filename = SourceFilename}) ->
    ErlFilename = filename:join(BuildDir, filename:basename(SourceFilename, ".erlt") ++ ".erl"),
    {ok, File} = file:open(ErlFilename, [write, {encoding, utf8}]),
    try
        [
            begin
                Chars = erl_prettypr:format(Form),
                io:put_chars(File, Chars),
                io:nl(File),
                io:nl(File)
            end
            || Form <- Forms
        ],
        ok
    after
        file:close(ErlFilename)
    end.

save_binary(none, St) ->
    {ok, none, St};
save_binary(Code, #compile{module = Mod, ofile = Outfile, options = Opts} = St) ->
    %% Test that the module name and output file name match.
    case member(no_error_module_mismatch, Opts) of
        true ->
            save_binary_1(Code, St);
        false ->
            Base = filename:rootname(filename:basename(Outfile)),
            case atom_to_list(Mod) of
                Base ->
                    save_binary_1(Code, St);
                _ ->
                    Es = [{St#compile.ofile, [{none, ?MODULE, {module_name, Mod, Base}}]}],
                    {error, St#compile{errors = St#compile.errors ++ Es}}
            end
    end.

save_binary_1(Code, St) ->
    Ofile = St#compile.ofile,
    %Temp working file
    Tfile = tmpfile(Ofile),
    case write_binary(Tfile, Code, St) of
        ok ->
            case file:rename(Tfile, Ofile) of
                ok ->
                    {ok, none, St};
                {error, RenameError} ->
                    Es0 = [{Ofile, [{none, ?MODULE, {rename, Tfile, Ofile, RenameError}}]}],
                    Es =
                        case file:delete(Tfile) of
                            ok ->
                                Es0;
                            {error, DeleteError} ->
                                Es0 ++
                                    [
                                        {Ofile, [
                                            {none, ?MODULE, {delete_temp, Tfile, DeleteError}}
                                        ]}
                                    ]
                        end,
                    {error, St#compile{errors = St#compile.errors ++ Es}}
            end;
        {error, Error} ->
            Es = [{Tfile, [{none, compile, {write_error, Error}}]}],
            {error, St#compile{errors = St#compile.errors ++ Es}}
    end.

write_binary(Name, Bin, St) ->
    Opts =
        case member(compressed, St#compile.options) of
            true -> [compressed];
            false -> []
        end,
    case file:write_file(Name, Bin, Opts) of
        ok -> ok;
        {error, _} = Error -> Error
    end.

%% report_errors(State) -> ok
%% report_warnings(State) -> ok
report_errors(#compile{options = Opts, errors = Errors}) ->
    case member(report_errors, Opts) of
        true ->
            foreach(
                fun
                    ({{F, _L}, Eds}) -> list_errors(F, Eds, Opts);
                    ({F, Eds}) -> list_errors(F, Eds, Opts)
                end,
                Errors
            );
        false ->
            ok
    end.

report_warnings(#compile{options = Opts, warnings = Ws0}) ->
    Werror = member(warnings_as_errors, Opts),
    P =
        case Werror of
            true -> "";
            false -> "Warning: "
        end,
    ReportWerror = Werror andalso member(report_errors, Opts),
    case member(report_warnings, Opts) orelse ReportWerror of
        true ->
            Ws1 = flatmap(
                fun
                    ({{F, _L}, Eds}) -> format_message(F, P, Eds, Opts);
                    ({F, Eds}) -> format_message(F, P, Eds, Opts)
                end,
                Ws0
            ),
            Ws = lists:sort(Ws1),
            foreach(fun({_, Str}) -> io:put_chars(Str) end, Ws);
        false ->
            ok
    end.

format_message(F, P, [{none, Mod, E} | Es], Opts) ->
    M = {none, io_lib:format("~ts: ~s~ts\n", [F, P, Mod:format_error(E)])},
    [M | format_message(F, P, Es, Opts)];
format_message(F, P, [{Loc, Mod, E} | Es], Opts) ->
    StartLoc = erl_anno:location(Loc),
    EndLoc =
        case erlt_parse:get_end_location(Loc) of
            undefined -> StartLoc;
            Loc2 -> Loc2
        end,
    Src = quote_source(F, StartLoc, EndLoc, Opts),
    Msg = io_lib:format("~ts:~ts: ~s~ts\n~ts", [
        F,
        fmt_pos(StartLoc),
        P,
        Mod:format_error(E),
        Src
    ]),
    Pos =
        if
            is_integer(StartLoc) -> {StartLoc, 0};
            true -> StartLoc
        end,
    [{{F, Pos}, Msg} | format_message(F, P, Es, Opts)];
format_message(_, _, [], _Opts) ->
    [].

%% list_errors(File, ErrorDescriptors, Opts) -> ok
list_errors(F, [{none, Mod, E} | Es], Opts) ->
    io:fwrite("~ts: ~ts\n", [F, Mod:format_error(E)]),
    list_errors(F, Es, Opts);
list_errors(F, [{{{_, _} = StartLoc, {_, _} = EndLoc}, Mod, E} | Es], Opts) ->
    %% this is the location format used in the type analysis pass
    Src = quote_source(F, StartLoc, EndLoc, Opts),
    io:fwrite("~ts:~ts: ~ts\n~ts", [F, fmt_pos(StartLoc), Mod:format_error(E), Src]),
    list_errors(F, Es, Opts);
list_errors(F, [{Loc, Mod, E} | Es], Opts) ->
    StartLoc = erl_anno:location(Loc),
    EndLoc =
        case erlt_parse:get_end_location(Loc) of
            undefined -> StartLoc;
            Loc2 -> Loc2
        end,
    list_errors(F, [{{StartLoc, EndLoc}, Mod, E} | Es], Opts);
list_errors(_F, [], _Opts) ->
    ok.

fmt_pos({Line, Col}) ->
    io_lib:format("~w:~w", [Line, Col]);
fmt_pos(Line) ->
    io_lib:format("~w", [Line]).

erlfile(".", Base, Suffix) ->
    Base ++ Suffix;
erlfile(Dir, Base, Suffix) ->
    filename:join(Dir, Base ++ Suffix).

outfile(Base, Ext, Opts) when is_atom(Ext) ->
    outfile(Base, atom_to_list(Ext), Opts);
outfile(Base, Ext, Opts) ->
    Obase =
        case keyfind(outdir, 1, Opts) of
            {outdir, Odir} ->
                filename:join(Odir, Base);
            % Not found or bad format
            _Other ->
                Base
        end,
    Obase ++ "." ++ Ext.

objfile(Base, St) ->
    outfile(Base, "beam", St#compile.options).

tmpfile(Ofile) ->
    reverse([$# | tl(reverse(Ofile))]).

quote_source(File, StartLoc, EndLoc, Opts) ->
    case proplists:get_bool(brief, Opts) of
        true -> "";
        false -> quote_source_1(File, StartLoc, EndLoc)
    end.

quote_source_1(File, Line, Loc2) when is_integer(Line) ->
    quote_source_1(File, {Line, 1}, Loc2);
quote_source_1(File, Loc1, Line) when is_integer(Line) ->
    quote_source_1(File, Loc1, {Line, -1});
quote_source_1(File, {StartLine, StartCol}, {EndLine, EndCol}) ->
    case file:read_file(File) of
        {ok, Bin} ->
            Ctx =
                if
                    StartLine =:= EndLine -> 0;
                    true -> 1
                end,
            case seek_line(Bin, 1, StartLine - Ctx) of
                {ok, Bin1} ->
                    quote_source_2(Bin1, StartLine, StartCol, EndLine, EndCol, Ctx);
                error ->
                    ""
            end;
        {error, _} ->
            ""
    end.

quote_source_2(Bin, StartLine, StartCol, EndLine, EndCol, Ctx) ->
    case take_lines(Bin, StartLine - Ctx, EndLine + Ctx) of
        [] ->
            "";
        Lines ->
            Lines1 =
                case length(Lines) =< (4 + Ctx) of
                    true ->
                        Lines;
                    false ->
                        %% before = context + start line + following line
                        %% after = end line + context
                        %% (total lines: 3 + 1 + context)
                        Before = lists:sublist(Lines, 2 + Ctx),
                        After = lists:reverse(
                            lists:sublist(lists:reverse(Lines), 1 + Ctx)
                        ),
                        Before ++ [{0, "..."}] ++ After
                end,
            Lines2 = decorate(Lines1, StartLine, StartCol, EndLine, EndCol),
            [[fmt_line(L, Text) || {L, Text} <- Lines2], $\n]
    end.

line_prefix() ->
    "% ".

fmt_line(L, Text) ->
    io_lib:format("~ts~4.ts| ~ts\n", [line_prefix(), line_to_txt(L), Text]).

line_to_txt(0) -> "";
line_to_txt(L) -> integer_to_list(L).

decorate([{Line, Text} = L | Ls], StartLine, StartCol, EndLine, EndCol) when
    Line =:= StartLine, EndLine =:= StartLine
->
    %% start and end on same line
    S = underline(Text, StartCol, EndCol),
    decorate(S, L, Ls, StartLine, StartCol, EndLine, EndCol);
decorate([{Line, Text} = L | Ls], StartLine, StartCol, EndLine, EndCol) when Line =:= StartLine ->
    %% start with end on separate line
    S = underline(Text, StartCol, string:length(Text) + 1),
    decorate(S, L, Ls, StartLine, StartCol, EndLine, EndCol);
%% decorate([{Line, Text}=L|Ls], StartLine, StartCol, EndLine, EndCol)
%%   when Line =:= EndLine ->
%%     S = underline(Text, EndCol,EndCol),  % just mark end
%%     decorate(S, L, Ls, StartLine, StartCol, EndLine, EndCol);
decorate([{_Line, _Text} = L | Ls], StartLine, StartCol, EndLine, EndCol) ->
    [L | decorate(Ls, StartLine, StartCol, EndLine, EndCol)];
decorate([], _StartLine, _StartCol, _EndLine, _EndCol) ->
    [].

%% don't produce empty decoration lines
decorate("", L, Ls, StartLine, StartCol, EndLine, EndCol) ->
    [L | decorate(Ls, StartLine, StartCol, EndLine, EndCol)];
decorate(Text, L, Ls, StartLine, StartCol, EndLine, EndCol) ->
    [L, {0, Text} | decorate(Ls, StartLine, StartCol, EndLine, EndCol)].

%% End typically points to the first position after the actual region.
%% If End = Start, we adjust it to Start+1 to mark at least one character
%% TODO: colorization option
underline(_Text, Start, End) when End < Start ->
    % no underlining at all if end column is unknown
    "";
underline(Text, Start, Start) ->
    underline(Text, Start, Start + 1);
underline(Text, Start, End) ->
    underline(Text, Start, End, 1).

underline(<<$\t, Text/binary>>, Start, End, N) when N < Start ->
    [$\t | underline(Text, Start, End, N + 1)];
underline(<<_, Text/binary>>, Start, End, N) when N < Start ->
    [$\s | underline(Text, Start, End, N + 1)];
underline(_Text, _Start, End, N) ->
    underline_1(N, End).

underline_1(N, End) when N < End ->
    [$^ | underline_1(N + 1, End)];
underline_1(_N, _End) ->
    "".

seek_line(Bin, L, L) -> {ok, Bin};
seek_line(<<$\n, Rest/binary>>, N, L) -> seek_line(Rest, N + 1, L);
seek_line(<<$\r, $\n, Rest/binary>>, N, L) -> seek_line(Rest, N + 1, L);
seek_line(<<_, Rest/binary>>, N, L) -> seek_line(Rest, N, L);
seek_line(<<>>, _, _) -> error.

take_lines(<<>>, _Here, _To) ->
    [];
take_lines(Bin, Here, To) when Here =< To ->
    {Line, Rest} = take_line(Bin, <<>>),
    [{Here, Line} | take_lines(Rest, Here + 1, To)];
take_lines(_Bin, _Here, _To) ->
    [].

take_line(<<$\n, Rest/binary>>, Ack) ->
    {Ack, Rest};
take_line(<<$\r, $\n, Rest/binary>>, Ack) ->
    {Ack, Rest};
take_line(<<B, Rest/binary>>, Ack) ->
    take_line(Rest, <<Ack/binary, B>>);
take_line(<<>>, Ack) ->
    {Ack, <<>>}.

%% pre_defs(Options)
%% inc_paths(Options)
%%  Extract the predefined macros and include paths from the option list.
pre_defs([{d, M, V} | Opts]) ->
    [{M, V} | pre_defs(Opts)];
pre_defs([{d, M} | Opts]) ->
    [M | pre_defs(Opts)];
pre_defs([_ | Opts]) ->
    pre_defs(Opts);
pre_defs([]) ->
    [].

inc_paths(Opts) ->
    [P || {i, P} <- Opts, is_list(P)].

binary_listing(Ext, Code0, St0) ->
    Code =
        case Ext of
            "defs" -> erlt_defs:normalise_definitions(Code0);
            _ -> Code0
        end,
    Code1 = normalize_for_typecheck(Code),
    Write = fun(Out, Forms) ->
        ok = io:setopts(Out, [{encoding, latin1}]),
        file:write(Out, term_to_binary(Forms))
    end,
    St = St0#compile{encoding = none},
    listing(Write, Ext, Code1, St).

src_listing(Ext, Code, St) ->
    listing(
        fun
            (Lf, {_Mod, _Exp, Fs}) -> do_src_listing(Lf, Fs);
            (Lf, Fs) -> do_src_listing(Lf, Fs)
        end,
        Ext,
        Code,
        St
    ).

do_src_listing(Lf, Fs) ->
    Opts = [lists:keyfind(encoding, 1, io:getopts(Lf))],
    foreach(fun(F) -> io:put_chars(Lf, [erlt_pp:form(F, Opts), "\n"]) end, Fs).

listing(Ext, Code, St0) ->
    St = St0#compile{encoding = none},
    listing(fun(Lf, Fs) -> beam_listing:module(Lf, Fs) end, Ext, Code, St).

listing(LFun, Ext, Code, St) ->
    Lfile = outfile(St#compile.base, Ext, St#compile.options),
    case file:open(Lfile, [write, delayed_write]) of
        {ok, Lf} ->
            Code = restore_expanded_types(Ext, Code),
            output_encoding(Lf, St),
            LFun(Lf, Code),
            ok = file:close(Lf),
            {ok, Code, St};
        {error, Error} ->
            Es = [{Lfile, [{none, compile, {write_error, Error}}]}],
            {error, St#compile{errors = St#compile.errors ++ Es}}
    end.

output_encoding(F, #compile{encoding = none}) ->
    ok = io:setopts(F, [{encoding, epp:default_encoding()}]);
output_encoding(F, #compile{encoding = Encoding}) ->
    ok = io:setopts(F, [{encoding, Encoding}]),
    ok = io:fwrite(F, <<"%% ~s\n">>, [epp:encoding_to_string(Encoding)]).

restore_expanded_types("E", {M, I, Fs0}) ->
    Fs = restore_expand_module(Fs0),
    {M, I, Fs};
restore_expanded_types(_Ext, Code) ->
    Code.

restore_expand_module([{attribute, Line, type, [Type]} | Fs]) ->
    [{attribute, Line, type, Type} | restore_expand_module(Fs)];
restore_expand_module([{attribute, Line, opaque, [Type]} | Fs]) ->
    [{attribute, Line, opaque, Type} | restore_expand_module(Fs)];
restore_expand_module([{attribute, Line, spec, [Arg]} | Fs]) ->
    [{attribute, Line, spec, Arg} | restore_expand_module(Fs)];
restore_expand_module([{attribute, Line, callback, [Arg]} | Fs]) ->
    [{attribute, Line, callback, Arg} | restore_expand_module(Fs)];
restore_expand_module([F | Fs]) ->
    [F | restore_expand_module(Fs)];
restore_expand_module([]) ->
    [].

%% Turn annotation fields into a uniform format for export to the type checker
normalize_for_typecheck(Forms) ->
    erlt_ast:map_anno(Forms, fun normalize_loc/1).

%% returns {{StartLine,StartColumn},{EndLine,EndColumn}}
normalize_loc(Line) when is_integer(Line) ->
    % only start line known
    {{Line, 0}, {Line, 0}};
normalize_loc({Line, Col} = Loc) when is_integer(Line), is_integer(Col) ->
    % only start position known
    {Loc, Loc};
normalize_loc(As) when is_list(As) ->
    Start = loc(erl_anno:location(As)),
    End =
        case erlt_parse:get_end_location(As) of
            undefined -> Start;
            Loc -> loc(Loc)
        end,
    {Start, End};
normalize_loc(_Other) ->
    % unknown position
    {{0, 0}, {0, 0}}.

loc({Line, Col} = Loc) when is_integer(Line), is_integer(Col) ->
    Loc;
loc(Line) when is_integer(Line) ->
    {Line, 0};
loc(_Other) ->
    {0, 0}.
