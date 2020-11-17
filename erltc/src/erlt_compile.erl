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

%% our tweaked copy of the standard compile.erl module
-define(OTP_COMPILE, erlt_otp_compile).

%% High-level interface.
%%
%% TODO: implement forms() and potentialy other APIs supported by the standard compile.erl
-export([file/2]).

%% erltc interface.
-export([compile/2]).

-export([format_error/1]).

%% Needed for ping-pong with erlt_otp_compile for error reporting
-export([comp_ret_ok/2, comp_ret_err/1]).

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

%% The compile state record, with our added fields
-include("erlt_compile.hrl").

-define(pass(P), {P, fun P/2}).

-define(DefFileSuffix, ".defs").
-define(EtfFileSuffix, ".etf").

%% NOTE: this is a wrapper around file/2 (both are exported)
% called by erltc.erl
compile(File0, Options) ->
    File = shorten_filename(File0),
    case file(File, Options) of
        {ok, _Mod} -> ok;
        _Other -> error
    end.

shorten_filename(Name) ->
    ?OTP_COMPILE:shorten_filename(Name).

%% NOTE: this is a variant of compile.erl do_compile() for files only
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

%% NOTE: this is an unfolded variant of compile.erl internal() for files only
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
    EnvCompilerOptions = ?OTP_COMPILE:env_compiler_options(),
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
                        ?pass(output_declarations_sterlang),
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
                        {iff, 'etf', [
                            {listing, fun(Code, St) ->
                                binary_listing("etf", Code, St)
                            end},
                            done
                        ]},
                        {iff, 'defs', [
                            {listing, fun(Code, St) ->
                                binary_listing("defs", Code, St)
                            end},
                            done
                        ]},
                        {unless, 'P', {unless, 'E', ?pass(erlt_typecheck)}},
                        ?pass(erlt_import),
                        ?pass(erlt_to_erl1),
                        {iff, 'A', {src_listing, "A"}},
                        ?pass(transform_module),
                        ?pass(compile_erl1_forms),
                        ?pass(maybe_save_binary)
                    ]
        end,
    Passes1 = ?OTP_COMPILE:select_passes(Passes, Options),

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
        ?pass(set_defs_file),
        ?pass(set_etf_file),
        ?pass(parse_module),
        ?pass(save_original_forms),
        ?pass(check_parse_errors),
        ?pass(extract_options),
        ?pass(erlt_lint_types),
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
    Ret = ?OTP_COMPILE:noenv_forms(Forms, [
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
            ?OTP_COMPILE:save_binary(Code, St);
        false ->
            {ok, none, St}
    end.

format_error({sterlang, Output}) ->
    io_lib:format("Type checking: ~n~s", [Output]);
format_error({module_dependency, defs_dependency, Mod}) ->
    io_lib:format("can't find ~s.~s", [Mod, ?DefFileSuffix]);
format_error({module_dependency, ModuleDepType, Mod}) ->
    io_lib:format("can't find ~s from -~s(~s)", [Mod, ModuleDepType, Mod]);
format_error(X) ->
    % TODO: copy formatters for locally-generated errors from copy-pasted code
    % here to avoid problems with error format compatibility in the future
    ?OTP_COMPILE:format_error(X).

%% =====================================================================
%% Copied from erlt_otp_compile to be able to route the warnings to a
%% file for the language server
comp_ret_ok(Code, #compile{warnings = Warn0, module = Mod, options = Opts} = St) ->
    case ?OTP_COMPILE:werror(St) of
        true ->
            case member(report_warnings, Opts) of
                true ->
                    io:format(
                        "~p: warnings being treated as errors\n",
                        [?MODULE]
                    );
                false ->
                    ok
            end,
            ?MODULE:comp_ret_err(St);
        false ->
            Warn = ?OTP_COMPILE:messages_per_file(Warn0),
            ?OTP_COMPILE:report_warnings(St#compile{warnings = Warn}),
            erlt_language_server:maybe_output_ls_diagnostics(Warn, [], St),
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

%% Copied from erlt_otp_compile to be able to route the warnings to a
%% file for the language server
comp_ret_err(#compile{warnings = Warn0, errors = Err0, options = Opts} = St) ->
    Warn = ?OTP_COMPILE:messages_per_file(Warn0),
    Err = ?OTP_COMPILE:messages_per_file(Err0),
    ?OTP_COMPILE:report_errors(St#compile{errors = Err}),
    ?OTP_COMPILE:report_warnings(St#compile{warnings = Warn}),
    erlt_language_server:maybe_output_ls_diagnostics(Warn, Err, St),
    case member(return_errors, Opts) of
        true -> {error, Err, Warn};
        false -> error
    end.

%% =====================================================================

set_defs_file(Code, #compile{build_dir = BuildDir, base = Base} = St) ->
    {ok, Code, St#compile{
        defs_file = filename:join(
            BuildDir,
            Base ++ ?DefFileSuffix
        )
    }}.

set_etf_file(Code, #compile{build_dir = BuildDir, base = Base} = St) ->
    {ok, Code, St#compile{
        etf_file = filename:join(
            BuildDir,
            Base ++ ?EtfFileSuffix
        )
    }}.

internal_comp(Passes, Code, File, Suffix, St) ->
    ?OTP_COMPILE:internal_comp(Passes, Code, File, Suffix, St).

remove_file(Code, St) ->
    ?OTP_COMPILE:remove_file(Code, St).

parse_module(Code, St) ->
    ?OTP_COMPILE:parse_module(Code, St).

save_original_forms(Code, St) ->
    {ok, Code, St#compile{original_forms = Code}}.

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

output_declarations_sterlang(Code, #compile{defs_file = FileName} = St) ->
    Defs = [Def || {attribute, _, _, _} = Def <- Code],
    OutputSterlang = term_to_binary(normalize_for_typecheck(Defs)),
    file:write_file(FileName ++ ".etf", OutputSterlang, [sync]),
    {ok, Code, St}.

output_declarations(Code, #compile{defs_file = FileName} = St) ->
    Output = term_to_binary(erlt_defs:normalise_definitions(Code)),
    file:write_file(FileName, Output, [sync]),
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

erlt_lint_types(Code, St) ->
    case erlt_lint_types:module(Code, St#compile.ifile) of
        {ok, Ws} ->
            {ok, Code, St#compile{warnings = St#compile.warnings ++ Ws}};
        {error, Es, Ws} ->
            {error, St#compile{
                warnings = St#compile.warnings ++ Ws,
                errors = St#compile.errors ++ Es
            }}
    end.

extract_options(Code0, #compile{options = Opt} = St) ->
    %% Extract compile options from code into options field.
    {ok, Code0, St#compile{options = Opt ++ ?OTP_COMPILE:compile_options(Code0)}}.

%% NOTE: many differences from compile.erl version
transform_module(Code0, #compile{options = Opt} = St) ->
    case ?OTP_COMPILE:transforms(Opt) of
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
                        ?OTP_COMPILE:clean_parse_transforms(Code0)
                end,
            ?OTP_COMPILE:foldl_transform(Ts, Code, St)
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

-type range() ::
    {{Line1 :: integer(), Column1 :: integer()}, {Line2 :: integer(), Column2 :: integer()}}
    | undefined.

-record(sterlang_result, {
    result :: {ok} | {error, range(), string()},
    mode :: native | jar | daemon | skipped,
    erltc_time :: non_neg_integer(),
    st_time :: non_neg_integer()
}).

erlt_typecheck(Code, St) ->
    R = #sterlang_result{result = Res} = run_sterlang(St),
    member(verbose, St#compile.options) andalso log_sterlang_result(R, St),

    %% Provide an example of what a hover entry should look like to be
    %% exposed to erlang_ls.  In future this could potentially be
    %% 'MarkupContent' as defined in the LSP spec
    Hovers = [#{ range =>#{ from => {1, 1}, to => {2, 1}}
               , kind => hover
               , id => undefined
               , data => <<"hover text woohoo5">>}], %% in POI format

    case Res of
        {ok} ->
            {ok, Code, St#compile{hover = Hovers}};
        {error, Range, ErrMessage} ->
            Location =
                case Range of
                    {Loc1, Loc2} -> [{location, Loc1}, {end_location, Loc2}];
                    _ -> none
                end,
            Error = {St#compile.ifile, [{Location, ?MODULE, {sterlang, ErrMessage}}]},
            Errors = St#compile.errors ++ [Error],
            {error, St#compile{errors = Errors, hover = Hovers}}
    end.

-spec log_sterlang_result(#sterlang_result{}, #compile{}) -> true.
log_sterlang_result(#sterlang_result{mode = M, result = R, erltc_time = T1, st_time = T2}, St) ->
    io:format("===> erltc+sterlang: ~tp~n", [{St#compile.ifile, M, element(1, R), T1, T2}]),
    true.

-spec run_sterlang(#compile{}) -> #sterlang_result{}.
run_sterlang(St) ->
    Start = erlang:monotonic_time('millisecond'),
    EtfFile = St#compile.etf_file,
    {CmdMode, CheckCmd} =
        case {filelib:is_regular("sterlang"), filelib:is_regular("sterlang.jar")} of
            {true, _} ->
                {native, lists:append(["./sterlang ", EtfFile])};
            {_, true} ->
                {jar, lists:append(["java -jar sterlang.jar ", EtfFile])};
            _ ->
                {undefined, undefined}
        end,
    {Mode, Result, SterlangTime} =
        case {CmdMode, is_alive()} of
            {undefined, false} ->
                {skipped, {ok}, undefined};
            {undefined, true} ->
                Ref = erlang:monitor(process, {api, sterlangd@localhost}),
                {api, sterlangd@localhost} ! {check, self(), Ref, EtfFile},
                Res1 =
                    receive
                        {'DOWN', Ref, _, _, noconnection} ->
                            {daemon, {error, undefined, "No connection to sterlangd@localhost"},
                                undefined};
                        {'DOWN', Ref, _, _, Reason} ->
                            Error = io_lib:format(
                                "Connection to sterlangd@localhost failed. Reason: ~p",
                                [Reason]
                            ),
                            {daemon, {error, undefined, Error}, undefined};
                        {Ref, Res, StTime} ->
                            {daemon, Res, StTime}
                    end,
                erlang:demonitor(Ref, [flush]),
                Res1;
            _ ->
                case eunit_lib:command(CheckCmd) of
                    {0, StdOutOutput} ->
                        {Res, StTime} = binary_to_term(list_to_binary(StdOutOutput)),
                        {CmdMode, Res, StTime};
                    {_, Output} ->
                        {CmdMode, {error, undefined, Output}, undefined}
                end
        end,
    End = erlang:monotonic_time('millisecond'),
    Time = End - Start,
    #sterlang_result{mode = Mode, result = Result, erltc_time = Time, st_time = SterlangTime}.

erlt_to_erl1(Code, St) ->
    case erlt_struct:module(Code, St#compile.global_defs) of
        Code1 when is_list(Code1) ->
            case erlt_enum:module(Code1, St#compile.global_defs) of
                Code2 when is_list(Code2) ->
                    do_erlt_to_erl1(Code2, St)
            end
    end.

do_erlt_to_erl1(Code, St0) ->
    Transforms = [erlt_shape, erlt_modifiers, erlt_pinning, erlt_try, erlt_atom],
    case ?OTP_COMPILE:foldl_transform(Transforms, Code, St0) of
        {ok, Erl1Forms, St0} ->
            write_erl1(Erl1Forms, St0),
            {ok, Erl1Forms, St0};
        {error, St1} ->
            {error, St1}
    end.

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
    ?OTP_COMPILE:listing(Write, Ext, Code1, St).

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
