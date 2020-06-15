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

%% Purpose: Run the Erlang2 compiler.
-module(erl2_compile).

% TODO: this prevents unused function warnings for compiler passes
-compile(export_all).
-compile(nowarn_export_all).

%% High-level interface.
%%
%% TODO: implement forms() and potentialy other APIs supported by the standard compile.erl
-export([file/2]).

%% Erl2c interface.
-export([compile/2]).


%
% NOTE: code below is based on copy-pasted pieces from erlang/lib/compiler-7.3/src/compile.erl (R21)
%
-import(lists, [member/2,reverse/1,reverse/2,keyfind/3,last/1,
		map/2,flatmap/2,foreach/2,foldr/3,any/2]).


-define(STDERR, standard_error).		%Macro to avoid misspellings.


-type err_warn_info() :: tuple().
-type option() :: atom() | {atom(), term()} | {'d', atom(), term()}.

% type of compile-time .erl dependency
%
% here, 'file' typically means file included by -include or -include_lib, but
% in theory, it could be something injected by a parse transform
-type compile_dep_type() :: file | behavior | parse_transform | core_transform.
-type compile_dep() :: {compile_dep_type(), file:filename()}.

% NOTE: slimmed down version of the original compile state
-record(compile, {filename="" :: file:filename(),
                  dir=""      :: file:filename(),
		  base=""     :: file:filename(),
		  ifile=""    :: file:filename(),
		  ofile=""    :: file:filename(),
		  module=[]   :: module() | [],
		  options=[]  :: [option()],  %Options for compilation
                  encoding=none :: none | erl2_epp:source_encoding(),
		  errors=[]     :: [err_warn_info()],
		  warnings=[]   :: [err_warn_info()],
                  compile_deps=[] :: [compile_dep()],

                  build_dir :: undefined | file:filename(),
                  % indicator of Erlang language flavor; valid combinations are
                  %
                  %    []                  -- erl1
                  %    [erl2, dt], [erl2]  -- dynamically typed erl2
                  %    [erl2, st]          -- statically typed erl2
                  %    [erl2, ffi]         -- ffi erl2
                  %    [erl2, specs]       -- specs for a module which is somewhere else
                  lang=[] :: [erl2 | st | dt | ffi | specs]

}).

-define(pass(P), {P,fun P/2}).


% called by erl2c.erl
%
% XXX: move to erl2c.erl? as it is unlikely to be useful for anything else
compile(File0, Options) ->
    File = shorten_filename(File0),
    case file(File, Options) of
	{ok,_Mod} -> ok;
	_Other -> error
    end.


shorten_filename(Name0) ->
    {ok,Cwd} = file:get_cwd(),
    case lists:prefix(Cwd, Name0) of
	false -> Name0;
	true ->
	    case lists:nthtail(length(Cwd), Name0) of
		"/"++N -> N;
		N -> N
	    end
    end.


file(File, Options) ->
    try
        do_file(File, Options)
    catch
        Class:Error:Stk ->
            io:format(?STDERR, "internal error while compiling ~s.erl:~n\t~p~nStacktrace:~n~p~n", [
                    File, {Class, Error}, Stk
            ]),
            error
    end.


do_file(File, Options0) ->
    %io:format("Options: ~tp\n", [Options0]),

    % TODO: make sure that "ERL_COMPILER_OPTIONS" environment variable if
    % defined, does not specify parse_transforms. Otherwise, we are risking
    % running parse_transforms twice. Plus, we are going to be restricting how
    % parse transforms can be specified anyway.
    EnvCompilerOptions = compile:env_compiler_options(),
    Options1 = Options0 ++ EnvCompilerOptions,
    Options2 = fix_compile_options(Options1),

    % TODO, XXX: derive makedep_output from it?
    BuildDir =
        case keyfind(build_dir, 1, Options0) of
            {build_dir, Dir} -> Dir;
            false ->
                "build"
        end,

    St0 = #compile{
        options=Options2,
        build_dir=BuildDir
    },

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

    Passes =
        case CompileMode of
            makedep2 ->
                % 'erlc -M2' -- a new dependency scanner, alternative to 'erlc -M' aka makedep; among other things, it
                % allows to establish the order in which .erl files should be compiled
                %
                % this mode exists mainly for testing the new dependency scanner
                TransformPasses = [ ?pass(transform_module) || member(makedep2_run_parse_transforms, Options0) ],
                [
                    ?pass(parse_module),
                    ?pass(check_parse_errors),
                    ?pass(extract_options),
                    ?pass(erl2_module_record),
                    ?pass(erl2_lint),
                    ?pass(erl2_expand),

                    ?pass(collect_erl2_compile_deps),
                    ?pass(erl2_to_erl1)
                ]
                ++
                TransformPasses
                ++
                [
                    ?pass(collect_erl1_compile_deps),
                    ?pass(output_compile_deps)
                ];
            build_scan ->
                % build scan phase -- generate depfiles; later, we are also going to
                % extract declarations from parsed module, and cache the parse tree
                [
                    ?pass(parse_module),
                    ?pass(check_parse_errors),
                    ?pass(extract_options),
                    ?pass(erl2_module_record),
                    ?pass(erl2_lint),
                    ?pass(erl2_expand),

                    ?pass(collect_erl2_compile_deps),
                    ?pass(erl2_to_erl1),

                    ?pass(collect_erl1_compile_deps),
                    ?pass(output_compile_deps)
                ];
            build_compile ->
                % build compile phase -- later, we are going to use this for optimizing
                % compilation by recovering information cached during the "scan" phase
                [
                    % TODO: do not remove the output file unless we know save_binary() is going to run
                    ?pass(remove_file),
                    ?pass(parse_module),
                    ?pass(check_parse_errors),
                    ?pass(extract_options),
                    ?pass(erl2_module_record),
                    ?pass(erl2_lint),
                    ?pass(erl2_expand),

                    ?pass(erl2_typecheck),
                    ?pass(erl2_to_erl1),

                    ?pass(transform_module),
                    ?pass(compile_erl1_forms),
                    ?pass(maybe_save_binary)
                ];
            compile ->
                % normal .erl compilation -- for erl1, should be identical to erlc behavior
                [
                    % TODO: do not remove the output file unless we know save_binary() is going to run
                    ?pass(remove_file),
                    ?pass(parse_module),
                    ?pass(check_parse_errors),
                    ?pass(extract_options),
                    ?pass(erl2_module_record),
                    ?pass(erl2_lint),
                    ?pass(erl2_expand),
                    {iff,'B',{src_listing,"B"}},

                    {unless,'P',{unless,'E',?pass(erl2_typecheck)}},
                    ?pass(erl2_to_erl1),

                    ?pass(transform_module),
                    ?pass(compile_erl1_forms),
                    ?pass(maybe_save_binary)
                ]
        end,
    Passes1 = select_passes(Passes, St0#compile.options),

    internal_comp(Passes1, _Code0 = unused, File, _Suffix = ".erl", St0).


is_makedep2_mode(Options) ->
    member(makedep2, Options) andalso member(makedep, Options).


% TODO: add a mode for printing deps in JSON format (-MJSON ?)
output_compile_deps(_Forms, St) ->
    IsM2Compat = member(makedep2_compat, St#compile.options),
    TargetBeam = shorten_filename(St#compile.ofile),

    Deps1 = St#compile.compile_deps,
    Deps2 =
        case member(makedep2_only_relative, St#compile.options) of
            false -> Deps1;
            true ->
                % keep dependencies with relative file paths
                [X || X = {_, Filename} <- Deps1, filename:pathtype(Filename) =:= relative]
        end,
    Deps =
        case IsM2Compat of
            false -> Deps2;
            true ->
                % excluding .beam dependencies in -M compatibility mode (which
                % should result in output identical to -M)
                [X || X = {file, _} <- Deps2]
        end,

    DepsFilenames = deps_to_unique_filenames(Deps),
    RuleCode = gen_make_rule_compat(TargetBeam, DepsFilenames),

    PhonyRulesCode =
        case proplists:get_value(makedep_phony, St#compile.options) of
               false -> "";
               _ ->
                   [gen_make_phony_rule(St#compile.ifile, X) || X <- DepsFilenames]
        end,

    % adding the rule to rebuild the depfile itself
    MakedepRuleCode =
        case proplists:get_value(makedep_output, St#compile.options) of
            undefined -> "";
            _ when IsM2Compat ->
                % excluding this rule generation in -M compatibility mode
                % (which should result in output identical to -M)
                "";
            MakedepFilename ->
                % NOTE: only including parse transforms as dependency graph
                % dependencies, if they were actually run
                IncludeParseTransforms = member(makedep2_run_parse_transforms, St#compile.options),
                [
                    "\n",
                    gen_depfile_make_rule(MakedepFilename, Deps, IncludeParseTransforms)
                ]
        end,

    Code = [
        RuleCode,
        MakedepRuleCode,
        PhonyRulesCode
    ],

    %% Write the Make rules to the selected output.
    %% If no output is specified, the default is to write rules to stdout
    Output0 = case proplists:get_value(makedep_output, St#compile.options) of
		  undefined ->
                      standard_io;
		  O ->
		      O
	      end,

    %% If the caller specified an io_device(), there's nothing to do. If he
    %% specified a filename, we must create it. Furthermore, this created file
    %% must be closed before returning.
    Ret = case Output0 of
	      _ when is_list(Output0) ->
		  case file:delete(Output0) of
		      Ret2 when Ret2 =:= ok; Ret2 =:= {error,enoent} ->
			  case file:open(Output0, [write]) of
			      {ok,IODev} ->
				  {ok,IODev,true};
			      {error,Reason2} ->
				  {error,open,Reason2}
			  end;
		      {error,Reason1} ->
			  {error,delete,Reason1}
		  end;
	      _ ->
		  {ok,Output0,false}
	  end,

    case Ret of
	{ok,Output1,CloseOutput} ->
	    try
		%% Write the Makefile.
		io:fwrite(Output1, "~ts", [Code]),
		%% Close the file if relevant.
		if
		    CloseOutput -> ok = file:close(Output1);
		    true -> ok
		end,
		{ok,Code,St}
	    catch
		error:_ ->
		    %% Couldn't write to output Makefile.
		    Err = {St#compile.ifile,[{none,?MODULE,write_error}]},
		    {error,St#compile{errors=St#compile.errors++[Err]}}
	    end;
	{error,open,Reason} ->
	    %% Couldn't open output Makefile.
	    Err = {St#compile.ifile,[{none,?MODULE,{open,Reason}}]},
	    {error,St#compile{errors=St#compile.errors++[Err]}};
	{error,delete,Reason} ->
	    %% Couldn't open output Makefile.
	    Err = {St#compile.ifile,[{none,?MODULE,{delete,Output0,Reason}}]},
	    {error,St#compile{errors=St#compile.errors++[Err]}}
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


% same as gen_make_rule(), but make sure the line are no longer than 78
% characters. This behavior is compatible with erlc (WHY?!)
gen_make_rule_compat(_Target, _DepsFilenames = []) ->
    [];
gen_make_rule_compat(Target, DepsFilenames) ->
    FirstLine = Target ++ ":",
    gen_make_rule_compat(DepsFilenames, FirstLine, _Acc = []).


gen_make_rule_compat([], CurrentLine, Acc) ->
    LastLine = CurrentLine ++ "\n",
    lists:reverse([LastLine | Acc]);

gen_make_rule_compat([NextFilename|RestFilenames], CurrentLine, Acc) ->
    NewCurrentLine = CurrentLine ++ " " ++ NextFilename,
    case length(NewCurrentLine) > 76 of
        false ->
            gen_make_rule_compat(RestFilenames, NewCurrentLine, Acc);
        true ->
            NewAcc = [CurrentLine ++ " \\\n" | Acc],
            NewCurrentLine1 = "  " ++ NextFilename,
            gen_make_rule_compat(RestFilenames, NewCurrentLine1, NewAcc)
    end.


gen_make_phony_rule(Ifile, Filename) ->
    case Ifile =:= Filename of
        true -> "";  % skip the .erl file being compiled
        false ->
            ["\n", Filename, ":\n"]
    end.


gen_depfile_make_rule(Target, Deps0, IncludeParseTransforms) ->
    % .d makefile depends only on includes and parse_transforms (not behaviors
    % and other stuff)
    Deps1 = [
        X
        ||
        X = {DepType, _} <- Deps0,
        DepType =:= file orelse (DepType =:= parse_transform andalso IncludeParseTransforms)
    ],
    DepsFilenames = deps_to_unique_filenames(Deps1),
    gen_make_rule(Target, DepsFilenames).


check_parse_errors(Forms, St0) ->
    % NOTE: not checking for parse_errors for erl1 -- in order to precisely follow erlc
    % behavior
    case is_lang_erl2(St0) of
        true ->
            do_check_parse_errors(Forms, St0);
        false ->
            {ok, Forms, St0}
    end.


do_check_parse_errors(Forms, St0) ->
    % in normal Erlang compilation lexer, parser, and epp errors are reported by
    % erl_lint.erl which is run as one of the later passes inside compile_erl1_forms()
    %
    % under erl2 stricter compilation model we no longer allow lexer & parser errors to
    % slip through, because we need valid forms to be able to run erl2 to erl1
    % translator and the erl2 typechecker
    %
    % under erl2 stricter compilation model we no longer allow epp errors,
    % e.g. includes that are not found at the dependency scan stage. For
    % example, adding originally missing include later may result in a
    % different compile order, not including transitive dependencies, or a
    % different parse output, because of conditional compilation. This may lead
    % to inconsistent build.
    case get_parse_errors(Forms) of
        [] ->
            {ok, Forms, St0};
        Errors ->
            {error, St0#compile{errors=Errors}}
    end.


get_parse_errors(Forms) ->
    get_parse_errors(Forms, _File = 'undefined', _Acc = []).

get_parse_errors([], _File, Acc) ->
    lists:reverse(Acc);
get_parse_errors([{attribute,_,file,{NewFile,_}}|Rest], _File, Acc) ->
    % update the name of the current file
    get_parse_errors(Rest, NewFile, Acc);
get_parse_errors([{error,Error}|Rest], File, Acc) ->
    % lexer/parser/epp error
    CompileError = {File, [Error]},
    get_parse_errors(Rest, File, [CompileError|Acc]);
get_parse_errors([_|Rest], File, Acc) ->
    get_parse_errors(Rest, File, Acc).


collect_erl1_compile_deps(Forms, St0) ->
    Deps = get_erl1_deps_from_forms(Forms, St0),
    %io:format("erl1 deps: ~p~n", [Deps]),

    St1 = append_compile_deps(Deps, St0),
    {ok, Forms, St1}.


append_compile_deps(Deps, St0) ->
    St0#compile{
        compile_deps = lists_append_uniq(St0#compile.compile_deps, Deps)
    }.


lists_append_uniq(From, To) ->
    lists_append_uniq(From, To, _Acc = []).

lists_append_uniq([], To, Acc) ->
    To ++ lists:reverse(Acc);
lists_append_uniq([H|T], To, Acc) ->
    NewAcc =
        case member(H, Acc) orelse member(H, To) of
            true -> Acc;
            false -> [H | Acc]
        end,
    lists_append_uniq(T, To, NewAcc).


get_erl1_deps_from_forms(Forms, St0) ->
    get_erl1_deps_from_forms(Forms, St0, _File = St0#compile.ifile, _Acc = []).


get_erl1_deps_from_forms([], _St, _File, Acc) ->
    % NOTE: returning in the order they were present in the file
    lists:reverse(Acc);

get_erl1_deps_from_forms([{attribute,_,file,{NewFile0,_}}|Rest], St, _File, Acc) ->
    % Remove "./" in front of the dependency filename.
    NewFile = remove_dot_slash(NewFile0),

    Dep = {file, NewFile},
    NewAcc =
        case member(Dep, Acc) of
            true -> Acc;
            false -> [Dep | Acc]
        end,
    % update the name of the current file
    get_erl1_deps_from_forms(Rest, St, NewFile, NewAcc);

get_erl1_deps_from_forms([{attribute,Line,Name,Value}|Rest], St, File, Acc) ->
    Deps = get_erl1_deps_from_attr(File, Line, Name, Value, St),
    get_erl1_deps_from_forms(Rest, St, File, Deps ++ Acc);

get_erl1_deps_from_forms([_|Rest], St, File, Acc) ->
    get_erl1_deps_from_forms(Rest, St, File, Acc).


remove_dot_slash(NewFile0) ->
    case NewFile0 of
        "./" ++ NewFile1 -> NewFile1;
        _ -> NewFile0
    end.


get_erl1_deps_from_attr(File, Line, Name, Value, St) ->
    Loc = {File, Line},
    case Name of
	import ->
            get_deps_from_import(Value, St);
        compile ->
            get_deps_from_compile(Loc, Value, St);
        _ when Name =:= behavior; Name =:= behaviour ->
            get_deps_from_behavior(Loc, Value, St);
        _ ->
            []
    end.


get_deps_from_import({_Mod, _Funs}, _St) ->
    % NOTE: not returning anything for now, as imported module is not a
    % compile-time dependency, but rather a runtime one. That is erlc doesn't
    % care about this, unlike in case of behaviors and parse_transforms
    %
    % Furthermore, imports could be circular, which is likely the reason for
    % not handling them at compile time in the current implementation
    [].


get_deps_from_behavior(Loc, Mod, St) ->
    [resolve_module_dependency(behavior, Loc, Mod, St)].


get_deps_from_compile(Loc, Value, St) ->
    case Value of
        L when is_list(L) ->  % list of values
            lists:foldl(
                fun (X, Acc) -> get_deps_from_compile_item(Loc, X, St) ++ Acc end,
                [], L
            );
        _ ->  % single value
            get_deps_from_compile_item(Loc, Value, St)
    end.


get_deps_from_compile_item(Loc, Value, St) ->
    case Value of
        {parse_transform, Mod} ->
            [resolve_module_dependency(parse_transform, Loc, Mod, St)];
        {core_transform, Mod} ->
            [resolve_module_dependency(core_transform, Loc, Mod, St)];
        _ ->
            []
    end.


% ModuleDepType = behavior | parse_transform | core_transform | depends_on | type_checking
resolve_module_dependency(ModuleDepType, Loc, Mod, St) ->
    Erl = filename:join(St#compile.dir, module_to_erl(Mod)),
    SpecsErl = filename:join(St#compile.dir, module_to_specs_erl(Mod)),
    case {filelib:is_regular(Erl), filelib:is_regular(SpecsErl)} of
        {true, _} ->
            % Mod's .erl is next to the original source .erl file, i.e. they
            % are in the same application. Make it depend on the .beam file
            % next to the original target .beam
            Beam0 = filename:join(filename:dirname(St#compile.ofile), module_to_beam(Mod)),

            % NOTE: have to use the same shorten_filename() transformation as
            % on the TargetBeam in output_compile_deps()
            Beam = shorten_filename(Beam0),

            {ModuleDepType, Beam};
        {_, true} ->
            Beam0 = filename:join(filename:dirname(St#compile.ofile), module_to_specs_beam(Mod)),
            Beam = shorten_filename(Beam0),
            {ModuleDepType, Beam};
        _ ->
            % it has to be present somewhere for compilation to work
            case code:which(Mod) of
                Path when is_list(Path) ->
                    {ModuleDepType, Path};
                _ ->
                    {ErrorFile, ErrorLine} = Loc,
                    Error = {ErrorFile, [{ErrorLine,?MODULE,{module_dependency,ModuleDepType,Mod}}]},

                    % NOTE: the error thrown here will be caught by internal_comp() -> Run0
                    throw({error,St#compile{errors=[Error]}})
            end
    end.


module_to_erl(Mod) ->
    atom_to_list(Mod) ++ ".erl".

module_to_specs_erl(Mod) ->
    atom_to_list(Mod) ++ ".specs.erl".

module_to_beam(Mod) ->
    atom_to_list(Mod) ++ ".beam".

module_to_specs_beam(Mod) ->
    atom_to_list(Mod) ++ ".specs.beam".


%% "mod.specs" -> "mod"
unspecs(Str) ->
    string:slice(Str, 0, length(Str) - 6).


fix_compile_options(Options) ->
    %% forcing to use nowarn_unused_record,
    %% motivation: unfolding of -module_alias(m1, some_module) brings ALL the records from some_module
    %% as they were defined (textually) in the current module.
    %% If any of them is unused then erl_lint:check_unused_records will produce a warning,
    %% which can be bad in the settings when warnings are errors.
    Options1 = lists:filter(fun (O) -> O =/= warn_unused_record orelse O =/= nowarn_unused_record end, Options),
    [nowarn_unused_record,no_error_module_mismatch|Options1].

compile_erl1_forms(Forms, St0) ->
    % NOTE: using forms_noenv() instead of forms(), because we've already
    % appended env_compiler_options() above
    % Make the compiler return errors and warnings instead of printing them
    Opts0 = St0#compile.options -- [report_warnings, report_errors],
    Forms1 = case get_lang(St0) of specs -> [{attribute,L,module,M}||{attribute,L,module,M} <- Forms]; _ -> Forms end,
    Ret = compile:noenv_forms(Forms1, [return_errors, return_warnings,
                                      {source, St0#compile.filename}
                                      | Opts0]),

    % TODO: handling of ok is not exhaustive, there could also be {ok, ModuleName, Warnings}
    case Ret of
        {ok,ModuleName} ->
            {ok, none, St0#compile{module=ModuleName}};
        {ok,ModuleName,BinaryOrCode} ->
            {ok, BinaryOrCode, St0#compile{module=ModuleName}};
        {ok,ModuleName,BinaryOrCode,Ws} ->
            Ws0 = St0#compile.warnings,
            {ok, BinaryOrCode, St0#compile{module=ModuleName, warnings=Ws0++Ws}};
        {error, Ws, Es} ->
            Ws0 = St0#compile.warnings,
            Es0 = St0#compile.errors,
            {error, St0#compile{errors=Es0++Es, warnings=Ws0++Ws}};
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


format_error({ocaml_typechecker,ExitCode,Output}) ->
    io_lib:format("ocaml exited with error code ~w: ~n~s~n", [ExitCode, Output]);
format_error({module_dependency,type_checking,Mod}) ->
    io_lib:format("can't find ~s.beam", [Mod]);
format_error({module_dependency,ModuleDepType,Mod}) ->
    io_lib:format("can't find ~s.beam from -~s(~s)", [Mod, ModuleDepType, Mod]);
format_error(X) ->
    % TODO: copy formatters for locally-generated errors from copy-pasted code
    % here to avoid problems with error format compatibility in the future
    compile:format_error(X).


internal_comp(Passes, Code0, File, Suffix, St0) ->
    Dir = filename:dirname(File),
    Base = filename:basename(File, Suffix),
    St1 = St0#compile{filename=File, dir=Dir, base=Base,
		      ifile=erlfile(Dir, Base, Suffix),
		      ofile=objfile(Base, St0)},
    Opts = St1#compile.options,
    Run0 = case member(time, Opts) of
	       true  ->
		   io:format("Compiling ~tp\n", [File]),
		   fun run_tc/3;
	       false ->
                   fun({_Name,Fun}, Code, St) ->
                           catch Fun(Code, St)
                   end
	   end,
    Run = case keyfind(eprof, 1, Opts) of
	      {eprof,EprofPass} ->
		  fun(P, Code, St) ->
			  run_eprof(P, Code, EprofPass, St)
		  end;
	      false ->
		  Run0
	  end,
    case fold_comp(Passes, Run, Code0, St1) of
	{ok,Code,St2} -> comp_ret_ok(Code, St2);
	{error,St2} -> comp_ret_err(St2)
    end.

fold_comp([{Name,Pass}|Ps], Run, Code0, St0) ->
    case Run({Name,Pass}, Code0, St0) of
	{ok,Code,St1} ->
            fold_comp(Ps, Run, Code, St1);
	{error,_St1}=Error ->
            Error;
	{'EXIT',Reason} ->
	    Es = [{St0#compile.ifile,[{none,?MODULE,{crash,Name,Reason}}]}],
	    {error,St0#compile{errors=St0#compile.errors ++ Es}};
	Other ->
	    Es = [{St0#compile.ifile,[{none,?MODULE,{bad_return,Name,Other}}]}],
	    {error,St0#compile{errors=St0#compile.errors ++ Es}}
    end;
fold_comp([], _Run, Code, St) -> {ok,Code,St}.

run_tc({Name,Fun}, Code, St) ->
    T1 = erlang:monotonic_time(),
    Val = (catch Fun(Code, St)),
    T2 = erlang:monotonic_time(),
    Elapsed = erlang:convert_time_unit(T2 - T1, native, millisecond),
    Mem0 = erts_debug:flat_size(Val)*erlang:system_info(wordsize),
    Mem = lists:flatten(io_lib:format("~.1f kB", [Mem0/1024])),
    io:format(" ~-30s: ~10.3f s ~12s\n",
	      [Name,Elapsed/1000,Mem]),
    Val.

run_eprof({Name,Fun}, Code, Name, St) ->
    io:format("~p: Running eprof\n", [Name]),
    c:appcall(tools, eprof, start_profiling, [[self()]]),
    Val = (catch Fun(Code, St)),
    c:appcall(tools, eprof, stop_profiling, []),
    c:appcall(tools, eprof, analyze, []),
    Val;
run_eprof({_,Fun}, Code, _, St) ->
    catch Fun(Code, St).

comp_ret_ok(Code, #compile{warnings=Warn0,module=Mod,options=Opts}=St) ->
    case werror(St) of
        true ->
            case member(report_warnings, Opts) of
                true ->
		    io:format("~p: warnings being treated as errors\n",
			      [?MODULE]);
                false ->
		    ok
            end,
            comp_ret_err(St);
        false ->
            Warn = messages_per_file(Warn0),
            report_warnings(St#compile{warnings = Warn}),
            Ret1 = case member(binary, Opts) andalso
		       not member(no_code_generation, Opts) of
                       true -> [Code];
                       false -> []
                   end,
            Ret2 = case member(return_warnings, Opts) of
                       true -> Ret1 ++ [Warn];
                       false -> Ret1
                   end,
            list_to_tuple([ok,Mod|Ret2])
    end.

comp_ret_err(#compile{warnings=Warn0,errors=Err0,options=Opts}=St) ->
    Warn = messages_per_file(Warn0),
    Err = messages_per_file(Err0),
    report_errors(St#compile{errors=Err}),
    report_warnings(St#compile{warnings=Warn}),
    case member(return_errors, Opts) of
	true -> {error,Err,Warn};
	false -> error
    end.

werror(#compile{options=Opts,warnings=Ws}) ->
    Ws =/= [] andalso member(warnings_as_errors, Opts).

%% messages_per_file([{File,[Message]}]) -> [{File,[Message]}]
messages_per_file(Ms) ->
    T = lists:sort([{File,M} || {File,Messages} <- Ms, M <- Messages]),
    PrioMs = [erl2_scan, erl2_epp, erl2_parse],
    {Prio0, Rest} =
        lists:mapfoldl(fun(M, A) ->
                               lists:partition(fun({_,{_,Mod,_}}) -> Mod =:= M;
                                                  (_) -> false
                                               end, A)
                       end, T, PrioMs),
    Prio = lists:sort(fun({_,{L1,_,_}}, {_,{L2,_,_}}) -> L1 =< L2 end,
                      lists:append(Prio0)),
    flatmap(fun mpf/1, [Prio, Rest]).

mpf(Ms) ->
    [{File,[M || {F,M} <- Ms, F =:= File]} ||
	File <- lists:usort([F || {F,_} <- Ms])].

select_passes([{pass,Mod}|Ps], Opts) ->
    F = fun(Code0, St) ->
		case catch Mod:module(Code0, St#compile.options) of
		    {ok,Code} ->
			{ok,Code,St};
		    {ok,Code,Ws} ->
			{ok,Code,St#compile{warnings=St#compile.warnings++Ws}};
		    {error,Es} ->
			{error,St#compile{errors=St#compile.errors ++ Es}}
		end
	end,
    [{Mod,F}|select_passes(Ps, Opts)];
select_passes([{src_listing,Ext}|_], _Opts) ->
    [{listing,fun (Code, St) -> src_listing(Ext, Code, St) end}];
select_passes([{listing,Ext}|_], _Opts) ->
    [{listing,fun (Code, St) -> listing(Ext, Code, St) end}];
select_passes([done|_], _Opts) ->
    [];
select_passes([{done,Ext}|_], Opts) ->
    select_passes([{unless,binary,{listing,Ext}}], Opts);
select_passes([{iff,Flag,Pass}|Ps], Opts) ->
    select_cond(Flag, true, Pass, Ps, Opts);
select_passes([{unless,Flag,Pass}|Ps], Opts) ->
    select_cond(Flag, false, Pass, Ps, Opts);
select_passes([{_,Fun}=P|Ps], Opts) when is_function(Fun) ->
    [P|select_passes(Ps, Opts)];
select_passes([{delay,Passes0}|Ps], Opts) when is_list(Passes0) ->
    %% Delay evaluation of compiler options and which compiler passes to run.
    %% Since we must know beforehand whether a listing will be produced, we
    %% will go through the list of passes and evaluate all conditions that
    %% select a list pass.
    case select_list_passes(Passes0, Opts) of
	{done,Passes} ->
	    [{delay,Passes}];
	{not_done,Passes} ->
	    [{delay,Passes}|select_passes(Ps, Opts)]
    end;
select_passes([{_,Test,Fun}=P|Ps], Opts) when is_function(Test),
					      is_function(Fun) ->
    [P|select_passes(Ps, Opts)];
select_passes([], _Opts) ->
    [];
select_passes([List|Ps], Opts) when is_list(List) ->
    case select_passes(List, Opts) of
	[] -> select_passes(Ps, Opts);
	Nested ->
	    case last(Nested) of
		{listing,_Fun} -> Nested;
		_Other         -> Nested ++ select_passes(Ps, Opts)
	    end
    end.

select_cond(Flag, ShouldBe, Pass, Ps, Opts) ->
    ShouldNotBe = not ShouldBe,
    case member(Flag, Opts) of
	ShouldBe    -> select_passes([Pass|Ps], Opts);
	ShouldNotBe -> select_passes(Ps, Opts)
    end.

%% select_list_passes([Pass], Opts) -> {done,[Pass]} | {not_done,[Pass]}
%%  Evaluate all conditions having to do with listings in the list of
%%  passes.

select_list_passes(Ps, Opts) ->
    select_list_passes_1(Ps, Opts, []).

select_list_passes_1([{iff,Flag,{listing,_}=Listing}|Ps], Opts, Acc) ->
    case member(Flag, Opts) of
	true -> {done,reverse(Acc, [Listing])};
	false -> select_list_passes_1(Ps, Opts, Acc)
    end;
select_list_passes_1([{iff,Flag,{done,Ext}}|Ps], Opts, Acc) ->
    case member(Flag, Opts) of
	false ->
	    select_list_passes_1(Ps, Opts, Acc);
	true ->
	    {done,case member(binary, Opts) of
		      false -> reverse(Acc, [{listing,Ext}]);
		      true -> reverse(Acc)
		  end}
    end;
select_list_passes_1([{iff=Op,Flag,List0}|Ps], Opts, Acc) when is_list(List0) ->
    case select_list_passes(List0, Opts) of
	{done,List} -> {done,reverse(Acc) ++ List};
	{not_done,List} -> select_list_passes_1(Ps, Opts, [{Op,Flag,List}|Acc])
    end;
select_list_passes_1([{unless=Op,Flag,List0}|Ps], Opts, Acc) when is_list(List0) ->
    case select_list_passes(List0, Opts) of
	{done,List} -> {done,reverse(Acc) ++ List};
	{not_done,List} -> select_list_passes_1(Ps, Opts, [{Op,Flag,List}|Acc])
    end;
select_list_passes_1([P|Ps], Opts, Acc) ->
    select_list_passes_1(Ps, Opts, [P|Acc]);
select_list_passes_1([], _, Acc) ->
    {not_done,reverse(Acc)}.


%% Remove the target file so we don't have an old one if the compilation fail.
remove_file(Code, St) ->
    _ = file:delete(St#compile.ofile),
    {ok,Code,St}.


parse_module(Forms0, St0) ->
    % try parsing as erl2 first
    Res = parse_module(Forms0, St0, _EppMod2 = erl2_epp),
    case Res of
        {ok, _Forms1, St1} ->
            case is_lang_erl2(St1) of
                true ->
                    % this is erl2, we guessed it right => returning
                    Res;
                false ->
                    % this is erl1, reparsing it as erl1
                    %
                    % NOTE, XXX: this is a temporary solution; it is unnecessarily expensive and only works for as long
                    % as erl2 syntax is an extension of erl1 syntax, which may not be true in the future
                    parse_module(Forms0, St0, _EppMod1 = epp)
            end;
        _ ->  % error
            Res
    end.


parse_module(_Code, St0, EppMod) ->
    case do_parse_module(utf8, St0, EppMod) of
	{ok,Fs0,St1} ->
            % extract indicator of Erlang language flavor
            {Lang, Fs1} = parse_lang(Fs0),
            {ok,Fs1,St1#compile{lang = Lang}};
	{error,_}=Ret ->
	    Ret;
	{invalid_unicode,File,Line} ->
	    case do_parse_module(latin1, St0, EppMod) of
		{ok,Code,St} ->
		    Es = [{File,[{Line,?MODULE,reparsing_invalid_unicode}]}],
		    {ok,Code,St#compile{warnings=Es++St#compile.warnings}};
		{error,St} ->
		    Es = [{File,[{Line,?MODULE,reparsing_invalid_unicode}]}],
		    {error,St#compile{errors=Es++St#compile.errors}}
	    end
    end.

erl2_module_record(Code, St) ->
    case is_lang_erl2(St) of
        true ->
            Code1 = erl2_module_record:parse_transform(Code, St#compile.options),
            {ok, Code1, St};
        false ->
            {ok, Code, St}
    end.

erl2_expand(Code, St) ->
    case is_lang_erl2(St) of
        true ->
            Code1 = erl2_expand:module(Code, St#compile.options),
            {ok, Code1, St};
        false ->
            {ok, Code, St}
    end.

erl2_lint(Code, St) ->
    case lists:member(get_lang(St), [dt,st,ffi]) of
        true ->
            do_erl2_lint(Code, St);
        false ->
            {ok, Code, St}
    end.


do_erl2_lint(Code, St) ->
    Opts = [nowarn_unused_type | St#compile.options],
    case erl2_lint:module(Code, St#compile.ifile, Opts) of
	{ok,Ws} ->
	    {ok,Code,St#compile{warnings=St#compile.warnings ++ Ws}};
	{error,Es,Ws} ->
	    {error,St#compile{warnings=St#compile.warnings ++ Ws,
			      errors=St#compile.errors ++ Es}}
    end.

parse_lang(Forms) ->
    parse_lang(Forms, _Lang = [], _Acc = []).


% NOTE: to count, -lang(...) has to preceed -module(...)
parse_lang([], Lang, Acc) ->
    {Lang, lists:reverse(Acc)};
parse_lang([{attribute,_,module,_} | _] = Forms, Lang, Acc) ->
    {Lang, lists:reverse(Acc, Forms)};
parse_lang([{attribute,_,lang,Lang} | Rest], _, Acc) ->
    % TODO: validate Lang -- see #compile{} definition for list of valid values
    {Lang, lists:reverse(Acc, Rest)};
parse_lang([Form | Rest], Lang, Acc) ->
    parse_lang(Rest, Lang, [Form | Acc]).


do_parse_module(DefEncoding, #compile{ifile=File,options=Opts,dir=Dir}=St, EppMod) ->
    SourceName0 = proplists:get_value(source, Opts, File),
    SourceName = case member(deterministic, Opts) of
                     true -> filename:basename(SourceName0);
                     false -> SourceName0
                 end,
    R = EppMod:parse_file(File,
                        [{includes,[".",Dir|inc_paths(Opts)]},
                         {source_name, SourceName},
                         {macros,pre_defs(Opts)},
                         {default_encoding,DefEncoding},
                         {location,{1,1}},
                         {scan_opts,[text]},
                         extra]),
    case R of
	{ok,Forms,Extra} ->
	    Encoding = proplists:get_value(encoding, Extra),
	    case find_invalid_unicode(Forms, File) of
		none ->
		    {ok,Forms,St#compile{encoding=Encoding}};
		{invalid_unicode,_,_}=Ret ->
		    case Encoding of
			none ->
			    Ret;
			_ ->
			    {ok,Forms,St#compile{encoding=Encoding}}
		    end
	    end;
	{error,E} ->
	    Es = [{St#compile.ifile,[{none,?MODULE,{epp,E}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

find_invalid_unicode([H|T], File0) ->
    case H of
	{attribute,_,file,{File,_}} ->
	    find_invalid_unicode(T, File);
	{error,{Line,file_io_server,invalid_unicode}} ->
	    {invalid_unicode,File0,Line};
	_Other ->
	    find_invalid_unicode(T, File0)
    end;
find_invalid_unicode([], _) -> none.

extract_options(Code0, #compile{options=Opt}=St) ->
    %% Extract compile options from code into options field.
    {ok, Code0, St#compile{options=Opt ++ compile_options(Code0)}}.

compile_options([{attribute,_L,compile,C}|Fs]) when is_list(C) ->
    C ++ compile_options(Fs);
compile_options([{attribute,_L,compile,C}|Fs]) ->
    [C|compile_options(Fs)];
compile_options([_F|Fs]) -> compile_options(Fs);
compile_options([]) -> [].

clean_parse_transforms(Fs) ->
    clean_parse_transforms_1(Fs, []).

clean_parse_transforms_1([{attribute,L,compile,C0}|Fs], Acc) when is_list(C0) ->
    C = lists:filter(fun({parse_transform,_}) -> false;
			(_) -> true
		     end, C0),
    clean_parse_transforms_1(Fs, [{attribute,L,compile,C}|Acc]);
clean_parse_transforms_1([{attribute,_,compile,{parse_transform,_}}|Fs], Acc) ->
    clean_parse_transforms_1(Fs, Acc);
clean_parse_transforms_1([F|Fs], Acc) ->
    clean_parse_transforms_1(Fs, [F|Acc]);
clean_parse_transforms_1([], Acc) -> reverse(Acc).

transforms(Os) -> [ M || {parse_transform,M} <- Os ].

transform_module(Code0, #compile{options=Opt}=St) ->
    case transforms(Opt) of
	[] ->
            %% No parse transforms.
            {ok,Code0,St};
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


is_lang_erl2(St) ->
    member(erl2, St#compile.lang).

is_lang_ffi(St) ->
    member(ffi, St#compile.lang).

is_lang_st(St) ->
    member(st, St#compile.lang).

is_lang_specs(St) ->
    member(specs, St#compile.lang).

get_lang(St) ->
    case length(St#compile.lang) of
        0 -> erl1;
        1 -> dt;
        2 -> lists:nth(2, lists:usort(St#compile.lang))
    end.

collect_erl2_compile_deps(Forms, St0) ->
    case is_lang_erl2(St0) of
        true ->
            do_collect_erl2_compile_deps(Forms, St0);
        false ->
            {ok, Forms, St0}
    end.


do_collect_erl2_compile_deps(Forms, St0) ->
    Deps = get_erl2_deps_from_forms(Forms, St0) ++ get_erl2_typed_deps(Forms, St0),
    %io:format("erl2 deps: ~p~n", [Deps]),

    St1 = append_compile_deps(Deps, St0),
    {ok, Forms, St1}.

get_erl2_typed_deps(Forms, St0) ->
    RawDeps =
        case {is_lang_st(St0), is_lang_ffi(St0)} of
            {true, _} -> erl2ocaml:st_deps(Forms);
            {_, true} -> erl2ocaml:ffi_deps(Forms);
            {_, _} -> []
        end,
    F = St0#compile.ifile,
    [resolve_module_dependency(type_checking, {F, L}, M, St0) || {L, M} <- RawDeps].

% TODO: remove code duplication between this and get_erl1_deps_from_forms()
get_erl2_deps_from_forms(Forms, St0) ->
    get_erl2_deps_from_forms(Forms, St0, _File = St0#compile.ifile, _Acc = []).


get_erl2_deps_from_forms([], _St, _File, Acc) ->
    % NOTE: returning in the order they were present in the file
    lists:reverse(Acc);

get_erl2_deps_from_forms([{attribute,_,file,{NewFile0,_}}|Rest], St, _File, Acc) ->
    % Remove "./" in front of the dependency filename.
    NewFile = remove_dot_slash(NewFile0),

    % update the name of the current file
    %
    % NOTE: not adding the file to the list of dependencies, because it will be added
    % during get_erl1_deps_from_forms() pass
    get_erl2_deps_from_forms(Rest, St, NewFile, Acc);

get_erl2_deps_from_forms([{attribute,Line,Name,Value}|Rest], St, File, Acc) ->
    Deps = get_erl2_deps_from_attr(File, Line, Name, Value, St),
    get_erl2_deps_from_forms(Rest, St, File, Deps ++ Acc);

get_erl2_deps_from_forms([_|Rest], St, File, Acc) ->
    get_erl2_deps_from_forms(Rest, St, File, Acc).


get_erl2_deps_from_attr(File, Line, Name, Value, St) ->
    Loc = {File, Line},
    case Name of
        depends_on ->
            % erl2 feature for specifying dependencies explicitly
            get_erl2_deps_from_depends_on(Loc, Value, St);
        _ ->
            []
    end.


% TODO: validate -depends_on([...]) properly
get_erl2_deps_from_depends_on(Loc, Deps, St) when is_list(Deps) ->
    [get_erl2_deps_from_depends_on_item(Loc, X, St) || X <- Deps].


get_erl2_deps_from_depends_on_item(Loc, Mod, St) when is_atom(Mod) ->
    resolve_module_dependency(depends_on, Loc, Mod, St).


erl2_typecheck(Code, St) ->
    case {is_lang_erl2(St), is_lang_ffi(St) orelse is_lang_st(St) orelse is_lang_specs(St)} of
        {true, true} ->
            do_erl2_typecheck(Code, St);
        {_, _} ->
            {ok,Code,St}
    end.


do_erl2_typecheck(Code, St) ->
    OcamlDir = filename:join(St#compile.build_dir, "ocaml"),
    Basename = filename:rootname(filename:basename(St#compile.ofile)),
    case generate_ocaml_code(OcamlDir, Basename, Code, St) of
        ok ->
            call_ocaml_typechecker(OcamlDir, Basename, St),
            {ok,Code,St};
        {error, {Line, ErrorBody}} ->
            StError = {St#compile.ifile, [{Line, erl2ocaml, ErrorBody}]},
            {error, St#compile{errors = [StError|St#compile.errors]}}
    end.

generate_ocaml_code(OcamlDir, Basename, Forms, St) ->
    Rootname = filename:join(OcamlDir, Basename),
    % TODO: error handling
    ok = filelib:ensure_dir(Rootname),

    case get_lang(St) of
        ffi ->
            case erl2ocaml:erl2ocaml_ffi(Forms) of
                {ok, PubMliCode, PrivMliCode} ->
                    ok = file:write_file(Rootname ++ ".mli", PubMliCode),
                    ok = file:write_file(Rootname ++ "_priv.mli", PrivMliCode);
                Error = {error, _} ->
                    Error
            end;
        st ->
            case erl2ocaml:erl2ocaml_st(Forms) of
                {ok, MliCode, MlCode, PrivMlCode} ->
                    ok = file:write_file(Rootname ++ ".ml", MlCode),
                    ok = file:write_file(Rootname ++ ".mli", MliCode),
                    ok = file:write_file(Rootname ++ "_priv.ml", PrivMlCode);
                Error = {error, _} ->
                    Error
            end;
        specs ->
            case erl2ocaml:erl2ocaml_specs(Forms) of
                {ok, MliCode} ->
                    ok = file:write_file(unspecs(Rootname) ++ ".mli", MliCode);
                Error = {error, _} ->
                    Error
            end
    end.

ensure_ocaml_ffi(OcamlDir) ->
    FfiName = filename:join(OcamlDir, "ffi.mli"),
    case filelib:is_regular(FfiName) of
        true -> ok;
        false ->
            ok = file:write_file(FfiName, erl2ocaml:ffi()),
            {0, _} = eunit_lib:command("ocamlc -c ffi.mli", OcamlDir),
            ok
    end.

call_ocaml_typechecker(OcamlDir, Basename, St) ->
    ok = ensure_ocaml_ffi(OcamlDir),

    FileArgs =
        case get_lang(St) of
            ffi ->
                [Basename, ".mli", " ", Basename, "_priv.mli"];
            st ->
                [Basename, ".mli", " ", Basename, ".ml", " ", Basename, "_priv.ml"];
            specs ->
                [unspecs(Basename), ".mli"]
        end,

    FmtCmd = lists:append(["ocamlformat --enable-outside-detected-project -i " | FileArgs]),
    CheckCmd = lists:append(["ocamlc  -c ", erl2ocaml:compiler_flags(), FileArgs]),
    {0, _} = eunit_lib:command(FmtCmd, OcamlDir),
    {ExitCode, Output} = eunit_lib:command(CheckCmd, OcamlDir),
    case ExitCode of
        0 ->
            % TODO: check for warnings
            ok;
        _ ->
            ErrorFile = St#compile.filename,
            Error = {ErrorFile, [{_ErrorLine = 1,?MODULE,{ocaml_typechecker,ExitCode,Output}}]},

            % NOTE: the error thrown here will be caught by internal_comp() -> Run0
            throw({error,St#compile{errors=[Error]}})
    end.


erl2_to_erl1(Code, St) ->
    case is_lang_erl2(St) of
        true ->
            do_erl2_to_erl1(Code, St);
        false ->
            {ok,Code,St}
    end.


do_erl2_to_erl1(Code, St) ->
    foldl_transform([erl2_enum,erl2_dots,erl2_caret], Code, St).


foldl_transform([T|Ts], Code0, St) ->
    Name = "transform " ++ atom_to_list(T),
    case code:ensure_loaded(T) =:= {module,T} andalso
        erlang:function_exported(T, parse_transform, 2) of
        true ->
            Fun = fun(Code, S) ->
                          T:parse_transform(Code, S#compile.options)
                  end,
            Run = case member(time, St#compile.options) of
                      true  ->
                          fun run_tc/3;
                      false ->
                          fun({_Name,F}, Code, S) ->
                                  catch F(Code, S)
                          end
                  end,
            case Run({Name, Fun}, Code0, St) of
                {error,Es,Ws} ->
                    {error,St#compile{warnings=St#compile.warnings ++ Ws,
                                      errors=St#compile.errors ++ Es}};
                {'EXIT',R} ->
                    Es = [{St#compile.ifile,[{none,compile,
                                              {parse_transform,T,R}}]}],
                    {error,St#compile{errors=St#compile.errors ++ Es}};
                {warning, Forms, Ws} ->
                    foldl_transform(Ts, Forms,
                                    St#compile{warnings=St#compile.warnings ++ Ws});
                Forms ->
                    foldl_transform(Ts, Forms, St)
            end;
        false ->
            Es = [{St#compile.ifile,[{none,compile,
                                      {undef_parse_transform,T}}]}],
            {error,St#compile{errors=St#compile.errors ++ Es}}
    end;
foldl_transform([], Code, St) -> {ok,Code,St}.


save_binary(none, St) -> {ok,none,St};
save_binary(Code, #compile{module=Mod,ofile=Outfile,options=Opts}=St) ->
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
		    Es = [{St#compile.ofile,
			   [{none,?MODULE,{module_name,Mod,Base}}]}],
		    {error,St#compile{errors=St#compile.errors ++ Es}}
	    end
    end.

save_binary_1(Code, St) ->
    Ofile = St#compile.ofile,
    Tfile = tmpfile(Ofile),		%Temp working file
    case write_binary(Tfile, Code, St) of
	ok ->
	    case file:rename(Tfile, Ofile) of
		ok ->
		    {ok,none,St};
		{error,RenameError} ->
		    Es0 = [{Ofile,[{none,?MODULE,{rename,Tfile,Ofile,
						  RenameError}}]}],
		    Es = case file:delete(Tfile) of
			     ok -> Es0;
			     {error,DeleteError} ->
				 Es0 ++
				     [{Ofile,
				       [{none,?MODULE,{delete_temp,Tfile,
						       DeleteError}}]}]
			 end,
		    {error,St#compile{errors=St#compile.errors ++ Es}}
	    end;
	{error,Error} ->
	    Es = [{Tfile,[{none,compile,{write_error,Error}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

write_binary(Name, Bin, St) ->
    Opts = case member(compressed, St#compile.options) of
	       true -> [compressed];
	       false -> []
	   end,
    case file:write_file(Name, Bin, Opts) of
	ok -> ok;
	{error,_}=Error -> Error
    end.

%% report_errors(State) -> ok
%% report_warnings(State) -> ok

report_errors(#compile{options=Opts,errors=Errors}) ->
    case member(report_errors, Opts) of
	true ->
	    foreach(fun ({{F,_L},Eds}) -> list_errors(F, Eds);
			({F,Eds}) -> list_errors(F, Eds) end,
		    Errors);
	false -> ok
    end.

report_warnings(#compile{options=Opts,warnings=Ws0}) ->
    Werror = member(warnings_as_errors, Opts),
    P = case Werror of
	    true -> "";
	    false -> "Warning: "
	end,
    ReportWerror = Werror andalso member(report_errors, Opts),
    case member(report_warnings, Opts) orelse ReportWerror of
	true ->
	    Ws1 = flatmap(fun({{F,_L},Eds}) -> format_message(F, P, Eds);
			     ({F,Eds}) -> format_message(F, P, Eds) end,
			  Ws0),
	    Ws = lists:sort(Ws1),
	    foreach(fun({_,Str}) -> io:put_chars(Str) end, Ws);
	false -> ok
    end.

format_message(F, P, [{Anno,Mod,E}|Es]) when is_list(Anno) ->
    format_message(F, P, [{erl_anno:location(Anno),Mod,E}|Es]);
format_message(F, P, [{none,Mod,E}|Es]) ->
    M = {none,io_lib:format("~ts: ~s~ts\n", [F,P,Mod:format_error(E)])},
    [M|format_message(F, P, Es)];
format_message(F, P, [{{Line,Column}=Loc,Mod,E}|Es]) ->
    M = {{F,Loc},io_lib:format("~ts:~w:~w ~s~ts\n",
                                [F,Line,Column,P,Mod:format_error(E)])},
    [M|format_message(F, P, Es)];
format_message(F, P, [{Line,Mod,E}|Es]) ->
    M = {{F,{Line,0}},io_lib:format("~ts:~w: ~s~ts\n",
                                [F,Line,P,Mod:format_error(E)])},
    [M|format_message(F, P, Es)];
format_message(_, _, []) -> [].

%% list_errors(File, ErrorDescriptors) -> ok

list_errors(F, [{Anno,Mod,E}|Es]) when is_list(Anno) ->
    list_errors(F, [{erl_anno:location(Anno),Mod,E}|Es]);
list_errors(F, [{none,Mod,E}|Es]) ->
    io:fwrite("~ts: ~ts\n", [F,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(F, [{{Line,Column},Mod,E}|Es]) ->
    io:fwrite("~ts:~w:~w: ~ts\n", [F,Line,Column,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(F, [{Line,Mod,E}|Es]) ->
    io:fwrite("~ts:~w: ~ts\n", [F,Line,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(_F, []) -> ok.


erlfile(".", Base, Suffix) ->
    Base ++ Suffix;
erlfile(Dir, Base, Suffix) ->
    filename:join(Dir, Base ++ Suffix).

outfile(Base, Ext, Opts) when is_atom(Ext) ->
    outfile(Base, atom_to_list(Ext), Opts);
outfile(Base, Ext, Opts) ->
    Obase = case keyfind(outdir, 1, Opts) of
                {outdir, Odir} -> filename:join(Odir, Base);
		_Other -> Base			% Not found or bad format
	    end,
    Obase ++ "." ++ Ext.

objfile(Base, St) ->
    outfile(Base, "beam", St#compile.options).

tmpfile(Ofile) ->
    reverse([$#|tl(reverse(Ofile))]).

%% pre_defs(Options)
%% inc_paths(Options)
%%  Extract the predefined macros and include paths from the option list.

pre_defs([{d,M,V}|Opts]) ->
    [{M,V}|pre_defs(Opts)];
pre_defs([{d,M}|Opts]) ->
    [M|pre_defs(Opts)];
pre_defs([_|Opts]) ->
    pre_defs(Opts);
pre_defs([]) -> [].

inc_paths(Opts) ->
    [ P || {i,P} <- Opts, is_list(P) ].

src_listing(Ext, Code, St) ->
    listing(fun (Lf, {_Mod,_Exp,Fs}) -> do_src_listing(Lf, Fs);
		(Lf, Fs) -> do_src_listing(Lf, Fs) end,
	    Ext, Code, St).

do_src_listing(Lf, Fs) ->
    Opts = [lists:keyfind(encoding, 1, io:getopts(Lf))],
    foreach(fun (F) -> io:put_chars(Lf, [erl2_pp:form(F, Opts),"\n"]) end,
	    Fs).

listing(Ext, Code, St0) ->
    St = St0#compile{encoding = none},
    listing(fun(Lf, Fs) -> beam_listing:module(Lf, Fs) end, Ext, Code, St).

listing(LFun, Ext, Code, St) ->
    Lfile = outfile(St#compile.base, Ext, St#compile.options),
    case file:open(Lfile, [write,delayed_write]) of
	{ok,Lf} ->
            Code = restore_expanded_types(Ext, Code),
            output_encoding(Lf, St),
	    LFun(Lf, Code),
	    ok = file:close(Lf),
	    {ok,Code,St};
	{error,Error} ->
	    Es = [{Lfile,[{none,compile,{write_error,Error}}]}],
	    {error,St#compile{errors=St#compile.errors ++ Es}}
    end.

output_encoding(F, #compile{encoding = none}) ->
    ok = io:setopts(F, [{encoding, epp:default_encoding()}]);
output_encoding(F, #compile{encoding = Encoding}) ->
    ok = io:setopts(F, [{encoding, Encoding}]),
    ok = io:fwrite(F, <<"%% ~s\n">>, [epp:encoding_to_string(Encoding)]).

restore_expanded_types("E", {M,I,Fs0}) ->
    Fs = restore_expand_module(Fs0),
    {M,I,Fs};
restore_expanded_types(_Ext, Code) -> Code.

restore_expand_module([{attribute,Line,type,[Type]}|Fs]) ->
    [{attribute,Line,type,Type}|restore_expand_module(Fs)];
restore_expand_module([{attribute,Line,opaque,[Type]}|Fs]) ->
    [{attribute,Line,opaque,Type}|restore_expand_module(Fs)];
restore_expand_module([{attribute,Line,spec,[Arg]}|Fs]) ->
    [{attribute,Line,spec,Arg}|restore_expand_module(Fs)];
restore_expand_module([{attribute,Line,callback,[Arg]}|Fs]) ->
    [{attribute,Line,callback,Arg}|restore_expand_module(Fs)];
restore_expand_module([{attribute,Line,record,[R]}|Fs]) ->
    [{attribute,Line,record,R}|restore_expand_module(Fs)];
restore_expand_module([F|Fs]) ->
    [F|restore_expand_module(Fs)];
restore_expand_module([]) -> [].
