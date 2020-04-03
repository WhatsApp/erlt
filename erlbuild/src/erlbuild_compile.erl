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

%% Purpose: correct and exhaustive dependency scanner for Erlang
%%
%% This should probably be merged upstream when proven and tested.
-module(erlbuild_compile).

% TODO: this prevents unused function warnings for compiler passes
-compile(export_all).
-compile(nowarn_export_all).

%% High-level interface.
%%
%% TODO: implement forms() and potentialy other APIs supported by the standard compile.erl
-export([file/2]).

%% erlbuild interface.
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
		  ifile=""    :: file:filename(),
		  ofile=""    :: file:filename(),
		  module=[]   :: module() | [],
		  options=[]  :: [option()],  %Options for compilation
                  encoding=none :: none | epp:source_encoding(),
		  errors=[]     :: [err_warn_info()],
		  warnings=[]   :: [err_warn_info()],
                  compile_deps=[] :: [compile_dep()]
}).

-define(pass(P), {P,fun P/2}).


% called by erlbuild.erl
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
    St0 = #compile{options=Options1},

    Suffix = ".erl",

    CompileMode =
        case is_makedep2_mode(Options0) of
            true ->
                makedep2;
            false ->
                compile
        end,

    Passes =
        case CompileMode of
            compile ->
                % normal .erl compilation -- later, we are going to use this
                % for optimizing compilation by caching information already
                % obtained during the "scan" phase
                [
                    % TODO: do not remove the output file unless we know save_binary() is going to run
                    ?pass(remove_file),
                    ?pass(parse_module),

                    ?pass(transform_module),

                    ?pass(compile_forms),
                    ?pass(maybe_save_binary)
                ];
            makedep2 ->
                % 'erlc -M2' -- a new dependency scanner, alternative to 'erlc -M' aka makedep; among other things, it
                % allows to establish the order in which .erl files should be compiled
                TransformPasses = [ ?pass(transform_module) || member(makedep2_run_parse_transforms, Options0) ],
                [
                    ?pass(parse_module),
                    ?pass(check_epp_errors)
                ]
                ++
                TransformPasses
                ++
                [
                    ?pass(collect_compile_deps),
                    ?pass(output_compile_deps)
                ]
        end,

    internal_comp(Passes, _Code0 = unused, File, Suffix, St0).


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

    RuleCode = gen_make_rule_compat(TargetBeam, Deps),

    PhonyRulesCode =
        case proplists:get_value(makedep_phony, St#compile.options) of
               false -> "";
               _ ->
                   [gen_make_phony_rule(St#compile.ifile, X) || X <- Deps]
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


gen_make_rule(_Target, _Deps = []) ->
    [];
gen_make_rule(Target, Deps) ->
    [FirstFilename | RestFilenames] = [Filename || {_DepType, Filename} <- Deps],
    RestLines = [[" \\\n  ", X] || X <- RestFilenames],
    [Target, ": ", FirstFilename, RestLines, "\n"].


% same as gen_make_rule(), but make sure the line are no longer than 78
% characters. This behavior is compatible with erlc (WHY?!)
gen_make_rule_compat(_Target, _Deps = []) ->
    [];
gen_make_rule_compat(Target, Deps) ->
    FirstLine = Target ++ ":",
    gen_make_rule_compat(Deps, FirstLine, _Acc = []).


gen_make_rule_compat([], CurrentLine, Acc) ->
    LastLine = CurrentLine ++ "\n",
    lists:reverse([LastLine | Acc]);

gen_make_rule_compat([H|T], CurrentLine, Acc) ->
    {_DepType, NextFilename} = H,
    NewCurrentLine = CurrentLine ++ " " ++ NextFilename,
    case length(NewCurrentLine) > 76 of
        false ->
            gen_make_rule_compat(T, NewCurrentLine, Acc);
        true ->
            NewAcc = [CurrentLine ++ " \\\n" | Acc],
            NewCurrentLine1 = "  " ++ NextFilename,
            gen_make_rule_compat(T, NewCurrentLine1, NewAcc)
    end.


gen_make_phony_rule(Ifile, {_DepType, Filename}) ->
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
    gen_make_rule(Target, Deps1).


check_epp_errors(Forms, St0) ->
    % under erlbuild stricter compilation model we no longer allow epp errors,
    % e.g. includes that are not found at the dependency scan stage. For
    % example, adding originally missing include later may result in a
    % different compile order, not including transitive dependencies, or a
    % different parse output, because of conditional compilation. This may lead
    % to inconsistent build.
    %
    % in normal Erlang compilation such errors are reported by erl_lint.erl and
    % NOT during makedep depedency scan
    case get_epp_errors(Forms) of
        [] ->
            {ok, Forms, St0};
        Errors ->
            {error, St0#compile{errors=Errors}}
    end.


get_epp_errors(Forms) ->
    get_epp_errors(Forms, _File = 'undefined', _Acc = []).

get_epp_errors([], _File, Acc) ->
    lists:reverse(Acc);
get_epp_errors([{attribute,_,file,{NewFile,_}}|Rest], _File, Acc) ->
    % update the name of the current file
    get_epp_errors(Rest, NewFile, Acc);
get_epp_errors([{error,{_,epp,_} = EppError}|Rest], File, Acc) ->
    % epp error, e.g. include/include_lib wasn't found
    CompileError = {File, [EppError]},
    get_epp_errors(Rest, File, [CompileError|Acc]);
get_epp_errors([_|Rest], File, Acc) ->
    get_epp_errors(Rest, File, Acc).


collect_compile_deps(Forms, St0) ->
    Deps = get_deps_from_forms(Forms, St0),
    %io:format("deps: ~p~n", [Deps]),

    St1 = St0#compile{
        compile_deps = St0#compile.compile_deps ++ Deps
    },
    {ok, Forms, St1}.



get_deps_from_forms(Forms, St0) ->
    get_deps_from_forms(Forms, St0, _File = St0#compile.ifile, _Acc = []).


get_deps_from_forms([], _St, _File, Acc) ->
    % NOTE: returning in the order they were present in the file
    lists:reverse(Acc);

get_deps_from_forms([{attribute,_,file,{NewFile0,_}}|Rest], St, _File, Acc) ->
    % Remove "./" in front of the dependency filename.
    NewFile =
        case NewFile0 of
            "./" ++ NewFile1 -> NewFile1;
            _ -> NewFile0
        end,

    Dep = {file, NewFile},
    NewAcc =
        case member(Dep, Acc) of
            true -> Acc;
            false -> [Dep | Acc]
        end,
    % update the name of the current file
    get_deps_from_forms(Rest, St, NewFile, NewAcc);

get_deps_from_forms([{attribute,Line,Name,Value}|Rest], St, File, Acc) ->
    Deps = get_deps_from_attr(File, Line, Name, Value, St),
    get_deps_from_forms(Rest, St, File, Deps ++ Acc);

get_deps_from_forms([_|Rest], St, File, Acc) ->
    get_deps_from_forms(Rest, St, File, Acc).


get_deps_from_attr(File, Line, Name, Value, St) ->
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


% attr_type = behavior | parse_transform | core_transform
resolve_module_dependency(ModuleDepType, Loc, Mod, St) ->
    Erl = filename:join(St#compile.dir, module_to_erl(Mod)),
    case filelib:is_regular(Erl) of
        true ->
            % Mod's .erl is next to the original source .erl file, i.e. they
            % are in the same application. Make it depend on the .beam file
            % next to the original target .beam
            Beam0 = filename:join(filename:dirname(St#compile.ofile), module_to_beam(Mod)),

            % NOTE: have to use the same shorten_filename() transformation as
            % on the TargetBeam in output_compile_deps()
            Beam = shorten_filename(Beam0),

            {ModuleDepType, Beam};
        false ->
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


module_to_beam(Mod) ->
    atom_to_list(Mod) ++ ".beam".



compile_forms(Forms, St0) ->
    % NOTE: using forms_noenv() instead of forms(), because we've already
    % appended env_compiler_options() above
    Ret = compile:noenv_forms(Forms, [{source, St0#compile.filename} | St0#compile.options]),

    % TODO: handling of ok is not exhaustive, there could also be {ok, ModuleName, Warnings}
    case Ret of
        {ok,ModuleName} ->
            {ok, none, St0#compile{module=ModuleName}};
        {ok,ModuleName,BinaryOrCode} ->
            {ok, BinaryOrCode, St0#compile{module=ModuleName}};
        {ok,ModuleName,BinaryOrCode,Warnings} ->
            {ok, BinaryOrCode, St0#compile{module=ModuleName, warnings=Warnings}};
        _ ->
            % errors in several different shapes
            {error, St0}
    end.


maybe_save_binary(Code, St) ->
    case is_binary(Code) of
        true ->
            save_binary(Code, St);
        false ->
            {ok, none, St}
    end.


format_error({module_dependency,ModuleDepType,Mod}) ->
    io_lib:format("can't find ~s.beam from -~s(~s)", [Mod, ModuleDepType, Mod]);
format_error(X) ->
    % TODO: copy formatters for locally-generated errors from copy-pasted code
    % here to avoid problems with error format compatibility in the future
    compile:format_error(X).


internal_comp(Passes, Code0, File, Suffix, St0) ->
    Dir = filename:dirname(File),
    Base = filename:basename(File, Suffix),
    St1 = St0#compile{filename=File, dir=Dir,
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
    PrioMs = [erl_scan, epp, erl_parse],
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

%% Remove the target file so we don't have an old one if the compilation fail.
remove_file(Code, St) ->
    _ = file:delete(St#compile.ofile),
    {ok,Code,St}.

parse_module(_Code, St0) ->
    case do_parse_module(utf8, St0) of
	{ok,_,_}=Ret ->
	    Ret;
	{error,_}=Ret ->
	    Ret;
	{invalid_unicode,File,Line} ->
	    case do_parse_module(latin1, St0) of
		{ok,Code,St} ->
		    Es = [{File,[{Line,?MODULE,reparsing_invalid_unicode}]}],
		    {ok,Code,St#compile{warnings=Es++St#compile.warnings}};
		{error,St} ->
		    Es = [{File,[{Line,?MODULE,reparsing_invalid_unicode}]}],
		    {error,St#compile{errors=Es++St#compile.errors}}
	    end
    end.

do_parse_module(DefEncoding, #compile{ifile=File,options=Opts,dir=Dir}=St) ->
    SourceName0 = proplists:get_value(source, Opts, File),
    SourceName = case member(deterministic, Opts) of
                     true -> filename:basename(SourceName0);
                     false -> SourceName0
                 end,
    R = epp:parse_file(File,
                        [{includes,[".",Dir|inc_paths(Opts)]},
                         {source_name, SourceName},
                         {macros,pre_defs(Opts)},
                         {default_encoding,DefEncoding},
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
    %% Extract compile options from code into options field.
    case transforms(Opt ++ compile_options(Code0)) of
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
                        % collect_compile_deps() pass
                        Code0;
                    false ->
                        clean_parse_transforms(Code0)
                end,
	    foldl_transform(Ts, Code, St)
    end.


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
format_message(F, P, [{Mod,E}|Es]) ->
    %% Not documented and not expected to be used any more, but
    %% keep a while just in case.
    M = {none,io_lib:format("~ts: ~s~ts\n", [F,P,Mod:format_error(E)])},
    [M|format_message(F, P, Es)];
format_message(_, _, []) -> [].

%% list_errors(File, ErrorDescriptors) -> ok

list_errors(F, [{none,Mod,E}|Es]) ->
    io:fwrite("~ts: ~ts\n", [F,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(F, [{{Line,Column},Mod,E}|Es]) ->
    io:fwrite("~ts:~w:~w: ~ts\n", [F,Line,Column,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(F, [{Line,Mod,E}|Es]) ->
    io:fwrite("~ts:~w: ~ts\n", [F,Line,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(F, [{Mod,E}|Es]) ->
    %% Not documented and not expected to be used any more, but
    %% keep a while just in case.
    io:fwrite("~ts: ~ts\n", [F,Mod:format_error(E)]),
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
