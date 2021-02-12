-module(erlt_build_basic_mode).

-include("erlt_build_types.hrl").

% timeout for scanning or building a file
% Note that this is high because when our team is developing erlt itself we sometimes
% run with sterlang.jar, which is slow to start up. Consider removing the timeouts entirely.
-define(TIMEOUT_FOR_FILE, 15000).

-export([invoke/1]).

-spec invoke(#args{}) -> ok | error.
invoke(#args{command = compile, build_dir = BuildDir} = Args) ->
    % BuildDir is erlt-specific, OK to wipe it. We do not remove the OutputDir (ebins)
    % because those can be generated from other tools (such as rebar3 classic Erlang compiler).
    % Note that this means our builds can be incorrect in a certain way: we don't remove the corresponding
    % beam when an erlt source file is removed.
    rmrf(BuildDir),
    try
        case do_phase("scan", Args) of
            ok ->
                do_phase("compile", Args);
            error ->
                error
        end
    catch
        Class:Reason:Stacktrace ->
            clean(Args),
            erlang:raise(Class, Reason, Stacktrace)
    end;
invoke(#args{command = clean, build_dir = undefined}) ->
    erlt_build_util:throw_error("internal error: I don't know which build directory to clean");
invoke(#args{command = clean, output_dir = undefined}) ->
    erlt_build_util:throw_error("internal error: I don't know which output directory to clean");
invoke(#args{command = clean} = Args) ->
    clean(Args).

clean(#args{build_dir = BuildDir, output_dir = OutputDir}) ->
    rmrf(BuildDir),
    rmrf(OutputDir).

% calls erltc Phase on InputFiles in parallel
do_phase(
    Phase,
    #args{input_files = InputFiles, build_dir = BuildDir, output_dir = OutputDir} = Args
) ->
    mkdirp(BuildDir),
    mkdirp(OutputDir),
    % used by our team when updating built-in-types, see ../built_ins/Makefile
    case os:getenv("SKIP_BUILT_INS") of
        "TRUE" -> ok;
        _ -> write_built_ins(Phase, Args)
    end,
    F = fun(InputFile) -> do_file(Phase, Args, InputFile) end,
    case p_map(F, InputFiles) of
        {results, Results} ->
            case lists:member(error, maps:values(Results)) of
                true ->
                    error;
                false ->
                    ok
            end;
        {timeout, TimedOutFiles} ->
            clean(Args),
            io:format(
                standard_error,
                "Internal error: following files timed out during the ~s phase: ~p~n",
                [Phase, TimedOutFiles]
            ),
            error
    end.

do_file(
    Phase,
    #args{src_dir = SrcDir0, build_dir = BuildDir, output_dir = OutputDir, erlc_argv = ErlcArgv},
    InputFile
) ->
    SrcDir =
        case SrcDir0 of
            undefined -> ".";
            Dir -> Dir
        end,
    PhaseArgs =
        case Phase of
            "scan" ->
                % The scan phase produces .D files (readable by Ninja, Make, and humans) representing the dependency graph.
                % These files will be used in future for incremental builds.
                ["-MF", filename:join(BuildDir, InputFile ++ ".D")];
            _ ->
                []
        end,
    Args =
        [
            "--build-phase",
            Phase,
            "--build-dir",
            BuildDir,
            "-o",
            OutputDir
        ] ++ PhaseArgs ++ ErlcArgv ++ [filename:join(SrcDir, InputFile)],
    erltc:api(Args).

%% @doc write defs files and beams for built_in types and modules.
%% see ../built_ins/README.md for more information
write_built_ins("compile", #args{build_dir = BuildDir, output_dir = EbinDir}) ->
    [
        ok = file:write_file(filename:join(BuildDir, Basename), Contents)
     || {Basename, Contents} <- erlt_build_statics:built_in_defs()
    ],
    [
        ok = file:write_file(filename:join(EbinDir, Basename), Contents)
     || {Basename, Contents} <- erlt_build_statics:built_in_ebins()
    ],
    ok;
write_built_ins(_, #args{src_dir = _SrcDir}) ->
    ok.

%% UTILITIES

% Errors are not handled (nocatch)
-spec p_map(Cb, list(Key)) -> {results, #{Key => Ret}} | {timeout, list(Key)} when
    Cb :: fun((Key) -> Ret), Ret :: term().
p_map(F, List) ->
    p_map(F, List, #{}).

p_map(_, [], Results) ->
    {results, Results};
p_map(F, List, Results0) ->
    {L1, L2} = safe_split(concurrency(), List),
    [work(F, Key) || Key <- L1],
    ResList = [
        receive
            {Key, Res} -> {Key, Res}
        end
     || Key <- L1
    ],
    TimedOutKeys = [Key || {Key, timeout} <- ResList],
    case TimedOutKeys of
        [] ->
            Results1 = maps:merge(Results0, maps:from_list(ResList)),
            p_map(F, L2, Results1);
        _ ->
            {timeout, TimedOutKeys}
    end.

work(F, Key) ->
    Self = self(),
    spawn_link(fun() ->
        TimeoutHandler = self(),
        Child = spawn_link(fun() -> TimeoutHandler ! {Key, F(Key)} end),
        receive
            {Key, Val} ->
                Self ! {Key, Val}
        after ?TIMEOUT_FOR_FILE ->
            exit(Child, kill),
            Self ! {Key, timeout}
        end
    end).

concurrency() ->
    case erlang:system_info(logical_processors) of
        unknown -> 10;
        Concurrency -> Concurrency
    end.

mkdirp(Dirname) ->
    % We use a dummy file name
    % because built-in `ensure_dir(F)` only ensures the *parent* of F exists
    Dummy = "__never_created",
    filelib:ensure_dir(filename:join(Dirname, Dummy)).

rmrf(Dir) ->
    case file:del_dir_r(Dir) of
        ok ->
            ok;
        {error, enoent} ->
            ok;
        {error, Reason} ->
            io:format(standard_error, "could not remove directory ~p: ~p~n", [Dir, Reason]),
            error
    end.

safe_split(N, List) when length(List) < N ->
    {List, []};
safe_split(N, List) ->
    lists:split(N, List).
