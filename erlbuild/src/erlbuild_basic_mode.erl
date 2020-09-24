-module(erlbuild_basic_mode).

-include("erlbuild_types.hrl").

% timeout for scanning or building a file
-define(TIMEOUT_FOR_FILE, 3000).

-export([invoke/1]).

invoke(#args{command = compile} = Args) ->
    clean(Args),
    try
        do_phase("scan", Args),
        do_phase("compile", Args)
    catch
        Class:Reason:Stacktrace ->
            clean(Args),
            erlang:raise(Class, Reason, Stacktrace)
    end,
    ok;
invoke(#args{command = clean, build_dir = undefined}) ->
    erlbuild_util:throw_error("internal error: I don't know which build directory to clean");
invoke(#args{command = clean, output_dir = undefined}) ->
    erlbuild_util:throw_error("internal error: I don't know which output directory to clean");
invoke(#args{command = clean} = Args) ->
    clean(Args).

clean(#args{build_dir = BuildDir, output_dir = OutputDir}) ->
    rmrf(BuildDir),
    rmrf(OutputDir).

% calls erltc Phase on InputFiles in parallel
do_phase(Phase, #args{input_files = InputFiles} = Args) ->
    F = fun(InputFile) -> do_file(Phase, Args, InputFile) end,
    case p_each(F, InputFiles) of
        ok ->
            ok;
        {timeout, TimedOutFiles} ->
            clean(Args),
            io:format(
                standard_error,
                "Internal error: following files timed out during the ~s phase: ~p~n",
                [Phase, TimedOutFiles]
            ),
            erlang:halt(3)
    end.

do_file(
    Phase,
    #args{build_dir = BuildDir, output_dir = OutputDir, erlc_argv = ErlcArgv},
    InputFile
) ->
    mkdirp(BuildDir),
    mkdirp(OutputDir),
    Args =
        [
            "--build-phase",
            Phase,
            "--build-dir",
            BuildDir
        ] ++ ErlcArgv ++ [InputFile],
    case erltc:api(Args) of
        ok ->
            ok;
        error ->
            % the actual error message is logged to stdio by erltc
            % so we simply halt
            erlang:halt(3)
    end,
    ok.

%% UTILITIES

% Errors are not handled (nocatch)
% and we ignore return values
-spec p_each(Cb, list(Key)) -> ok | {timeout, list(Key)} when
    Cb :: fun((Key) -> Ret), Ret :: term().
p_each(_, []) ->
    ok;
p_each(F, List) ->
    {L1, L2} = safe_split(concurrency(), List),
    [work(F, Key) || Key <- L1],
    Results = [
        receive
            {Key, Res} -> {Key, Res}
        end
        || Key <- L1
    ],
    TimedOutKeys = [Key || {Key, timeout} <- Results],
    case length(TimedOutKeys) of
        0 ->
            p_each(F, L2);
        _ ->
            {timeout, TimedOutKeys}
    end.

work(F, Key) ->
    Self = self(),
    spawn_link(fun() ->
        TimeoutHandler = self(),
        Child = spawn_link(fun() -> TimeoutHandler ! {Key, {ok, F(Key)}} end),
        receive
            {Key, {ok, Val}} ->
                Self ! {Key, {ok, Val}}
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
            erlang:halt(1)
    end.

safe_split(N, List) when length(List) < N ->
    {List, []};
safe_split(N, List) ->
    lists:split(N, List).
