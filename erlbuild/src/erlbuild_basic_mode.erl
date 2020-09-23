-module(erlbuild_basic_mode).

-include("erlbuild_types.hrl").

% timeout for scanning or building a file
-define(TIMEOUT_FOR_FILE, 3000).

-export([invoke/1]).

invoke(#args{command = compile} = Args) ->
    do_phase("scan", Args),
    do_phase("compile", Args),
    ok;
invoke(#args{command = clean, build_dir = undefined}) ->
    erlbuild_util:throw_error("I don't know which directory to clean");
invoke(#args{command = clean, build_dir = BuildDir}) ->
    file:del_dir_r(BuildDir).

% calls erltc Phase on InputFiles in parallel
do_phase(Phase, #args{input_files = InputFiles} = Args) ->
    p_each(
        fun(InputFile) -> do_file(Phase, Args, InputFile) end,
        Phase,
        InputFiles,
        concurrency(),
        ?TIMEOUT_FOR_FILE
    ).

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
p_each(_, _, [], _, _) ->
    ok;
p_each(F, DisplayName, List, Concurrency, TimeoutForFile) ->
    {L1, L2} = safe_split(Concurrency, List),
    Pid = self(),
    [spawn_link(fun() -> Pid ! {Item, F(Item)} end) || Item <- L1],
    [
        receive
            {Item, _Res} ->
                ok
        after TimeoutForFile ->
            erlbuild_util:throw_error("timed out waiting for ~p of ~p ~n", [DisplayName, Item])
        end
        || Item <- L1
    ],
    p_each(F, DisplayName, L2, Concurrency, TimeoutForFile).

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

safe_split(N, List) when length(List) < N ->
    {List, []};
safe_split(N, List) ->
    lists:split(N, List).
