% This code was adapted from earlier work by Anton Lavrik @alavrik
-module(rebar3_erlt_prv).

-include("erlbuild_types.hrl").

% rebar3 plugin docs: https://www.rebar3.org/docs/tutorials/building_plugins/
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, erlt).
-define(DEPS, [compile, app_discovery]).
-define(SRC_DIR, "src").

-define(EXAMPLE,
    "build all apps in the project: `rebar3 erlt`~n\tbuild a single app rebar3 erlt <app-name>~n\tclean: `rebar3 erlt clean [<app name>]`"
).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider =
        providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, ?EXAMPLE},
            {opts, []},
            {short_desc, "build tool for ErlT"},
            {
                desc,
                "build tool for ErlT~n~n"
                ?EXAMPLE
            }
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    {Task, Args} = get_task_and_args(rebar_state:command_args(State)),
    AppInfos = get_app_infos(Args, rebar_state:project_apps(State)),
    [
        begin
            validate_app_info(AppInfo),
            handle_app(Task, AppInfo)
        end
        || AppInfo <- AppInfos
    ],
    {ok, State}.

get_task_and_args(AllArgs) ->
    case AllArgs of
        ["clean" | Args] -> {"clean", Args};
        ["compile" | Args] -> {"compile", Args};
        Args -> {"compile", Args}
    end.

get_app_infos([], ProjectApps) ->
    ProjectApps;
get_app_infos([Arg], ProjectApps) ->
    case
        lists:search(fun(App) -> binary_to_list(rebar_app_info:name(App)) =:= Arg end, ProjectApps)
    of
        {value, App} ->
            [App];
        false ->
            rebar_utils:abort(
                "expected the optional positional argument to be an app name, instead got: ~p. Project apps are: ~p",
                [Arg, [binary_to_list(rebar_app_info:name(App)) || App <- ProjectApps]]
            )
    end;
get_app_infos(Args, _ProjectApps) ->
    rebar_utils:abort("expected either no ppositional arguments or an app name, instead got: ~p", [
        Args
    ]).

handle_app(Task = "compile", AppInfo) ->
    RebarOpts = rebar_app_info:opts(AppInfo),
    ErlOpts = rebar_opts:erl_opts(RebarOpts),
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    OutDir = rebar_app_info:out_dir(AppInfo),

    ErltCompileFlags = [],
    % TODO: read erlt_opts from config. Use these instead of erl_opts.
    % See how erl_opts is implemented and mirror it.
    % ErlbuildCompileFlags = rebar_opts:get(RebarOpts, erlt_opts, ""),
    CommonOptions = [
        "-I",
        filename:join(OutDir, "include"),
        "-pa",
        EbinDir
    ],
    handle_dir(
        Task,
        AppInfo,
        ErltCompileFlags,
        ?SRC_DIR,
        EbinDir,
        CommonOptions,
        ErlOpts
    ),
    ok;
handle_app(_Task = "clean", AppInfo) ->
    Options = make_erlt_dir_options(AppInfo, ?SRC_DIR),
    verbose_level() >= 1 andalso
        rebar_log:info("Cleaning ~s/~s", [rebar_app_info:name(AppInfo), ?SRC_DIR]),
    Argv = ["clean" | Options],
    call_erlbuild(Argv).

handle_dir(Task, AppInfo, ErltCompileFlags, SrcDir, OutputDir, CommonOptions, ErlOpts) ->
    BaseDir = rebar_app_info:dir(AppInfo),
    FullSrcDir = filename:join(BaseDir, SrcDir),

    % this is the original logic from rebar_compiler:find_source_files() --
    % keeping it for now just in case we may need it
    %     SourceExtRe = "^(?!\\._).*\\.(erl|xrl|yrl)$",
    %     Recursive = rebar_dir:recursive(RebarOpts, SrcDir),
    %     SourcesPaths = rebar_utils:find_files(FullSrcDir, SourceExtRe, Recursive),
    %     Sources = [string:prefix(X, FullSrcDir ++ "/") || X <- SourcesPaths],
    %
    Sources = filelib:wildcard("*" ++ ?SOURCE_FILE_EXTENSION, FullSrcDir),
    case Sources of
        % no sources to compile
        [] ->
            ok;
        _ ->
            verbose_level() >= 1 andalso
                rebar_log:info("Compiling ~s/~s", [rebar_app_info:name(AppInfo), SrcDir]),

            Options =
                make_erlt_dir_options(AppInfo, SrcDir) ++
                    CommonOptions ++
                    % options defined in rebar.config, or injected by rebar -- keeping them as-is, i.e. represented as Erlang terms
                    ["+" ++ to_string(Term) || Term <- ErlOpts],

            Argv = lists:append([
                [Task],
                [ErltCompileFlags || ErltCompileFlags =/= ""],
                Options,
                Sources
            ]),

            call_erlbuild(Argv),

            % We run this *after* call_erlbuild(), because
            % erlbuild will create "ebin" directory, without which
            % code:add_patha() would return {error,bad_directory}
            AbsOutputDir = filename:absname(OutputDir),
            case code:add_patha(AbsOutputDir) of
                true ->
                    ok;
                AddPathError ->
                    rebar_utils:error("erlt: code:add_patha(~s) returned error: ~p", [
                        AbsOutputDir,
                        AddPathError
                    ])
            end,

            ok
    end.

make_erlt_dir_options(AppInfo, SrcDir) ->
    OutDir = rebar_app_info:out_dir(AppInfo),
    BuildDir = filename:join(OutDir, "build"),
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    BaseDir = rebar_app_info:dir(AppInfo),
    FullSrcDir = filename:join(BaseDir, SrcDir),

    VerboseOption =
        case verbose_level() of
            0 -> [];
            VerboseLevel -> ["-v" ++ integer_to_list(VerboseLevel)]
        end,

    VerboseOption ++
        [
            "--src-dir",
            FullSrcDir,
            "--build-dir",
            BuildDir,
            "-o",
            EbinDir
        ].

call_erlbuild(Argv) ->
    rebar_log:log(debug, "calling erlbuild with args: ~p", [Argv]),
    % erlbuild expected to either return OK or halt(nonzero)
    ok = erlbuild:main(Argv).

validate_app_info(AppInfo) ->
    RebarOpts = rebar_app_info:opts(AppInfo),
    case rebar_opts:get(RebarOpts, erl_first_files, []) of
        [] ->
            ok;
        _ ->
            rebar_utils:abort("erlt: erl_first_files is not supported", [])
    end,
    case rebar_dir:src_dirs(RebarOpts, [?SRC_DIR]) of
        [?SRC_DIR] ->
            ok;
        SrcDirs ->
            rebar_utils:abort("erlt: invalid src_dirs (only [\"src\"] is supported): ~p", [SrcDirs])
    end.

verbose_level() ->
    case os:getenv("VERBOSE") of
        false ->
            0;
        "" ->
            0;
        Str ->
            try
                Int = list_to_integer(Str),
                case Int >= 0 of
                    true -> Int;
                    false -> 0
                end
            catch
                _:_ ->
                    % some sensible max
                    9
            end
    end.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).

to_string(X) when is_list(X) ->
    X;
to_string(X) when is_binary(X) ->
    binary_to_list(X);
to_string(X) ->
    lists:flatten(io_lib:format("~w", [X])).
