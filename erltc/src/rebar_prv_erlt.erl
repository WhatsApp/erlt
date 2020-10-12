% This code was adapted from earlier work by Anton Lavrik @alavrik
-module(rebar_prv_erlt).

-export([init/1, run/3, compile_app/1, clean_app/1]).

-include("erlt_build_types.hrl").

-define(SRC_DIR, "src").

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State0) ->
    {ok, State1} = rebar_prv_erlt_compile:init(State0),
    rebar_prv_erlt_clean:init(State1).

% aborts when there is an error
-spec run(F, string(), rebar_state:t()) -> {ok, rebar_state:t()} when
    F :: fun((rebar_app_info:t()) -> ok | error).
run(F, Description, State) ->
    Apps =
        case rebar_state:current_app(State) of
            undefined ->
                rebar_state:project_apps(State);
            CurrentApp ->
                [CurrentApp]
        end,
    lists:foreach(fun validate_app_info/1, Apps),
    [
        case F(App) of
            ok ->
                ok;
            error ->
                rebar_utils:abort("~s failed", [Description])
        end
        || App <- Apps
    ],
    {ok, State}.

compile_app(AppInfo) ->
    RebarOpts = rebar_app_info:opts(AppInfo),
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    OutDir = rebar_app_info:out_dir(AppInfo),

    ErltOpts = get_erlt_opts(RebarOpts),
    CommonOptions = [
        "-I",
        filename:join(OutDir, "include"),
        "-pa",
        EbinDir
    ],
    handle_dir(
        "compile",
        AppInfo,
        ?SRC_DIR,
        EbinDir,
        CommonOptions,
        ErltOpts
    ).

clean_app(AppInfo) ->
    Options = make_erlt_dir_options(AppInfo, ?SRC_DIR),
    verbose_level() >= 1 andalso
        rebar_log:info("Cleaning ~s/~s", [rebar_app_info:name(AppInfo), ?SRC_DIR]),
    Argv = ["--build", "clean" | Options],
    call_erltc(Argv).

handle_dir(Task, AppInfo, SrcDir, OutputDir, CommonOptions, ErltOpts) ->
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
                    ["+" ++ to_string(Term) || Term <- ErltOpts],

            Argv = lists:append([
                ["--build", Task],
                Options,
                Sources
            ]),
            case call_erltc(Argv) of
                ok ->
                    % We run this *after* call_erlt_build(), because
                    % erlt_build will create "ebin" directory, without which
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
                    end;
                error ->
                    error
            end
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

-spec call_erltc([string]) -> ok | error.
call_erltc(Argv) ->
    Repro = string:join(Argv, " "),
    rebar_log:log(debug, "calling erlt_build. You can reproduce with:~n erltc ~s", [Repro]),
    erltc:api(Argv).

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

get_erlt_opts(RebarOpts) ->
    % we can copy the following features from rebar_opts:erl_opts if we want them:
    % - platform defines
    % - forcing debug_info to always be on
    rebar_opts:get(RebarOpts, erlt_opts, []).

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

to_string(X) when is_list(X) ->
    X;
to_string(X) when is_binary(X) ->
    binary_to_list(X);
to_string(X) ->
    lists:flatten(io_lib:format("~w", [X])).
