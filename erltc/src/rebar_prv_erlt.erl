% This code was adapted from earlier work by Anton Lavrik @alavrik
-module(rebar_prv_erlt).

-export([init/1, run/2, compile_app/1, clean_app/1]).

-include("erlt_build_types.hrl").

-define(SRC_DIR, "src").

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State0) ->
    {ok, State1} = rebar_prv_erlt_compile:init(State0),
    rebar_prv_erlt_clean:init(State1).

run(F, State) ->
    Args = rebar_state:command_args(State),
    AppInfos = get_app_infos(Args, rebar_state:project_apps(State)),
    [
        begin
            validate_app_info(AppInfo),
            F(AppInfo)
        end
        || AppInfo <- AppInfos
    ],
    ok.

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

compile_app(AppInfo) ->
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
        "compile",
        AppInfo,
        ErltCompileFlags,
        ?SRC_DIR,
        EbinDir,
        CommonOptions,
        ErlOpts
    ),
    ok.

clean_app(AppInfo) ->
    Options = make_erlt_dir_options(AppInfo, ?SRC_DIR),
    verbose_level() >= 1 andalso
        rebar_log:info("Cleaning ~s/~s", [rebar_app_info:name(AppInfo), ?SRC_DIR]),
    Argv = ["clean" | Options],
    call_erlt_build(Argv).

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

            call_erlt_build(Argv),

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

call_erlt_build(Argv) ->
    rebar_log:log(debug, "calling erlt_build with args: ~p", [Argv]),
    % erlt_build expected to either return OK or halt(nonzero)
    ok = erlt_build:main(Argv).

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

to_string(X) when is_list(X) ->
    X;
to_string(X) when is_binary(X) ->
    binary_to_list(X);
to_string(X) ->
    lists:flatten(io_lib:format("~w", [X])).
