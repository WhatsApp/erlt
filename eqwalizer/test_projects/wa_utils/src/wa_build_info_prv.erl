-module(wa_build_info_prv).

-behaviour(provider).

-export([init/1, do/1]).

init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, build_info},
        {module, wa_build_info_prv},
        {bare, true},
        {deps, [compile]},
        {example, "rebar3 build_info"},
        {short_desc, "Print build_info."},
        {desc, "Print build_info."},
        {opts, [{to, $t, "to", {string, undefined}, "file to write buid_info in ETF"}]}])),
    {ok, State1}.

do(State) ->
    Apps = all_apps(State),
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    To = proplists:get_value(to, RawOpts),
    Dirs = ebin_dirs(Apps, State),
    ToPrint = mk_paths(Dirs),
    Data =
        #{
            apps => [{list_to_binary(App), list_to_binary(Dir)} || {App, Dir} <- ToPrint],
            otp_root => list_to_binary(code:lib_dir())
        },
    case To of
        undefined ->
            rebar_log:log(info, "Build info: ~p~n", [Data]);
        File ->
            ok = file:write_file(File, term_to_binary(Data)),
            rebar_log:log(info, "Build info written to: ~ts~n", [File])
    end,
    {ok, State}.

all_apps(State) ->
    ProjectDeps = project_deps(State),
    ProjectApps = rebar_state:project_apps(State),
    lists:map(fun(A) -> binary_to_list(rebar_app_info:name(A)) end, ProjectApps) ++ ProjectDeps.

ebin_dirs(Apps, State) ->
    lists:map(
        fun(App) ->
            {App, [
                io_lib:format("~ts/~ts/ebin", [rebar_dir:checkouts_out_dir(State), App]),
                io_lib:format("~ts/~ts/ebin", [rebar_dir:deps_dir(State), App])
            ]}
        end,
        Apps
    ).

name(App) when is_tuple(App) -> element(1, App);
name(Name) when is_binary(Name); is_list(Name); is_atom(Name) -> Name.

normalize(AppName) when is_list(AppName) -> AppName;
normalize(AppName) when is_atom(AppName) -> atom_to_list(AppName);
normalize(AppName) when is_binary(AppName) -> binary_to_list(AppName).

%% Copy-pasta from rebar_prv_path
project_deps(State) ->
    Profiles = rebar_state:current_profiles(State),
    DepList = lists:foldl(
        fun(Profile, Acc) -> rebar_state:get(State, {deps, Profile}, []) ++ Acc end,
        [],
        Profiles
    ),
    LockList = lists:foldl(
        fun(Profile, Acc) -> rebar_state:get(State, {locks, Profile}, []) ++ Acc end,
        [],
        Profiles
    ),
    Deps = [normalize(name(Dep)) || Dep <- DepList ++ LockList],
    lists:usort(Deps).

mk_paths(KVs) ->
    mk_paths(KVs, []).

mk_paths([], Acc) ->
    lists:reverse(Acc);
mk_paths([{App, Paths} | KVs], Acc) ->
    RealPaths = lists:filter(fun(P) -> ec_file:is_dir(P) end, Paths),
    Acc1 =
        case RealPaths of
            [Path] -> [{App, Path} | Acc];
            _Other -> Acc
        end,
    mk_paths(KVs, Acc1).
