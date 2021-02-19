-module(wa_corec).

-behaviour(provider).
-behaviour(rebar_compiler).

-export([
    init/1,
    context/1,
    needed_files/4,
    dependencies/3,
    compile/4,
    clean/2
]).

init(State) ->
    State1 = rebar_state:append_compilers(State, [wa_corec]),
    {ok, State1}.

context(AppInfo) ->
    OutMappings = [{".beam.core", rebar_app_info:ebin_dir(AppInfo)}],
    maps:put(out_mappings, OutMappings, rebar_compiler_erl:context(AppInfo)).

needed_files(_, ErlFiles, _, AppInfo) ->
    EBinDir = rebar_app_info:ebin_dir(AppInfo),
    {{[], []}, {[F || F <- ErlFiles, need_rebuild(F, EBinDir)], []}}.

need_rebuild(ErlFile, EBinDir) ->
    ModName = filename:basename(ErlFile, ".erl"),
    BeamFile = filename:join(EBinDir, ModName) ++ ".beam",
    CoreFile = filename:join(EBinDir, ModName) ++ ".beam.core",
    case filelib:is_regular(CoreFile) of
        false -> true;
        true -> filelib:last_modified(CoreFile) < filelib:last_modified(BeamFile)
    end.

dependencies(_, _, _) -> [].

compile(Source, [{_, OutDir}], _Config, _Opts) ->
    Mod = filename:basename(Source, ".erl"),
    BeamFile = filename:join(OutDir, Mod) ++ ".beam",
    CoreFile = filename:join(OutDir, Mod) ++ ".beam.core",
    rebar_log:log(debug, "corec:compile: ~p", [Source]),
    rebar_log:log(debug, "corec:compile: ~p", [BeamFile]),
    case get_core_forms(BeamFile) of
        {ok, Cerl} ->
            file:write_file(CoreFile, term_to_binary(Cerl));
        {error, Msg} ->
            rebar_log:log(error, Msg, []),
            rebar_compiler:error_tuple(Mod, Msg, [], [])
    end.

target_base(OutDir, Source) ->
    filename:join(OutDir, filename:basename(Source, ".erl")).

clean(Files, AppInfo) ->
    Dir = rebar_app_info:ebin_dir(AppInfo),
    [file:delete(target_base(Dir, F) ++ ".beam.core") || F <- Files],
    ok.

% adapted from dialyzer_utils.erl 84adefa331
get_core_forms(File) ->
    case beam_lib:chunks(File, [debug_info]) of
        {ok, {Module, [{debug_info, {debug_info_v1, Backend, Metadata}}]}} ->
            case Backend:debug_info(core_v1, Module, Metadata, src_compiler_opts()) of
                {ok, Core} -> {ok, dialyzer_clean_core:clean(Core)};
                {error, _} -> {error, "Could not get CErl for " ++ File}
            end;
        _ -> {error, "Could not get CErl for " ++ File}
    end.

% copied from dialyzer_utils.erl 84adefa331
src_compiler_opts() ->
    [
        no_copt,
        to_core,
        binary,
        return_errors,
        no_inline,
        strict_record_tests,
        strict_record_updates,
        dialyzer,
        no_spawn_compiler_process
    ].
