% private types and constants shared between modules in this app

% command-line args
-record(args, {
    % erlbuild <command>
    command :: compile | clean,
    % -j
    jobs :: undefined | string(),
    % -v
    verbose = 0 :: non_neg_integer(),
    % -o
    output_dir :: undefined | string(),
    % --build-dir
    build_dir :: undefined | string(),
    % --src-dir
    src_dir :: undefined | string(),
    % --makefile
    makefile :: undefined | string(),
    % --erlbuild
    erlbuild = "erlbuild",
    % --gen-only
    gen_only = false,
    % --erlc
    erlc,
    % --erlbuild-erlc
    erlbuild_erlc,
    % input file names
    input_files = [] :: [string()],
    % argv to pass to erlc
    erlc_argv = [] :: [string()],
    % whether to use incremental compilation
    incremental = false :: boolean()
}).

