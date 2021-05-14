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

% private types and constants shared between modules in this app

-define(SOURCE_FILE_EXTENSION, ".erlt").

% command-line args
-record(args, {
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
