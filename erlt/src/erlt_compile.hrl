%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2020. All Rights Reserved.
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
%%
%% %CopyrightEnd%

%% The compile state record.
-record(compile, {
    %% our added fields
    build_dir :: undefined | file:filename(),
    original_forms,
    global_defs :: undefined | erlt_defs:defs(),
    variable_state :: undefined | erlt:var_state(),
    % path to .defs file (contains specs, types, enums, and structs)
    defs_file :: undefined | file:filename(),
    etf_file :: undefined | file:filename(),
    % whether we have written a defs file to disk
    has_written_defs_file = false :: boolean(),

    %% standard fields
    filename = "" :: file:filename(),
    dir = "" :: file:filename(),
    base = "" :: file:filename(),
    ifile = "" :: file:filename(),
    ofile = "" :: file:filename(),
    module = [] :: module() | [],
    core_code = [] :: cerl:c_module() | [],
    %Abstract code for debugger.
    abstract_code = [] :: abstract_code(),
    %Options for compilation
    options = [] :: [option()],
    %Options for module_info
    mod_options = [] :: [option()],
    encoding = none :: none | epp:source_encoding(),
    errors = [] :: [err_warn_info()],
    warnings = [] :: [err_warn_info()],
    extra_chunks = [] :: [{binary(), binary()}]
}).
