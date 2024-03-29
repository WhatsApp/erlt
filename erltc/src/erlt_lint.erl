%%% -*- erlang-indent-level: 4 -*-
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
%%
%% Do necessary checking of Erlang code.

-module(erlt_lint).

-export([module/4, format_error/1]).
-export([is_pattern_expr/1]).
-export([is_guard_expr/1]).
-export([bool_option/4, value_option/3, value_option/7]).

-import(lists, [
    all/2,
    any/2,
    foldl/3,
    foldr/3,
    map/2,
    mapfoldl/3,
    member/2,
    reverse/1
]).

%% Removed functions

-removed([{modify_line, 2, "use erl_parse:map_anno/2 instead"}]).

-define(IS_FUNCTION(F), (F =:= function orelse F =:= unchecked_function)).

-define(IS_STRUCT(S), (S =:= struct orelse S =:= exception orelse S =:= message)).

%% keeping only our added errors and warnings; the real linter will run later

%% bool_option(OnOpt, OffOpt, Default, Options) -> boolean().
%% value_option(Flag, Default, Options) -> Value.
%% value_option(Flag, Default, OnOpt, OnVal, OffOpt, OffVal, Options) ->
%%              Value.
%%  The option handling functions.

-spec bool_option(atom(), atom(), boolean(), [compile:option()]) -> boolean().
bool_option(On, Off, Default, Opts) ->
    foldl(
        fun
            (Opt, _Def) when Opt =:= On -> true;
            (Opt, _Def) when Opt =:= Off -> false;
            (_Opt, Def) -> Def
        end,
        Default,
        Opts
    ).

value_option(Flag, Default, Opts) ->
    foldl(
        fun
            ({Opt, Val}, _Def) when Opt =:= Flag -> Val;
            (_Opt, Def) -> Def
        end,
        Default,
        Opts
    ).

value_option(Flag, Default, On, OnVal, Off, OffVal, Opts) ->
    foldl(
        fun
            ({Opt, Val}, _Def) when Opt =:= Flag -> Val;
            (Opt, _Def) when Opt =:= On -> OnVal;
            (Opt, _Def) when Opt =:= Off -> OffVal;
            (_Opt, Def) -> Def
        end,
        Default,
        Opts
    ).

%% The maximum number of arguments allowed for a function.

-define(MAX_ARGUMENTS, 255).

%% The error and warning info structures, {Line,Module,Descriptor},
%% are kept in their seperate fields in the lint state record together
%% with the name of the file (when a new file is entered, marked by
%% the 'file' attribute, then the field 'file' of the lint record is
%% set). At the end of the run these lists are packed into a list of
%% {FileName,ErrorDescList} pairs which are returned.

-include_lib("stdlib/include/erl_bits.hrl").

%%-define(DEBUGF(X,Y), io:format(X, Y)).
-define(DEBUGF(X, Y), void).

% a convenient alias
-type line() :: erl_anno:anno().
% function+arity
-type fa() :: {atom(), arity()}.
% type+arity
-type ta() :: {atom(), arity()}.
-type module_or_mfa() :: module() | mfa().

-type gexpr_context() :: 'guard' | 'bin_seg_size' | 'map_key' | 'field_default'.

-record(typeinfo, {attr, line}).
%% Usage of records, functions, and imports. The variable table, which
%% is passed on as an argument, holds the usage of variables.
-record(usage, {
    %Who calls who
    calls = maps:new(),
    %Actually imported functions
    imported = [],
    %Actually imported types
    imported_types = [],
    %Used type definitions
    used_types = maps:new() :: #{ta() := line()}
}).

%% Define the lint state record.
%% 'called' and 'exports' contain {Line, {Function, Arity}},
%% the other function collections contain {Function, Arity}.
-record(lint, {
    state = start :: 'start' | 'attribute' | 'function',
    % Module
    module = '',
    % Behaviour
    behaviour = [],
    % Exports
    exports = gb_sets:empty() :: gb_sets:set(fa()),
    % Imports
    imports = [] :: orddict:orddict(fa(), module()),
    % Compile flags
    compile = [],
    % Struct definitions
    structs = #{} :: #{atom() => Def :: term()},
    % All defined functions (prescanned)
    locals = gb_sets:empty() :: gb_sets:set(fa()),
    % Functions explicitly not autoimported
    no_auto = gb_sets:empty() :: gb_sets:set(fa()) | 'all',
    % Defined fuctions
    defined = gb_sets:empty() :: gb_sets:set(fa()),
    % On-load function
    on_load = [] :: [fa()],
    % Line for on_load
    on_load_line = erl_anno:new(0) :: erl_anno:anno(),
    % Exported functions named as BIFs
    clashes = [],
    % Not considered deprecated
    not_deprecated = [],
    % Not considered removed
    not_removed = gb_sets:empty() :: gb_sets:set(module_or_mfa()),
    % Current function
    func = [],
    % Warn format calls
    warn_format = 0,
    % All enabled warnings (ordset).
    enabled_warnings = [],
    % All no warn bif clashes (ordset).
    nowarn_bif_clash = [],
    % Current errors
    errors = [],
    % Current warnings
    warnings = [],
    % From last file attribute
    file = "" :: string(),
    % true if qlc.hrl included
    xqlc = false :: boolean(),
    % Called functions
    called = [] :: [{fa(), line()}],
    usage = #usage{} :: #usage{},
    % Type specifications
    specs = maps:new() :: #{mfa() => line()},
    % Callback types
    callbacks = maps:new() :: #{mfa() => line()},
    % Optional callbacks
    optional_callbacks = maps:new() :: #{mfa() => line()},
    % Type definitions
    types = maps:new() :: #{ta() => #typeinfo{}},
    % Imported types
    imp_types = [] :: orddict:orddict(fa(), module()),
    % Exported types
    exp_types = gb_sets:empty() :: gb_sets:set(ta()),
    %Variables in binary pattern
    bvt = none :: 'none' | [any()],
    %Context of guard expression
    gexpr_context = guard :: gexpr_context(),
    %Is the context type checked
    type_checked = true :: boolean(),
    % Enum definitions
    enums = #{} :: #{atom() => #{atom() => term()}},
    % Global definition database
    defs_db :: erlt_defs:defs(),
    % Used to track recursive struct & enum defaults
    default_expansion = skip :: skip | sets:set(atom() | {atom(), atom()})
}).

-type lint_state() :: #lint{}.
-type error_description() :: term().
-type error_info() :: {erl_anno:line(), module(), error_description()}.

%% format_error(Error)
%%  Return a string describing the error.

-spec format_error(ErrorDescriptor) -> io_lib:chars() when ErrorDescriptor :: error_description().
format_error(undefined_module) ->
    "no module definition";
format_error(redefine_module) ->
    "redefining module";
format_error(pmod_unsupported) ->
    "parameterized modules are no longer supported";
%% format_error({redefine_mod_import, M, P}) ->
%%     io_lib:format("module '~s' already imported from package '~s'", [M, P]);
format_error(non_latin1_module_unsupported) ->
    "module names with non-latin1 characters are not supported";
format_error(invalid_call) ->
    "invalid function call";
format_error({attribute, A}) ->
    io_lib:format("attribute ~tw after function definitions", [A]);
format_error({missing_qlc_hrl, A}) ->
    io_lib:format("qlc:q/~w called, but \"qlc.hrl\" not included", [A]);
format_error({redefine_import, {{F, A}, M}}) ->
    io_lib:format("function ~tw/~w already imported from ~w", [F, A, M]);
format_error({redefine_import_type, {{F, _}, M}}) ->
    io_lib:format("type ~tw already imported from ~w", [F, M]);
format_error({import_type_duplicate, {F, _}}) ->
    io_lib:format("type ~tw imported twice", [F]);
format_error({bad_inline, {F, A}}) ->
    io_lib:format("inlined function ~tw/~w undefined", [F, A]);
format_error({invalid_deprecated, D}) ->
    io_lib:format("badly formed deprecated attribute ~tw", [D]);
format_error({bad_deprecated, {F, A}}) ->
    io_lib:format("deprecated function ~tw/~w undefined or not exported", [F, A]);
format_error({invalid_removed, D}) ->
    io_lib:format("badly formed removed attribute ~tw", [D]);
format_error({bad_removed, {F, A}}) when F =:= '_'; A =:= '_' ->
    io_lib:format(
        "at least one function matching ~tw/~w is still exported",
        [F, A]
    );
format_error({bad_removed, {F, A}}) ->
    io_lib:format(
        "removed function ~tw/~w is still exported",
        [F, A]
    );
format_error({bad_nowarn_unused_function, {F, A}}) ->
    io_lib:format("function ~tw/~w undefined", [F, A]);
format_error({bad_nowarn_bif_clash, {F, A}}) ->
    io_lib:format("function ~tw/~w undefined", [F, A]);
format_error(disallowed_nowarn_bif_clash) ->
    io_lib:format(
        "compile directive nowarn_bif_clash is no longer allowed,~n"
        " - use explicit module names or -compile({no_auto_import, [F/A]})",
        []
    );
format_error({bad_on_load, Term}) ->
    io_lib:format("badly formed on_load attribute: ~tw", [Term]);
format_error(multiple_on_loads) ->
    "more than one on_load attribute";
format_error({bad_on_load_arity, {F, A}}) ->
    io_lib:format("function ~tw/~w has wrong arity (must be 0)", [F, A]);
format_error({undefined_on_load, {F, A}}) ->
    io_lib:format("function ~tw/~w undefined", [F, A]);
format_error(nif_inline) ->
    "inlining is enabled - local calls to NIFs may call their Erlang "
    "implementation instead";
format_error(export_all) ->
    "export_all flag enabled - all functions will be exported";
format_error({duplicated_export, {F, A}}) ->
    io_lib:format("function ~tw/~w already exported", [F, A]);
format_error({unused_import, {{F, A}, M}}) ->
    io_lib:format("import ~w:~tw/~w is unused", [M, F, A]);
format_error({unused_import_type, {{F, A}, M}}) ->
    io_lib:format("imported type ~w:~tw/~w is unused", [M, F, A]);
format_error({undefined_function, {F, A}}) ->
    io_lib:format("function ~tw/~w undefined", [F, A]);
format_error({undefined_checked, {M, F, A}}) ->
    io_lib:format("checked spec for function ~tw:~tw/~w undefined", [M, F, A]);
format_error({private_function, {M, F, A}}) ->
    io_lib:format("function ~tw:~tw/~w is not exported", [M, F, A]);
format_error({unchecked_function, {M, F, A}}) ->
    io_lib:format("function ~tw:~tw/~w is unchecked (called from checked code)", [M, F, A]);
format_error({redefine_function, {F, A}}) ->
    io_lib:format("function ~tw/~w already defined", [F, A]);
format_error({define_import, {F, A}}) ->
    io_lib:format("defining imported function ~tw/~w", [F, A]);
format_error({unused_function, {F, A}}) ->
    io_lib:format("function ~tw/~w is unused", [F, A]);
format_error({call_to_redefined_bif, {F, A}}) ->
    io_lib:format(
        "ambiguous call of overridden auto-imported BIF ~w/~w~n"
        " - use erlang:~w/~w or \"-compile({no_auto_import,[~w/~w]}).\" "
        "to resolve name clash",
        [F, A, F, A, F, A]
    );
format_error({call_to_redefined_old_bif, {F, A}}) ->
    io_lib:format(
        "ambiguous call of overridden pre R14 auto-imported BIF ~w/~w~n"
        " - use erlang:~w/~w or \"-compile({no_auto_import,[~w/~w]}).\" "
        "to resolve name clash",
        [F, A, F, A, F, A]
    );
format_error({redefine_old_bif_import, {F, A}}) ->
    io_lib:format(
        "import directive overrides pre R14 auto-imported BIF ~w/~w~n"
        " - use \"-compile({no_auto_import,[~w/~w]}).\" "
        "to resolve name clash",
        [F, A, F, A]
    );
format_error({redefine_bif_import, {F, A}}) ->
    io_lib:format(
        "import directive overrides auto-imported BIF ~w/~w~n"
        " - use \"-compile({no_auto_import,[~w/~w]}).\" to resolve name clash",
        [F, A, F, A]
    );
format_error({redefine_builtin_type_import, {F, A}}) ->
    io_lib:format(
        "import directive overrides auto-imported builtin type ~w/~w~n"
        " - use \"-compile({no_auto_import,[~w/~w]}).\" to resolve name clash",
        [F, A, F, A]
    );
format_error({deprecated, MFA, String, Rel}) ->
    io_lib:format(
        "~s is deprecated and will be removed in ~s; ~s",
        [format_mfa(MFA), Rel, String]
    );
format_error({deprecated, MFA, String}) when is_list(String) ->
    io_lib:format("~s is deprecated; ~s", [format_mfa(MFA), String]);
format_error({deprecated_type, {M1, F1, A1}, String}) when is_list(String) ->
    io_lib:format(
        "the type ~p:~p~s is deprecated; ~s",
        [M1, F1, gen_type_paren(A1), String]
    );
format_error({removed, MFA, ReplacementMFA, Rel}) ->
    io_lib:format(
        "call to ~s will fail, since it was removed in ~s; "
        "use ~s",
        [format_mfa(MFA), Rel, format_mfa(ReplacementMFA)]
    );
format_error({removed, MFA, String}) when is_list(String) ->
    io_lib:format("~s is removed; ~s", [format_mfa(MFA), String]);
format_error({removed_type, MNA, String}) ->
    io_lib:format("the type ~s is removed; ~s", [format_mna(MNA), String]);
format_error({obsolete_guard, {F, A}}) ->
    io_lib:format("~p/~p obsolete (use is_~p/~p)", [F, A, F, A]);
format_error({obsolete_guard_overridden, Test}) ->
    io_lib:format(
        "obsolete ~s/1 (meaning is_~s/1) is illegal when "
        "there is a local/imported function named is_~p/1 ",
        [Test, Test, Test]
    );
format_error({too_many_arguments, Arity}) ->
    io_lib:format(
        "too many arguments (~w) - "
        "maximum allowed is ~w",
        [Arity, ?MAX_ARGUMENTS]
    );
%% --- patterns and guards ---
format_error(illegal_pattern) ->
    "illegal pattern";
format_error(illegal_map_key) ->
    "illegal map key in pattern";
format_error(illegal_bin_pattern) ->
    "binary patterns cannot be matched in parallel using '='";
format_error(illegal_expr) ->
    "illegal expression";
format_error(illegal_caret) ->
    "operator ^ is only allowed in patterns";
format_error({illegal_guard_local_call, {F, A}}) ->
    io_lib:format("call to local/imported function ~tw/~w is illegal in guard", [F, A]);
format_error(illegal_guard_expr) ->
    "illegal guard expression";
%% --- maps ---
format_error(illegal_map_construction) ->
    "only association operators '=>' are allowed in map construction";
format_error(map_syntax_disallowed) ->
    "map syntax is only allowed in unchecked functions";
%% --- enums ---
format_error({redefine_enum, T, C}) ->
    io_lib:format("variant ~tw already defined in enum ~tw", [C, T]);
format_error({private_enum, N}) ->
    io_lib:format("enum ~ts is not exported", [format_name(N)]);
format_error({undefined_enum, N}) ->
    io_lib:format("enum ~ts undefined", [format_name(N)]);
format_error({undefined_enum_variant, N, V}) ->
    io_lib:format("enum ~ts has no variant ~tw", [format_name(N), V]);
format_error(invalid_name) ->
    "only fully qualified remote 'module:name' or local 'name' references are supported";
format_error(unqualified_enum) ->
    "bare enums (or unquoted atoms) are not supported";
%% --- structs ---
format_error({undefined_struct, N}) ->
    io_lib:format("struct ~ts undefined", [format_name(N)]);
format_error({private_struct, N}) ->
    io_lib:format("struct ~ts is not exported", [format_name(N)]);
format_error({reuse_shape_field, F}) ->
    io_lib:format("field ~tw used more than once in the same shape", [F]);
format_error({redefine_shape_field, F}) ->
    io_lib:format("field ~tw already defined for this shape type", [F]);
format_error({redefine_field, R, F}) ->
    io_lib:format("field ~tw already defined in ~ts", [F, format_reference(R)]);
format_error(bad_multi_field_init) ->
    io_lib:format("'_' initializes no omitted fields", []);
format_error({undefined_field, R, F}) ->
    io_lib:format("field ~tw undefined in ~ts", [F, format_reference(R)]);
format_error({recursive_field, R, F}) ->
    io_lib:format("field ~tw of ~ts contains recursive default value", [F, format_reference(R)]);
format_error({no_field_value, R, F}) ->
    io_lib:format("field ~tw of ~ts has no initializer or default value", [F, format_reference(R)]);
format_error({no_fields_expected, R}) ->
    io_lib:format("no fields defined for ~ts", [format_reference(R)]);
format_error({no_fields_given, R}) ->
    io_lib:format("expected fields for ~ts", [format_reference(R)]);
format_error({illegal_field_default_call, {F, A}}) ->
    io_lib:format("call to local/imported function ~tw/~w is illegal in defaults", [F, A]);
format_error(illegal_field_default) ->
    "illegal default expression";
format_error({extra_positional_field, Actual, Expected, R}) ->
    Msg = "~p positional fields provided when ~p expected in ~ts",
    io_lib:format(Msg, [Actual, Expected, format_reference(R)]);
format_error({missing_positional_field, Actual, Expected, R}) ->
    Msg = "~p positional fields provided when ~p expected in ~ts",
    io_lib:format(Msg, [Actual, Expected, format_reference(R)]);
format_error(positional_after_labelled_field) ->
    "positional field defined after labelled fields";
%% --- variables ----
format_error({unbound_var, V}) ->
    io_lib:format("variable ~w is unbound", [V]);
format_error({unsafe_var, V, {What, Where}}) ->
    io_lib:format("variable ~w unsafe in ~w ~s", [V, What, format_where(Where)]);
format_error({exported_var, V, {What, Where}}) ->
    io_lib:format("variable ~w exported from ~w ~s", [V, What, format_where(Where)]);
format_error({unpinned_var, V}) ->
    io_lib:format("variable ~w is already bound - must write '^~s' to use value in pattern", [V, V]);
format_error({shadowed_var, V, In}) ->
    io_lib:format("variable ~w shadowed in ~w", [V, In]);
format_error({unused_var, V}) ->
    io_lib:format("variable ~w is unused", [V]);
format_error({stacktrace_guard, V}) ->
    io_lib:format("stacktrace variable ~w must not be used in a guard", [V]);
format_error({stacktrace_bound, V}) ->
    io_lib:format("stacktrace variable ~w must not be previously bound", [V]);
format_error(illegal_catch_kind) ->
    "only variables and atoms throw, exit, and error are allowed in kind position of catch clauses";
format_error(illegal_catch_stack) ->
    "only unbound variables are allowed in stacktrace position of catch clauses";
format_error(illegal_checked_catch) ->
    "checked code does not support capturing class or stacktrace in catch clauses";
%% --- binaries ---
format_error({undefined_bittype, Type}) ->
    io_lib:format("bit type ~tw undefined", [Type]);
format_error({bittype_mismatch, Val1, Val2, What}) ->
    io_lib:format("conflict in ~s specification for bit field: '~p' and '~p'", [
        What,
        Val1,
        Val2
    ]);
format_error(bittype_unit) ->
    "a bit unit size must not be specified unless a size is specified too";
format_error(illegal_bitsize) ->
    "illegal bit size";
format_error({illegal_bitsize_local_call, {F, A}}) ->
    io_lib:format(
        "call to local/imported function ~tw/~w is illegal in a size "
        "expression for a binary segment",
        [F, A]
    );
format_error(non_integer_bitsize) ->
    "a size expression in a pattern evaluates to a non-integer value; "
    "this pattern cannot possibly match";
format_error(unsized_binary_not_at_end) ->
    "a binary field without size is only allowed at the end of a binary pattern";
format_error(typed_literal_string) ->
    "a literal string in a binary pattern must not have a type or a size";
format_error(utf_bittype_size_or_unit) ->
    "neither size nor unit must be given for segments of type utf8/utf16/utf32";
format_error({bad_bitsize, Type}) ->
    io_lib:format("bad ~s bit size", [Type]);
format_error(unsized_binary_in_bin_gen_pattern) ->
    "binary fields without size are not allowed in patterns of bit string generators";
%% --- behaviours ---
format_error({conflicting_behaviours, {Name, Arity}, B, FirstL, FirstB}) ->
    io_lib:format(
        "conflicting behaviours - callback ~tw/~w required by both '~p' "
        "and '~p' ~s",
        [Name, Arity, B, FirstB, format_where(FirstL)]
    );
format_error({undefined_behaviour_func, {Func, Arity}, Behaviour}) ->
    io_lib:format("undefined callback function ~tw/~w (behaviour '~w')", [
        Func,
        Arity,
        Behaviour
    ]);
format_error({undefined_behaviour, Behaviour}) ->
    io_lib:format("behaviour ~tw undefined", [Behaviour]);
format_error({undefined_behaviour_callbacks, Behaviour}) ->
    io_lib:format("behaviour ~w callback functions are undefined", [Behaviour]);
format_error({ill_defined_behaviour_callbacks, Behaviour}) ->
    io_lib:format("behaviour ~w callback functions erroneously defined", [Behaviour]);
format_error({ill_defined_optional_callbacks, Behaviour}) ->
    io_lib:format("behaviour ~w optional callback functions erroneously defined", [
        Behaviour
    ]);
format_error({behaviour_info, {_M, F, A}}) ->
    io_lib:format(
        "cannot define callback attibute for ~tw/~w when "
        "behaviour_info is defined",
        [F, A]
    );
format_error({redefine_optional_callback, {F, A}}) ->
    io_lib:format("optional callback ~tw/~w duplicated", [F, A]);
format_error({undefined_callback, {_M, F, A}}) ->
    io_lib:format("callback ~tw/~w is undefined", [F, A]);
%% --- types and specs ---
format_error({singleton_typevar, Name}) ->
    io_lib:format("type variable ~tw is only used once (is unbound)", [Name]);
format_error({reused_typevar, Name}) ->
    io_lib:format("type variable ~tw appears multiple times in type arguments", [Name]);
format_error(underscore_type) ->
    "_ is not a valid type";
format_error({bad_export_type, _ETs}) ->
    io_lib:format("bad export_type declaration", []);
format_error({duplicated_export_type, {T, A}}) ->
    io_lib:format("type ~tw/~w already exported", [T, A]);
format_error({undefined_type, {TypeName, Arity}}) ->
    io_lib:format("type ~tw~s undefined", [TypeName, gen_type_paren(Arity)]);
format_error({undefined_remote_type, M, N, A}) ->
    io_lib:format("type ~tw:~tw~s undefined", [M, N, gen_type_paren(A)]);
format_error({private_remote_type, M, N, A}) ->
    io_lib:format("type ~tw:~tw~s is private", [M, N, gen_type_paren(A)]);
format_error({remote_type_wrong_arity, M, N, Given, Exp}) ->
    io_lib:format("type ~tw:~tw is defined with ~w parameters, ~w given", [M, N, Exp, Given]);
format_error({unused_type, {TypeName, Arity}}) ->
    io_lib:format("type ~tw~s is unused", [TypeName, gen_type_paren(Arity)]);
format_error({new_builtin_type, {TypeName, Arity}}) ->
    io_lib:format(
        "type ~w~s is a new builtin type; "
        "its (re)definition is allowed only until the next release",
        [TypeName, gen_type_paren(Arity)]
    );
format_error({builtin_type, {TypeName, Arity}}) ->
    io_lib:format("type ~w~s is a builtin type; it cannot be redefined", [
        TypeName,
        gen_type_paren(Arity)
    ]);
format_error({renamed_type, OldName, NewName}) ->
    io_lib:format(
        "type ~w() is now called ~w(); "
        "please use the new name instead",
        [OldName, NewName]
    );
format_error({redefine_type, {TypeName, _Arity}}) ->
    io_lib:format("type ~tw already defined", [TypeName]);
format_error({type_syntax, Constr}) ->
    io_lib:format("bad ~tw type", [Constr]);
format_error(old_abstract_code) ->
    io_lib:format(
        "abstract code generated before Erlang/OTP 19.0 and "
        "having typed record fields cannot be compiled",
        []
    );
format_error({redefine_spec, {M, F, A}}) ->
    io_lib:format("spec for ~tw:~tw/~w already defined", [M, F, A]);
format_error({redefine_spec, {F, A}}) ->
    io_lib:format("spec for ~tw/~w already defined", [F, A]);
format_error({redefine_callback, {F, A}}) ->
    io_lib:format("callback ~tw/~w already defined", [F, A]);
format_error({bad_callback, {M, F, A}}) ->
    io_lib:format("explicit module not allowed for callback ~tw:~tw/~w", [M, F, A]);
format_error({bad_module, {M, F, A}}) ->
    io_lib:format("spec for function ~w:~tw/~w from other module", [M, F, A]);
format_error({spec_fun_undefined, {F, A}}) ->
    io_lib:format("spec for undefined function ~tw/~w", [F, A]);
format_error({missing_spec, {F, A}}) ->
    io_lib:format("missing specification for function ~tw/~w", [F, A]);
format_error(spec_wrong_arity) ->
    "spec has wrong arity";
format_error(callback_wrong_arity) ->
    "callback has wrong arity";
format_error({deprecated_builtin_type, {Name, Arity}, Replacement, Rel}) ->
    UseS =
        case Replacement of
            {Mod, NewName} ->
                io_lib:format("use ~w:~w/~w", [Mod, NewName, Arity]);
            {Mod, NewName, NewArity} ->
                io_lib:format("use ~w:~w/~w or preferably ~w:~w/~w", [
                    Mod,
                    NewName,
                    Arity,
                    Mod,
                    NewName,
                    NewArity
                ])
        end,
    io_lib:format(
        "type ~w/~w is deprecated and will be "
        "removed in ~s; use ~s",
        [Name, Arity, Rel, UseS]
    );
format_error(opaque_unchecked_type_error) ->
    "opaque unchecked type must be term()";
format_error({not_exported_opaque, {TypeName, Arity}}) ->
    io_lib:format("opaque type ~tw~s is not exported", [TypeName, gen_type_paren(Arity)]);
format_error({underspecified_opaque, {TypeName, Arity}}) ->
    io_lib:format("opaque type ~tw~s is underspecified and therefore meaningless", [
        TypeName,
        gen_type_paren(Arity)
    ]);
format_error({bad_dialyzer_attribute, Term}) ->
    io_lib:format("badly formed dialyzer attribute: ~tw", [Term]);
format_error({bad_dialyzer_option, Term}) ->
    io_lib:format("unknown dialyzer warning option: ~tw", [Term]);
%% --- obsolete? unused? ---
format_error({format_error, {Fmt, Args}}) ->
    io_lib:format(Fmt, Args).

format_reference({enum, N, C}) -> io_lib:format("enum ~ts.~ts", [format_name(N), C]);
format_reference({struct, N}) -> io_lib:format("struct ~ts", [format_name(N)]).

format_name({M, N}) -> io_lib:format("~tw:~tw", [M, N]);
format_name(N) -> io_lib:format("~tw", [N]).

gen_type_paren(Arity) when is_integer(Arity), Arity >= 0 ->
    gen_type_paren_1(Arity, ")").

gen_type_paren_1(0, Acc) -> "(" ++ Acc;
gen_type_paren_1(1, Acc) -> "(_" ++ Acc;
gen_type_paren_1(N, Acc) -> gen_type_paren_1(N - 1, ",_" ++ Acc).

format_mfa({M, F, [_ | _] = As}) ->
    "," ++ ArityString = lists:append([[$, | integer_to_list(A)] || A <- As]),
    format_mf(M, F, ArityString);
format_mfa({M, F, A}) when is_integer(A) ->
    format_mf(M, F, integer_to_list(A)).

format_mf(M, F, ArityString) when is_atom(M), is_atom(F) ->
    atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ ArityString.

format_mna({M, N, A}) when is_integer(A) ->
    atom_to_list(M) ++ ":" ++ atom_to_list(N) ++ gen_type_paren(A).

format_where(L) when is_integer(L) ->
    io_lib:format("(line ~p)", [L]);
format_where({L, C}) when is_integer(L), is_integer(C) ->
    io_lib:format("(line ~p, column ~p)", [L, C]);
format_where(Anno) when is_list(Anno) ->
    format_where(erl_anno:location(Anno)).

%% Local functions that are somehow automatically generated.

pseudolocals() ->
    [{module_info, 0}, {module_info, 1}, {record_info, 2}].

%% module([Form], FileName, DefsDb, [CompileOption]) ->
%%      {ok,[Warning]} | {error,[Error],[Warning]}
%%  Start processing a module. Define predefined functions and exports and
%%  apply_lambda/2 has been called to shut lint up. N.B. these lists are
%%  really all ordsets!

-spec module(AbsForms, FileName, DefsDB, CompileOptions) ->
    {ok, Warnings} | {error, Errors, Warnings}
when
    AbsForms :: [erlt_parse:abstract_form() | erlt_parse:form_info()],
    FileName :: atom() | string(),
    DefsDB :: erlt_defs:defs(),
    CompileOptions :: [compile:option()],
    Warnings :: [{file:filename(), [ErrorInfo]}],
    Errors :: [{FileName2 :: file:filename(), [ErrorInfo]}],
    ErrorInfo :: error_info().
module(Forms, FileName, DefsDB, Opts0) ->
    %% We want the options given on the command line to take
    %% precedence over options in the module.
    Opts = compiler_options(Forms) ++ Opts0,
    St = forms(Forms, start(FileName, DefsDB, Opts)),
    return_status(St).

compiler_options(Forms) ->
    lists:flatten([C || {attribute, _, compile, C} <- Forms]).

%% start(FileName, DepsDB, [Option]) -> State

start(File, DefsDB, Opts) ->
    Enabled0 = [
        {unused_vars, bool_option(warn_unused_vars, nowarn_unused_vars, true, Opts)},
        {export_all, bool_option(warn_export_all, nowarn_export_all, true, Opts)},
        % not optional here
        {export_vars, true},
        {shadow_vars, bool_option(warn_shadow_vars, nowarn_shadow_vars, true, Opts)},
        {unused_import, bool_option(warn_unused_import, nowarn_unused_import, false, Opts)},
        {unused_function, bool_option(warn_unused_function, nowarn_unused_function, true, Opts)},
        {unused_type, bool_option(warn_unused_type, nowarn_unused_type, true, Opts)},
        {bif_clash, bool_option(warn_bif_clash, nowarn_bif_clash, true, Opts)},
        {deprecated_function,
            bool_option(
                warn_deprecated_function,
                nowarn_deprecated_function,
                true,
                Opts
            )},
        {deprecated_type, bool_option(warn_deprecated_type, nowarn_deprecated_type, true, Opts)},
        {obsolete_guard, bool_option(warn_obsolete_guard, nowarn_obsolete_guard, true, Opts)},
        {missing_spec, bool_option(warn_missing_spec, nowarn_missing_spec, false, Opts)},
        {missing_spec_all,
            bool_option(warn_missing_spec_all, nowarn_missing_spec_all, false, Opts)},
        {removed, bool_option(warn_removed, nowarn_removed, true, Opts)},
        {nif_inline,
            bool_option(
                warn_nif_inline,
                nowarn_nif_inline,
                true,
                Opts
            )}
    ],
    Enabled1 = [Category || {Category, true} <- Enabled0],
    Enabled = ordsets:from_list(Enabled1),
    Calls =
        case ordsets:is_element(unused_function, Enabled) of
            true ->
                maps:from_list([{{module_info, 1}, pseudolocals()}]);
            false ->
                undefined
        end,
    #lint{
        state = start,
        exports = gb_sets:from_list([{module_info, 0}, {module_info, 1}]),
        compile = Opts,
        %% Internal pseudo-functions must appear as defined/reached.
        defined = gb_sets:from_list(pseudolocals()),
        called = [{F, 0} || F <- pseudolocals()],
        usage = #usage{calls = Calls},
        warn_format = value_option(warn_format, 1, warn_format, 1, nowarn_format, 0, Opts),
        enabled_warnings = Enabled,
        nowarn_bif_clash = nowarn_function(nowarn_bif_clash, Opts),
        defs_db = DefsDB,
        file = File
    }.

%% is_warn_enabled(Category, St) -> boolean().
%%  Check whether a warning of category Category is enabled.
is_warn_enabled(Type, #lint{enabled_warnings = Enabled}) ->
    ordsets:is_element(Type, Enabled).

%% return_status(State) ->
%%      {ok,[Warning]} | {error,[Error],[Warning]}
%%  Pack errors and warnings properly and return ok | error.

return_status(St) ->
    Ws = pack_warnings(St#lint.warnings),
    case pack_errors(St#lint.errors) of
        [] -> {ok, Ws};
        Es -> {error, Es, Ws}
    end.

%% pack_errors([{File,ErrD}]) -> [{File,[ErrD]}].
%%  Sort on (reversed) insertion order.

pack_errors(Es) ->
    {Es1, _} = mapfoldl(fun({File, E}, I) -> {{File, {I, E}}, I - 1} end, -1, Es),
    map(
        fun({File, EIs}) -> {File, map(fun({_I, E}) -> E end, EIs)} end,
        pack_warnings(Es1)
    ).

%% pack_warnings([{File,ErrD}]) -> [{File,[ErrD]}]
%%  Sort on line number.

pack_warnings(Ws) ->
    [
        {File, lists:sort([W || {F, W} <- Ws, F =:= File])}
     || File <- lists:usort([F || {F, _} <- Ws])
    ].

%% add_error(ErrorDescriptor, State) -> State'
%% add_error(Line, Error, State) -> State'
%% add_warning(ErrorDescriptor, State) -> State'
%% add_warning(Line, Error, State) -> State'

add_error(E, St) -> add_lint_error(E, St#lint.file, St).

add_error(Anno, E0, #lint{gexpr_context = Context} = St) ->
    E =
        case {E0, Context} of
            {illegal_guard_expr, bin_seg_size} ->
                illegal_bitsize;
            {{illegal_guard_local_call, FA}, bin_seg_size} ->
                {illegal_bitsize_local_call, FA};
            {illegal_guard_expr, field_default} ->
                illegal_field_default;
            {{illegal_goard_local_call, FA}, field_default} ->
                {illegal_field_default_local_call, FA};
            {_, _} ->
                E0
        end,
    {File, Location} = loc(Anno, St),
    add_lint_error({Location, erlt_lint, E}, File, St).

add_lint_error(E, File, St) ->
    St#lint{errors = [{File, E} | St#lint.errors]}.

add_warning(W, St) -> add_lint_warning(W, St#lint.file, St).

add_warning(FileLine, W, St) ->
    {File, Location} = loc(FileLine, St),
    add_lint_warning({Location, erlt_lint, W}, File, St).

add_lint_warning(W, File, St) ->
    St#lint{warnings = [{File, W} | St#lint.warnings]}.

loc(Anno, St) ->
    Location0 = erl_anno:location(Anno),
    Location =
        case erlt_parse:get_end_location(Anno) of
            undefined -> Location0;
            EndLoc -> [{location, Location0}, {end_location, EndLoc}]
        end,
    case erl_anno:file(Anno) of
        undefined -> {St#lint.file, Location};
        File -> {File, Location}
    end.

%% forms([Form], State) -> State'

forms(Forms0, St0) ->
    Forms = eval_file_attribute(Forms0, St0),
    %% Annotations from now on include the 'file' item.
    Locals = local_functions(Forms),
    AutoImportSuppressed = auto_import_suppressed(St0#lint.compile),
    StDeprecated = disallowed_compile_flags(Forms, St0),
    St1 =
        includes_qlc_hrl(Forms, StDeprecated#lint{
            locals = Locals,
            no_auto = AutoImportSuppressed
        }),
    St2 = bif_clashes(Forms, St1),
    St3 = not_deprecated(Forms, St2),
    St4 = not_removed(Forms, St3),
    St5 = foldl(fun form/2, pre_scan(Forms, St4), Forms),
    post_traversal_check(Forms, St5).

pre_scan([{attribute, L, compile, C} | Fs], St) ->
    case
        is_warn_enabled(export_all, St) andalso
            member(export_all, lists:flatten([C]))
    of
        true ->
            pre_scan(Fs, add_warning(L, export_all, St));
        false ->
            pre_scan(Fs, St)
    end;
%% structs and enums can appear in any order, scan for definitions before
%% actual checking begins
pre_scan([{attribute, Loc, Struct, {_, TypeDef, Args}} | Fs], St) when ?IS_STRUCT(Struct) ->
    pre_scan(Fs, struct_def(Loc, TypeDef, length(Args), St));
pre_scan([{attribute, Loc, enum, {_, TypeDef, Args}} | Fs], St) ->
    pre_scan(Fs, enum_def(Loc, TypeDef, length(Args), St));
pre_scan([_ | Fs], St) ->
    pre_scan(Fs, St);
pre_scan([], St) ->
    St.

includes_qlc_hrl(Forms, St) ->
    %% QLC calls erl_lint several times, sometimes with the compile
    %% attribute removed. The file attribute, however, is left as is.
    QH = [
        File
     || {attribute, _, file, {File, _line}} <- Forms, filename:basename(File) =:= "qlc.hrl"
    ],
    St#lint{xqlc = QH =/= []}.

eval_file_attribute(Forms, St) ->
    eval_file_attr(Forms, St#lint.file).

eval_file_attr([{attribute, _L, file, {File, _Line}} = Form | Forms], _File) ->
    [Form | eval_file_attr(Forms, File)];
eval_file_attr([Form0 | Forms], File) ->
    Form = set_form_file(Form0, File),
    [Form | eval_file_attr(Forms, File)];
eval_file_attr([], _File) ->
    [].

%% Sets the file only on the form. This is used on post-traversal.
%% For the remaining of the AST we rely on #lint.file.

set_form_file({attribute, L, K, V}, File) ->
    {attribute, erl_anno:set_file(File, L), K, V};
set_form_file({F, L, N, A, C}, File) when ?IS_FUNCTION(F) ->
    {F, erl_anno:set_file(File, L), N, A, C};
set_form_file(Form, _File) ->
    Form.

%% form(Form, State) -> State'
%%  Check a form returning the updated State. Handle generic cases here.

form({error, E}, St) ->
    add_error(E, St);
form({warning, W}, St) ->
    add_warning(W, St);
form({attribute, _L, file, {File, _Line}}, St) ->
    St#lint{file = File};
form({attribute, _L, compile, _}, St) ->
    St;
form(Form, #lint{state = State} = St) ->
    case State of
        start -> start_state(Form, St);
        attribute -> attribute_state(Form, St);
        function -> function_state(Form, St)
    end.

%% start_state(Form, State) -> State'

start_state({attribute, Line, module, {_, _}} = Form, St0) ->
    St1 = add_error(Line, pmod_unsupported, St0),
    attribute_state(Form, St1#lint{state = attribute});
start_state({attribute, Line, module, M}, St0) ->
    St1 = St0#lint{module = M},
    St2 = St1#lint{state = attribute},
    check_module_name(M, Line, St2);
start_state(Form, St) ->
    Anno =
        case Form of
            {eof, L} ->
                erl_anno:new(L);
            %% {warning, Warning} and {error, Error} not possible here.
            _ ->
                element(2, Form)
        end,
    St1 = add_error(Anno, undefined_module, St),
    attribute_state(Form, St1#lint{state = attribute}).

%% attribute_state(Form, State) ->
%%      State'

attribute_state({attribute, _L, module, _M}, #lint{module = ''} = St) ->
    St;
attribute_state({attribute, L, module, _M}, St) ->
    add_error(L, redefine_module, St);
attribute_state({attribute, L, export, Es}, St) ->
    export(L, Es, St);
attribute_state({attribute, L, export_type, Es}, St) ->
    export_type(L, Es, St);
attribute_state({attribute, L, import, Is}, St) ->
    import(L, Is, St);
attribute_state({attribute, L, import_type, Is}, St) ->
    import_type(L, Is, St);
attribute_state({attribute, La, behaviour, Behaviour}, St) ->
    St#lint{behaviour = St#lint.behaviour ++ [{La, Behaviour}]};
attribute_state({attribute, La, behavior, Behaviour}, St) ->
    St#lint{behaviour = St#lint.behaviour ++ [{La, Behaviour}]};
attribute_state({attribute, L, type, {TypeName, TypeDef, Args}}, St) ->
    type_def(type, L, TypeName, TypeDef, Args, St);
attribute_state({attribute, L, opaque, {TypeName, TypeDef, Args}}, St) ->
    type_def(opaque, L, TypeName, TypeDef, Args, St);
attribute_state({attribute, L, unchecked_opaque, {TypeName, TypeDef, Args}}, St) ->
    type_def(unchecked_opaque, L, TypeName, TypeDef, Args, St);
attribute_state({attribute, L, enum, {TypeName, TypeDef, Args}}, St) ->
    type_def(enum, L, TypeName, TypeDef, Args, St);
attribute_state({attribute, L, Struct, {TypeName, TypeDef, Args}}, St) when ?IS_STRUCT(Struct) ->
    type_def(Struct, L, TypeName, TypeDef, Args, St);
attribute_state({attribute, L, spec, {Fun, Types}}, St) ->
    spec_decl(L, Fun, Types, St);
attribute_state({attribute, L, callback, {Fun, Types}}, St) ->
    callback_decl(L, Fun, Types, St);
attribute_state({attribute, L, optional_callbacks, Es}, St) ->
    optional_callbacks(L, Es, St);
attribute_state({attribute, L, on_load, Val}, St) ->
    on_load(L, Val, St);
% Ignore others
attribute_state({attribute, _L, _Other, _Val}, St) ->
    St;
attribute_state(Form, St) ->
    function_state(Form, St#lint{state = function}).

%% function_state(Form, State) ->
%%      State'
%%  Allow for type and opaque type definitions and spec
%%  declarations to be intersperced within function definitions.
%%  Dialyzer attributes are also allowed everywhere.

function_state({attribute, L, type, {TypeName, TypeDef, Args}}, St) ->
    type_def(type, L, TypeName, TypeDef, Args, St);
function_state({attribute, L, opaque, {TypeName, TypeDef, Args}}, St) ->
    type_def(opaque, L, TypeName, TypeDef, Args, St);
function_state({attribute, L, unchecked_opaque, {TypeName, TypeDef, Args}}, St) ->
    type_def(unchecked_opaque, L, TypeName, TypeDef, Args, St);
function_state({attribute, L, enum, {TypeName, TypeDef, Args}}, St) ->
    type_def(enum, L, TypeName, TypeDef, Args, St);
function_state({attribute, L, Struct, {TypeName, TypeDef, Args}}, St) when ?IS_STRUCT(Struct) ->
    type_def(Struct, L, TypeName, TypeDef, Args, St);
function_state({attribute, L, spec, {Fun, Types}}, St) ->
    spec_decl(L, Fun, Types, St);
function_state({attribute, _L, dialyzer, _Val}, St) ->
    St;
function_state({attribute, La, Attr, _Val}, St) ->
    add_error(La, {attribute, Attr}, St);
function_state({unchecked_function, L, N, A, Cs}, St) ->
    St1 = function(L, N, A, Cs, St#lint{type_checked = false}),
    St1#lint{type_checked = true};
function_state({function, L, N, A, Cs}, St) ->
    function(L, N, A, Cs, St#lint{type_checked = true});
function_state({eof, L}, St) ->
    eof(L, St).

%% eof(LastLine, State) ->
%%      State'

eof(_Line, St0) ->
    St0.

%% bif_clashes(Forms, State0) -> State.

bif_clashes(Forms, #lint{nowarn_bif_clash = Nowarn} = St) ->
    Clashes0 = [
        {Name, Arity}
     || {F, _L, Name, Arity, _Cs} <- Forms, ?IS_FUNCTION(F), erl_internal:bif(Name, Arity)
    ],
    Clashes = ordsets:subtract(ordsets:from_list(Clashes0), Nowarn),
    St#lint{clashes = Clashes}.

%% not_deprecated(Forms, State0) -> State

not_deprecated(Forms, #lint{compile = Opts} = St0) ->
    %% There are no line numbers in St0#lint.compile.
    MFAsL = [
        {MFA, L}
     || {attribute, L, compile, Args} <- Forms,
        {nowarn_deprecated_function, MFAs0} <- lists:flatten([Args]),
        MFA <- lists:flatten([MFAs0])
    ],
    Nowarn = [MFA || {nowarn_deprecated_function, MFAs0} <- Opts, MFA <- lists:flatten([MFAs0])],
    ML = [{M, L} || {{M, _F, _A}, L} <- MFAsL, is_atom(M)],
    St1 = foldl(
        fun({M, L}, St2) ->
            check_module_name(M, L, St2)
        end,
        St0,
        ML
    ),
    St1#lint{not_deprecated = ordsets:from_list(Nowarn)}.

%% not_removed(Forms, State0) -> State

not_removed(Forms, #lint{compile = Opts} = St0) ->
    %% There are no line numbers in St0#lint.compile.
    MFAsL = [
        {MFA, L}
     || {attribute, L, compile, Args} <- Forms,
        {nowarn_removed, MFAs0} <- lists:flatten([Args]),
        MFA <- lists:flatten([MFAs0])
    ],
    Nowarn = [MFA || {nowarn_removed, MFAs0} <- Opts, MFA <- lists:flatten([MFAs0])],
    St1 = foldl(
        fun
            ({{M, _F, _A}, L}, St2) ->
                check_module_name(M, L, St2);
            ({M, L}, St2) ->
                check_module_name(M, L, St2)
        end,
        St0,
        MFAsL
    ),
    St1#lint{not_removed = gb_sets:from_list(Nowarn)}.

%% The nowarn_bif_clash directive is not only deprecated, it's actually an error from R14A
disallowed_compile_flags(Forms, St0) ->
    %% There are (still) no line numbers in St0#lint.compile.
    Errors0 = [
        {St0#lint.file, {L, erlt_lint, disallowed_nowarn_bif_clash}}
     || {attribute, A, compile, nowarn_bif_clash} <- Forms, {_, L} <- [loc(A, St0)]
    ],
    Errors1 = [
        {St0#lint.file, {L, erlt_lint, disallowed_nowarn_bif_clash}}
     || {attribute, A, compile, {nowarn_bif_clash, {_, _}}} <- Forms, {_, L} <- [loc(A, St0)]
    ],
    Disabled = (not is_warn_enabled(bif_clash, St0)),
    Errors =
        if
            Disabled andalso Errors0 =:= [] ->
                [
                    {St0#lint.file, {erlt_lint, disallowed_nowarn_bif_clash}}
                    | St0#lint.errors
                ];
            Disabled ->
                Errors0 ++ Errors1 ++ St0#lint.errors;
            true ->
                Errors1 ++ St0#lint.errors
        end,
    St0#lint{errors = Errors}.

%% post_traversal_check(Forms, State0) -> State.
%% Do some further checking after the forms have been traversed and
%% data about calls etc. have been collected.

post_traversal_check(Forms, St0) ->
    St1 = check_behaviour(St0),
    St2 = check_deprecated(Forms, St1),
    St3 = check_imports(Forms, St2),
    St4 = check_inlines(Forms, St3),
    St5 = check_undefined_functions(St4),
    St6 = check_unused_functions(Forms, St5),
    St7 = check_bif_clashes(Forms, St6),
    St8 = check_specs_without_function(St7),
    St9 = check_functions_without_spec(Forms, St8),
    StA = check_undefined_types(St9),
    StB = check_unused_types(Forms, StA),
    StD = check_on_load(StB),
    StF = check_local_opaque_types(StD),
    StG = check_dialyzer_attribute(Forms, StF),
    StH = check_callback_information(StG),
    check_removed(Forms, StH).

%% check_behaviour(State0) -> State
%% Check that the behaviour attribute is valid.

check_behaviour(St0) ->
    behaviour_check(St0#lint.behaviour, St0).

%% behaviour_check([{Line,Behaviour}], State) -> State'
%%  Check behaviours for existence and defined functions.

behaviour_check(Bs, St0) ->
    {AllBfs0, St1} = all_behaviour_callbacks(Bs, [], St0),
    St = behaviour_missing_callbacks(AllBfs0, St1),
    Exports = exports(St0),
    F = fun(Bfs, OBfs) ->
        [
            B
         || B <- Bfs,
            not lists:member(B, OBfs) orelse
                gb_sets:is_member(B, Exports)
        ]
    end,
    %% After fixing missing callbacks new warnings may be emitted.
    AllBfs = [{Item, F(Bfs0, OBfs0)} || {Item, Bfs0, OBfs0} <- AllBfs0],
    behaviour_conflicting(AllBfs, St).

all_behaviour_callbacks([{Line, B} | Bs], Acc, St0) ->
    {Bfs0, OBfs0, St} = behaviour_callbacks(Line, B, St0),
    all_behaviour_callbacks(Bs, [{{Line, B}, Bfs0, OBfs0} | Acc], St);
all_behaviour_callbacks([], Acc, St) ->
    {reverse(Acc), St}.

behaviour_callbacks(Line, B, St0) ->
    try B:behaviour_info(callbacks) of
        undefined ->
            St1 = add_warning(Line, {undefined_behaviour_callbacks, B}, St0),
            {[], [], St1};
        Funcs ->
            case is_fa_list(Funcs) of
                true ->
                    try B:behaviour_info(optional_callbacks) of
                        undefined ->
                            {Funcs, [], St0};
                        OptFuncs ->
                            %% OptFuncs should always be OK thanks to
                            %% sys_pre_expand.
                            case is_fa_list(OptFuncs) of
                                true ->
                                    {Funcs, OptFuncs, St0};
                                false ->
                                    W = {ill_defined_optional_callbacks, B},
                                    St1 = add_warning(Line, W, St0),
                                    {Funcs, [], St1}
                            end
                    catch
                        _:_ ->
                            {Funcs, [], St0}
                    end;
                false ->
                    St1 = add_warning(Line, {ill_defined_behaviour_callbacks, B}, St0),
                    {[], [], St1}
            end
    catch
        _:_ ->
            St1 = add_warning(Line, {undefined_behaviour, B}, St0),
            St2 = check_module_name(B, Line, St1),
            {[], [], St2}
    end.

behaviour_missing_callbacks([{{Line, B}, Bfs0, OBfs} | T], St0) ->
    Bfs = ordsets:subtract(ordsets:from_list(Bfs0), ordsets:from_list(OBfs)),
    Exports = gb_sets:to_list(exports(St0)),
    Missing = ordsets:subtract(Bfs, Exports),
    St = foldl(
        fun(F, S0) ->
            case is_fa(F) of
                true ->
                    M = {undefined_behaviour_func, F, B},
                    add_warning(Line, M, S0);
                false ->
                    % ill_defined_behaviour_callbacks
                    S0
            end
        end,
        St0,
        Missing
    ),
    behaviour_missing_callbacks(T, St);
behaviour_missing_callbacks([], St) ->
    St.

behaviour_conflicting(AllBfs, St) ->
    R0 = sofs:relation(AllBfs, [{item, [callback]}]),
    R1 = sofs:family_to_relation(R0),
    R2 = sofs:converse(R1),
    R3 = sofs:relation_to_family(R2),
    R4 = sofs:family_specification(fun(S) -> sofs:no_elements(S) > 1 end, R3),
    R = sofs:to_external(R4),
    behaviour_add_conflicts(R, St).

behaviour_add_conflicts([{Cb, [{FirstLoc, FirstB} | Cs]} | T], St0) ->
    FirstL = element(2, loc(FirstLoc, St0)),
    St = behaviour_add_conflict(Cs, Cb, FirstL, FirstB, St0),
    behaviour_add_conflicts(T, St);
behaviour_add_conflicts([], St) ->
    St.

behaviour_add_conflict([{Line, B} | Cs], Cb, FirstL, FirstB, St0) ->
    St = add_warning(Line, {conflicting_behaviours, Cb, B, FirstL, FirstB}, St0),
    behaviour_add_conflict(Cs, Cb, FirstL, FirstB, St);
behaviour_add_conflict([], _, _, _, St) ->
    St.

%% check_deprecated(Forms, State0) -> State

check_deprecated(Forms, St0) ->
    %% Get the correct list of exported functions.
    Exports =
        case member(export_all, St0#lint.compile) of
            true -> St0#lint.defined;
            false -> St0#lint.exports
        end,
    X = ignore_predefined_funcs(gb_sets:to_list(Exports)),
    #lint{module = Mod} = St0,
    Bad = [
        {E, L}
     || {attribute, L, deprecated, Depr} <- Forms,
        D <- lists:flatten([Depr]),
        E <- depr_cat(D, X, Mod)
    ],
    foldl(
        fun({E, L}, St1) ->
            add_error(L, E, St1)
        end,
        St0,
        Bad
    ).

depr_cat({F, A, Flg} = D, X, Mod) ->
    case deprecated_flag(Flg) of
        false -> [{invalid_deprecated, D}];
        true -> depr_fa(F, A, X, Mod)
    end;
depr_cat({F, A}, X, Mod) ->
    depr_fa(F, A, X, Mod);
depr_cat(module, _X, _Mod) ->
    [];
depr_cat(D, _X, _Mod) ->
    [{invalid_deprecated, D}].

depr_fa('_', '_', _X, _Mod) ->
    [];
depr_fa(F, '_', X, _Mod) when is_atom(F) ->
    %% Don't use this syntax for built-in functions.
    case lists:filter(fun({F1, _}) -> F1 =:= F end, X) of
        [] -> [{bad_deprecated, {F, '_'}}];
        _ -> []
    end;
depr_fa(F, A, X, Mod) when is_atom(F), is_integer(A), A >= 0 ->
    case lists:member({F, A}, X) of
        true ->
            [];
        false ->
            case erlang:is_builtin(Mod, F, A) of
                true -> [];
                false -> [{bad_deprecated, {F, A}}]
            end
    end;
depr_fa(F, A, _X, _Mod) ->
    [{invalid_deprecated, {F, A}}].

deprecated_flag(next_version) -> true;
deprecated_flag(next_major_release) -> true;
deprecated_flag(eventually) -> true;
deprecated_flag(String) -> deprecated_desc(String).

deprecated_desc([Char | Str]) when is_integer(Char) -> deprecated_desc(Str);
deprecated_desc([]) -> true;
deprecated_desc(_) -> false.

%% check_removed(Forms, State0) -> State

check_removed(Forms, St0) ->
    %% Get the correct list of exported functions.
    Exports =
        case member(export_all, St0#lint.compile) of
            true -> St0#lint.defined;
            false -> St0#lint.exports
        end,
    X = ignore_predefined_funcs(gb_sets:to_list(Exports)),
    #lint{module = Mod} = St0,
    Bad = [
        {E, L}
     || {attribute, L, removed, Removed} <- Forms,
        R <- lists:flatten([Removed]),
        E <- removed_cat(R, X, Mod)
    ],
    foldl(
        fun({E, L}, St1) ->
            add_error(L, E, St1)
        end,
        St0,
        Bad
    ).

removed_cat({F, A, Desc} = R, X, Mod) ->
    case removed_desc(Desc) of
        false -> [{invalid_removed, R}];
        true -> removed_fa(F, A, X, Mod)
    end;
removed_cat({F, A}, X, Mod) ->
    removed_fa(F, A, X, Mod);
removed_cat(module, X, Mod) ->
    removed_fa('_', '_', X, Mod);
removed_cat(R, _X, _Mod) ->
    [{invalid_removed, R}].

removed_fa('_', '_', X, _Mod) ->
    case X of
        [_ | _] -> [{bad_removed, {'_', '_'}}];
        [] -> []
    end;
removed_fa(F, '_', X, _Mod) when is_atom(F) ->
    %% Don't use this syntax for built-in functions.
    case lists:filter(fun({F1, _}) -> F1 =:= F end, X) of
        [_ | _] -> [{bad_removed, {F, '_'}}];
        _ -> []
    end;
removed_fa(F, A, X, Mod) when is_atom(F), is_integer(A), A >= 0 ->
    case lists:member({F, A}, X) of
        true ->
            [{bad_removed, {F, A}}];
        false ->
            case erlang:is_builtin(Mod, F, A) of
                true -> [{bad_removed, {F, A}}];
                false -> []
            end
    end;
removed_fa(F, A, _X, _Mod) ->
    [{invalid_removed, {F, A}}].

removed_desc([Char | Str]) when is_integer(Char) -> removed_desc(Str);
removed_desc([]) -> true;
removed_desc(_) -> false.

%% Ignores functions added by erl_internal:add_predefined_functions/1
ignore_predefined_funcs([{behaviour_info, 1} | Fs]) ->
    ignore_predefined_funcs(Fs);
ignore_predefined_funcs([{module_info, 0} | Fs]) ->
    ignore_predefined_funcs(Fs);
ignore_predefined_funcs([{module_info, 1} | Fs]) ->
    ignore_predefined_funcs(Fs);
ignore_predefined_funcs([Other | Fs]) ->
    [Other | ignore_predefined_funcs(Fs)];
ignore_predefined_funcs([]) ->
    [].

%% check_imports(Forms, State0) -> State

check_imports(Forms, St0) ->
    case is_warn_enabled(unused_import, St0) of
        false ->
            St0;
        true ->
            Usage = St0#lint.usage,
            Unused = ordsets:subtract(St0#lint.imports, Usage#usage.imported),
            Imports = [
                {{FA, Mod}, L}
             || {attribute, L, import, {Mod, Fs}} <- Forms, FA <- lists:usort(Fs)
            ],
            Bad = [{FM, L} || FM <- Unused, {FM2, L} <- Imports, FM =:= FM2],
            St1 = func_line_warning(unused_import, Bad, St0),
            TUnused = ordsets:subtract(St1#lint.imp_types, Usage#usage.imported_types),
            TImports = [
                {{FA, Mod}, L}
             || {attribute, L, import_type, {Mod, Fs}} <- Forms, FA <- lists:usort(Fs)
            ],
            TBad = [{FM, L} || FM <- TUnused, {FM2, L} <- TImports, FM =:= FM2],
            func_line_warning(unused_import_type, TBad, St1)
    end.

%% check_inlines(Forms, State0) -> State

check_inlines(Forms, St0) ->
    check_option_functions(Forms, inline, bad_inline, St0).

%% check_unused_functions(Forms, State0) -> State

check_unused_functions(Forms, St0) ->
    St1 = check_option_functions(
        Forms,
        nowarn_unused_function,
        bad_nowarn_unused_function,
        St0
    ),
    Opts = St1#lint.compile,
    case
        member(export_all, Opts) orelse
            not is_warn_enabled(unused_function, St1)
    of
        true ->
            St1;
        false ->
            Nowarn = nowarn_function(nowarn_unused_function, Opts),
            Usage = St1#lint.usage,
            Used = reached_functions(initially_reached(St1), Usage#usage.calls),
            UsedOrNowarn = ordsets:union(Used, Nowarn),
            Unused = ordsets:subtract(gb_sets:to_list(St1#lint.defined), UsedOrNowarn),
            Functions = [{{N, A}, L} || {F, L, N, A, _} <- Forms, ?IS_FUNCTION(F)],
            Bad = [{FA, L} || FA <- Unused, {FA2, L} <- Functions, FA =:= FA2],
            func_line_warning(unused_function, Bad, St1)
    end.

initially_reached(#lint{exports = Exp, on_load = OnLoad}) ->
    OnLoad ++ gb_sets:to_list(Exp).

%% reached_functions(RootSet, CallRef) -> [ReachedFunc].
%% reached_functions(RootSet, CallRef, [ReachedFunc]) -> [ReachedFunc].

reached_functions(Root, Ref) ->
    reached_functions(Root, [], Ref, gb_sets:empty()).

reached_functions([R | Rs], More0, Ref, Reached0) ->
    case gb_sets:is_element(R, Reached0) of
        true ->
            reached_functions(Rs, More0, Ref, Reached0);
        false ->
            %It IS reached
            Reached = gb_sets:add_element(R, Reached0),
            case maps:find(R, Ref) of
                {ok, More} -> reached_functions(Rs, [More | More0], Ref, Reached);
                error -> reached_functions(Rs, More0, Ref, Reached)
            end
    end;
reached_functions([], [_ | _] = More, Ref, Reached) ->
    reached_functions(lists:append(More), [], Ref, Reached);
reached_functions([], [], _Ref, Reached) ->
    gb_sets:to_list(Reached).

%% check_undefined_functions(State0) -> State

check_undefined_functions(#lint{called = Called0, defined = Def0} = St0) ->
    Called = sofs:relation(Called0, [{func, location}]),
    Def = sofs:from_external(gb_sets:to_list(Def0), [func]),
    Undef = sofs:to_external(sofs:drestriction(Called, Def)),
    foldl(
        fun({NA, L}, St) ->
            add_error(L, {undefined_function, NA}, St)
        end,
        St0,
        Undef
    ).

%% check_undefined_types(State0) -> State

check_undefined_types(#lint{usage = Usage, types = Def} = St0) ->
    Used = Usage#usage.used_types,
    UTAs = maps:keys(Used),
    Undef = [
        {TA, map_get(TA, Used)}
     || TA <- UTAs,
        not is_map_key(TA, Def),
        not is_default_type(TA)
    ],
    foldl(
        fun({TA, L}, St) ->
            add_error(L, {undefined_type, TA}, St)
        end,
        St0,
        Undef
    ).

%% check_bif_clashes(Forms, State0) -> State

check_bif_clashes(Forms, St0) ->
    %% St0#lint.defined is now complete.
    check_option_functions(Forms, nowarn_bif_clash, bad_nowarn_bif_clash, St0).

check_option_functions(Forms, Tag0, Type, St0) ->
    %% There are no line numbers in St0#lint.compile.
    FAsL = [
        {FA, L}
     || {attribute, L, compile, Args} <- Forms,
        {Tag, FAs0} <- lists:flatten([Args]),
        Tag0 =:= Tag,
        FA <- lists:flatten([FAs0])
    ],
    DefFunctions =
        (gb_sets:to_list(St0#lint.defined) -- pseudolocals()) ++
            [{F, A} || {{F, A}, _} <- orddict:to_list(St0#lint.imports)],
    Bad = [{FA, L} || {FA, L} <- FAsL, not member(FA, DefFunctions)],
    func_line_error(Type, Bad, St0).

nowarn_function(Tag, Opts) ->
    ordsets:from_list([FA || {Tag1, FAs} <- Opts, Tag1 =:= Tag, FA <- lists:flatten([FAs])]).

func_line_warning(Type, Fs, St) ->
    foldl(fun({F, Line}, St0) -> add_warning(Line, {Type, F}, St0) end, St, Fs).

func_line_error(Type, Fs, St) ->
    foldl(fun({F, Line}, St0) -> add_error(Line, {Type, F}, St0) end, St, Fs).

check_callback_information(
    #lint{callbacks = Callbacks, optional_callbacks = OptionalCbs, defined = Defined} = St0
) ->
    OptFun = fun(MFA, Line, St) ->
        case is_map_key(MFA, Callbacks) of
            true ->
                St;
            false ->
                add_error(Line, {undefined_callback, MFA}, St)
        end
    end,
    St1 = maps:fold(OptFun, St0, OptionalCbs),
    case gb_sets:is_member({behaviour_info, 1}, Defined) of
        false ->
            St1;
        true ->
            case map_size(Callbacks) of
                0 ->
                    St1;
                _ ->
                    FoldFun = fun(Fa, Line, St) ->
                        add_error(Line, {behaviour_info, Fa}, St)
                    end,
                    maps:fold(FoldFun, St1, Callbacks)
            end
    end.

%% For storing the import list we use the orddict module.
%% We know an empty set is [].

-spec export(line(), [fa()], lint_state()) -> lint_state().
%%  Mark functions as exported, also as called from the export line.

export(Line, Es, #lint{exports = Es0, called = Called} = St0) ->
    {Es1, C1, St1} =
        foldl(
            fun(NA, {E, C, St2}) ->
                St =
                    case gb_sets:is_element(NA, E) of
                        true ->
                            Warn = {duplicated_export, NA},
                            add_warning(Line, Warn, St2);
                        false ->
                            St2
                    end,
                {gb_sets:add_element(NA, E), [{NA, Line} | C], St}
            end,
            {Es0, Called, St0},
            Es
        ),
    St1#lint{exports = Es1, called = C1}.

-spec export_type(line(), [ta()], lint_state()) -> lint_state().
%%  Mark types as exported; also mark them as used from the export line.

export_type(Line, ETs, #lint{usage = Usage, exp_types = ETs0} = St0) ->
    UTs0 = Usage#usage.used_types,
    try
        foldl(
            fun({T, A} = TA, {E, UTs, St2}) when is_atom(T), is_integer(A) ->
                St =
                    case gb_sets:is_element(TA, E) of
                        true ->
                            Warn = {duplicated_export_type, TA},
                            add_warning(Line, Warn, St2);
                        false ->
                            St2
                    end,
                {gb_sets:add_element(TA, E), maps:put(TA, Line, UTs), St}
            end,
            {ETs0, UTs0, St0},
            ETs
        )
    of
        {ETs1, UTs1, St1} ->
            St1#lint{usage = Usage#usage{used_types = UTs1}, exp_types = ETs1}
    catch
        error:_ ->
            add_error(Line, {bad_export_type, ETs}, St0)
    end.

-spec exports(lint_state()) -> gb_sets:set(fa()).
exports(#lint{compile = Opts, defined = Defs, exports = Es}) ->
    case lists:member(export_all, Opts) of
        true -> Defs;
        false -> Es
    end.

-type import() :: {module(), [fa()]} | module().

-spec import(line(), import(), lint_state()) -> lint_state().
import(Line, {Mod, Fs}, St00) ->
    St = check_module_name(Mod, Line, St00),
    Mfs = ordsets:from_list(Fs),
    case check_imports(Line, Mfs, St#lint.imports) of
        [] ->
            St#lint{imports = add_imports(Mod, Mfs, St#lint.imports)};
        Efs ->
            {Err, St1} =
                foldl(
                    fun
                        ({bif, {F, A}, _}, {Err, St0}) ->
                            %% BifClash - import directive
                            Warn =
                                is_warn_enabled(bif_clash, St0) andalso
                                    (not bif_clash_specifically_disabled(St0, {F, A})),
                            AutoImpSup = is_autoimport_suppressed(St0#lint.no_auto, {F, A}),
                            OldBif = erl_internal:old_bif(F, A),
                            {Err,
                                if
                                    Warn and (not AutoImpSup) and OldBif ->
                                        add_error(
                                            Line,
                                            {redefine_old_bif_import, {F, A}},
                                            St0
                                        );
                                    Warn and (not AutoImpSup) ->
                                        add_warning(
                                            Line,
                                            {redefine_bif_import, {F, A}},
                                            St0
                                        );
                                    true ->
                                        St0
                                end};
                        (Ef, {_Err, St0}) ->
                            {true, add_error(Line, {redefine_import, Ef}, St0)}
                    end,
                    {false, St},
                    Efs
                ),
            if
                not Err ->
                    St1#lint{imports = add_imports(Mod, Mfs, St#lint.imports)};
                true ->
                    St1
            end
    end.

check_imports(_Line, Fs, Is) ->
    foldl(
        fun(F, Efs) ->
            case orddict:find(F, Is) of
                {ok, Mod} ->
                    [{F, Mod} | Efs];
                error ->
                    {N, A} = F,
                    case erl_internal:bif(N, A) of
                        true ->
                            [{bif, F, erlang} | Efs];
                        false ->
                            Efs
                    end
            end
        end,
        [],
        Fs
    ).

add_imports(Mod, Fs, Is) ->
    foldl(fun(F, Is0) -> orddict:store(F, Mod, Is0) end, Is, Fs).

-spec imported(atom(), arity(), lint_state()) -> {'yes', module()} | 'no'.
imported(F, A, St) ->
    case orddict:find({F, A}, St#lint.imports) of
        {ok, Mod} -> {yes, Mod};
        error -> no
    end.

-type import_type() :: {module(), [ta()]} | module().

-spec import_type(line(), import_type(), lint_state()) -> lint_state().
import_type(Line, {Mod, Ts}, St00) ->
    St = check_module_name(Mod, Line, St00),
    case check_type_imports(Ts, St#lint.imp_types, St#lint.types) of
        {Imports, []} ->
            St#lint{imp_types = add_imports(Mod, Imports, St#lint.imp_types)};
        {Imports, Errors} ->
            {ReportError, St1} =
                foldl(
                    fun
                        ({built_in, {T, A}}, {Err, St0}) ->
                            %% bif_clash and no_auto also applies to builtin types
                            Warn = is_warn_enabled(bif_clash, St0),
                            AutoImpSup = is_autoimport_suppressed(St0#lint.no_auto, {T, A}),
                            {Err,
                                if
                                    Warn and (not AutoImpSup) ->
                                        add_warning(
                                            Line,
                                            {redefine_builtin_type_import, {T, A}},
                                            St0
                                        );
                                    true ->
                                        St0
                                end};
                        ({local, NA}, {_Err, St0}) ->
                            {true, add_error(Line, {redefine_type, NA}, St0)};
                        ({remote, NA, From}, {_Err, St0}) ->
                            {true, add_error(Line, {redefine_import_type, {NA, From}}, St0)};
                        ({duplicate, NA}, {_Err, St0}) ->
                            {true, add_error(Line, {import_type_duplicate, NA}, St0)}
                    end,
                    {false, St},
                    Errors
                ),
            if
                not ReportError ->
                    St1#lint{imp_types = add_imports(Mod, Imports, St#lint.imp_types)};
                true ->
                    St1
            end
    end.

check_type_imports(Candidates, Imports, Types0) ->
    Types = maps:to_list(Types0),
    Fun = fun({N, A} = T, {Acc, Err}) ->
        case lists:search(fun({Name, _}) -> Name =:= N end, Acc) of
            {value, NA} ->
                {Acc, [{duplicate, NA} | Err]};
            false ->
                Search = fun({{Name, _}, _}) -> Name =:= N end,
                case lists:search(Search, Types) of
                    {value, {NA, _}} ->
                        {Acc, [{local, NA} | Err]};
                    false ->
                        case lists:search(Search, Imports) of
                            {value, {NA, Module}} ->
                                {Acc, [{remote, NA, Module} | Err]};
                            false ->
                                case erl_internal:is_type(N, A) of
                                    true -> {Acc, [{built_in, T} | Err]};
                                    false -> {[T | Acc], Err}
                                end
                        end
                end
        end
    end,
    lists:foldl(Fun, {[], []}, Candidates).

imported_type(Name, St) ->
    case [I || {{T, _}, _} = I <- St#lint.imp_types, T =:= Name] of
        [{{Name, Arity}, Mod}] -> {yes, Mod, Arity};
        _ -> no
    end.

-spec on_load(erl_anno:anno(), fa(), lint_state()) -> lint_state().
%%  Check an on_load directive and remember it.

on_load(Line, {Name, Arity} = Fa, #lint{on_load = OnLoad0} = St0) when
    is_atom(Name), is_integer(Arity)
->
    %% Always add the function name (even if there is a problem),
    %% to avoid irrelevant warnings for unused functions.
    St = St0#lint{on_load = [Fa | OnLoad0], on_load_line = Line},
    case St of
        #lint{on_load = [{_, 0}]} ->
            %% This is the first on_load attribute seen in the module
            %% and it has the correct arity.
            St;
        #lint{on_load = [{_, _}]} ->
            %% Wrong arity.
            add_error(Line, {bad_on_load_arity, Fa}, St);
        #lint{on_load = [_, _ | _]} ->
            %% Multiple on_load attributes.
            add_error(Line, multiple_on_loads, St)
    end;
on_load(Line, Val, St) ->
    %% Bad syntax.
    add_error(Line, {bad_on_load, Val}, St).

check_on_load(#lint{defined = Defined, on_load = [{_, 0} = Fa], on_load_line = Line} = St) ->
    case gb_sets:is_member(Fa, Defined) of
        true -> St;
        false -> add_error(Line, {undefined_on_load, Fa}, St)
    end;
check_on_load(St) ->
    St.

-spec call_function(line(), atom(), arity(), lint_state()) -> lint_state().
%%  Add to both called and calls.

call_function(
    Line,
    F,
    A,
    #lint{usage = Usage0, called = Cd, func = Func, file = File} = St
) ->
    #usage{calls = Cs} = Usage0,
    NA = {F, A},
    Usage =
        case Cs of
            undefined -> Usage0;
            _ -> Usage0#usage{calls = maps_prepend(Func, NA, Cs)}
        end,
    Anno = erl_anno:set_file(File, Line),
    St#lint{called = [{NA, Anno} | Cd], usage = Usage}.

%% function(Line, Name, Arity, Clauses, State) -> State.

function(Line, Name, Arity, Cs, St0) ->
    St1 = St0#lint{func = {Name, Arity}},
    St2 = define_function(Line, Name, Arity, St1),
    clauses(Cs, St2).

-spec define_function(line(), atom(), arity(), lint_state()) -> lint_state().
define_function(Line, Name, Arity, St0) ->
    St1 = keyword_warning(Line, Name, St0),
    NA = {Name, Arity},
    case gb_sets:is_member(NA, St1#lint.defined) of
        true ->
            add_error(Line, {redefine_function, NA}, St1);
        false ->
            St2 = function_check_max_args(Line, Arity, St1),
            St3 = St2#lint{defined = gb_sets:add_element(NA, St2#lint.defined)},
            case imported(Name, Arity, St3) of
                {yes, _M} -> add_error(Line, {define_import, NA}, St3);
                no -> St3
            end
    end.

function_check_max_args(Line, Arity, St) when Arity > ?MAX_ARGUMENTS ->
    add_error(Line, {too_many_arguments, Arity}, St);
function_check_max_args(_, _, St) ->
    St.

%% clauses([Clause], State) -> {VarTable, State}.

clauses(Cs, St) ->
    foldl(
        fun(C, St0) ->
            {_, St1} = clause(C, St0),
            St1
        end,
        St,
        Cs
    ).

clause({clause, _Line, H, G, B}, St0) ->
    Vt0 = [],
    {Hvt, Hnew, St1} = head(H, Vt0, St0),
    Vt1 = vtupdate(Hvt, vtupdate(Hnew, Vt0)),
    {Gvt, St2} = guard(G, Vt1, St1),
    Vt2 = vtupdate(Gvt, Vt1),
    {Bvt, St3} = exprs(B, Vt2, St2),
    Upd = vtupdate(Bvt, Vt2),
    check_unused_vars(Upd, Vt0, St3).

%% head([HeadPattern], VarTable, State) ->
%%      {VarTable,NewVars,State}
%%  Check a patterns in head returning "all" variables. Not updating the
%%  known variable list will result in multiple error messages/warnings.

head(Ps, Vt, St0) ->
    % No imported pattern variables
    head(Ps, Vt, [], St0).

head([P | Ps], Vt, Old, St0) ->
    {Pvt, Pnew, St1} = pattern(P, Vt, Old, St0),
    {Psvt, Psnew, St2} = head(Ps, Vt, Old, St1),
    {vtmerge_pat(Pvt, Psvt), vtmerge_pat(Pnew, Psnew), St2};
head([], _Vt, _Env, St) ->
    {[], [], St}.

%% pattern(Pattern, VarTable, Old, State) ->
%%                  {UpdVarTable,NewVars,State}.
%%  Check pattern return variables. VarTable is the set of variables
%%  outside the pattern, used for map keys and binary sizes. Old is empty
%%  if vars are shadowing, as in fun heads and comprehension generators, or
%%  is otherwise equal to VarTable; this is used for deciding whether an
%%  occurrence is a binding occurrence or a use.
%%  UpdVarTable is updated when a previously existing variable is used in
%%  the pattern. New variables and their uses are recorded in NewVars. The
%%  caller can then decide what to do with it depending on whether or not
%%  variables in the pattern shadow old variables. This separation is
%%  one way of dealing with these:
%%  A = 4, fun(<<A:A>>) -> % A #2 unused
%%  A = 4, fun(<<A:8,16:A>>) -> % A #1 unused

pattern(P, Vt, St) ->
    % No imported pattern variables
    pattern(P, Vt, [], St).

pattern({var, _Line, '_'}, _Vt, _Old, St) ->
    %Ignore anonymous variable
    {[], [], St};
pattern({var, Line, V}, _Vt, Old, St) ->
    pat_var(V, Line, Old, [], St);
pattern({op, _, '^', {var, Line, V}}, Vt, _Old, St) when V =/= '_' ->
    %% this is checked like a normal expression variable,
    %% since it will actually become a guard test
    {Vt1, St1} = expr_var(V, Line, Vt, St),
    {Vt1, [], St1};
pattern({char, _Line, _C}, _Vt, _Old, St) ->
    {[], [], St};
pattern({integer, _Line, _I}, _Vt, _Old, St) ->
    {[], [], St};
pattern({float, _Line, _F}, _Vt, _Old, St) ->
    {[], [], St};
pattern({atom, Line, A}, _Vt, _Old, St) ->
    {[], [], keyword_warning(Line, A, St)};
pattern({atom_expr, _, _}, _Vt, _Old, St) ->
    {[], [], St};
pattern({string, _Line, _S}, _Vt, _Old, St) ->
    {[], [], St};
pattern({nil, _Line}, _Vt, _Old, St) ->
    {[], [], St};
pattern({cons, _Line, H, T}, Vt, Old, St0) ->
    {Hvt, Hnew, St1} = pattern(H, Vt, Old, St0),
    {Tvt, Tnew, St2} = pattern(T, Vt, Old, St1),
    {vtmerge_pat(Hvt, Tvt), vtmerge_pat(Hnew, Tnew), St2};
pattern({tuple, _Line, Ps}, Vt, Old, St) ->
    pattern_list(Ps, Vt, Old, St);
pattern({enum, Line, Name, Variant, Fields}, Vt, Old, St0) ->
    Result = check_enum(Line, Name, Variant, St0, fun(ResolvedName, Defs, St) ->
        pattern_fields(Fields, Line, ResolvedName, Defs, Vt, Old, St)
    end),
    expr_check_result_in_pattern(Result);
pattern({map, Line, Ps}, Vt, Old, St) ->
    pattern_map(Ps, Vt, Old, check_map_allowed(Line, St));
pattern({shape, _Line, Pfs}, Vt, Old, St) ->
    check_shape_pattern_fields(Pfs, Vt, Old, St, []);
pattern({struct, Line, Name, Fields}, Vt, Old, St0) ->
    Result = check_struct(Line, Name, St0, fun(ResolvedName, Defs, St) ->
        pattern_fields(Fields, Line, ResolvedName, Defs, Vt, Old, St)
    end),
    expr_check_result_in_pattern(Result);
pattern({struct_index, Line, Name, Field}, _Vt, _Old, St0) ->
    Result = check_struct(Line, Name, St0, fun(ResolvedName, Defs, St) ->
        field(Field, ResolvedName, Defs, St)
    end),
    expr_check_result_in_pattern(Result);
pattern({bin, _, Fs}, Vt, Old, St) ->
    pattern_bin(Fs, Vt, Old, St);
pattern({op, _Line, '++', {nil, _}, R}, Vt, Old, St) ->
    pattern(R, Vt, Old, St);
pattern({op, _Line, '++', {cons, Li, {char, _L2, _C}, T}, R}, Vt, Old, St) ->
    %Char unimportant here
    pattern({op, Li, '++', T, R}, Vt, Old, St);
pattern({op, _Line, '++', {cons, Li, {integer, _L2, _I}, T}, R}, Vt, Old, St) ->
    %Weird, but compatible!
    pattern({op, Li, '++', T, R}, Vt, Old, St);
pattern({op, _Line, '++', {string, _Li, _S}, R}, Vt, Old, St) ->
    %String unimportant here
    pattern(R, Vt, Old, St);
pattern({match, _Line, Pat1, Pat2}, Vt, Old, St0) ->
    {Lvt, Lnew, St1} = pattern(Pat1, Vt, Old, St0),
    {Rvt, Rnew, St2} = pattern(Pat2, Vt, Old, St1),
    St3 = reject_invalid_alias(Pat1, Pat2, Vt, St2),
    {vtmerge_pat(Lvt, Rvt), vtmerge_pat(Lnew, Rnew), St3};
%% Catch legal constant expressions, including unary +,-.
pattern(Pat, _Vt, _Old, St) ->
    case is_pattern_expr(Pat) of
        true -> {[], [], St};
        false -> {[], [], add_error(element(2, Pat), illegal_pattern, St)}
    end.

pattern_list(Ps, Vt, Old, St) ->
    foldl(
        fun(P, {Psvt, Psnew, St0}) ->
            {Pvt, Pnew, St1} = pattern(P, Vt, Old, St0),
            {vtmerge_pat(Pvt, Psvt), vtmerge_pat(Psnew, Pnew), St1}
        end,
        {[], [], St},
        Ps
    ).

expr_check_result_in_pattern({Vt, St}) -> {Vt, [], St};
expr_check_result_in_pattern({Vt, New, St}) -> {Vt, New, St}.

%% reject_invalid_alias(Pat, Expr, Vt, St) -> St'
%%  Reject aliases for binary patterns at the top level.
%%  Reject aliases for maps patterns at the top level.
%%  The variables table (Vt) are for maps checkking.

reject_invalid_alias_expr({bin, _, _} = P, {match, _, P0, E}, Vt, St0) ->
    St = reject_invalid_alias(P, P0, Vt, St0),
    reject_invalid_alias_expr(P, E, Vt, St);
reject_invalid_alias_expr({map, _, _} = P, {match, _, P0, E}, Vt, St0) ->
    St = reject_invalid_alias(P, P0, Vt, St0),
    reject_invalid_alias_expr(P, E, Vt, St);
reject_invalid_alias_expr({match, _, _, _} = P, {match, _, P0, E}, Vt, St0) ->
    St = reject_invalid_alias(P, P0, Vt, St0),
    reject_invalid_alias_expr(P, E, Vt, St);
reject_invalid_alias_expr(_, _, _, St) ->
    St.

%% reject_invalid_alias(Pat1, Pat2, St) -> St'
%%  Aliases of binary patterns, such as <<A:8>> = <<B:4,C:4>> or even
%%  <<A:8>> = <<A:8>>, are not allowed. Traverse the patterns in parallel
%%  and generate an error if any binary aliases are found.
%%    We generate an error even if is obvious that the overall pattern can't
%%  possibly match, for instance, {a,<<A:8>>,c}={x,<<A:8>>} WILL generate an
%%  error.
%%    Maps should reject unbound variables here.

reject_invalid_alias({bin, Line, _}, {bin, _, _}, _, St) ->
    add_error(Line, illegal_bin_pattern, St);
reject_invalid_alias({map, _Line, Ps1}, {map, _, Ps2}, Vt, St0) ->
    Fun = fun
        ({map_field_exact, L, {var, _, K}, _V}, Sti) ->
            case is_var_bound(K, Vt) of
                true ->
                    Sti;
                false ->
                    add_error(L, {unbound_var, K}, Sti)
            end;
        ({map_field_exact, _L, _K, _V}, Sti) ->
            Sti
    end,
    foldl(Fun, foldl(Fun, St0, Ps1), Ps2);
reject_invalid_alias({cons, _, H1, T1}, {cons, _, H2, T2}, Vt, St0) ->
    St = reject_invalid_alias(H1, H2, Vt, St0),
    reject_invalid_alias(T1, T2, Vt, St);
reject_invalid_alias({tuple, _, Es1}, {tuple, _, Es2}, Vt, St) ->
    reject_invalid_alias_list(Es1, Es2, Vt, St);
reject_invalid_alias({match, _, P1, P2}, P, Vt, St0) ->
    St = reject_invalid_alias(P1, P, Vt, St0),
    reject_invalid_alias(P2, P, Vt, St);
reject_invalid_alias(P, {match, _, _, _} = M, Vt, St) ->
    reject_invalid_alias(M, P, Vt, St);
reject_invalid_alias(_P1, _P2, _Vt, St) ->
    St.

reject_invalid_alias_list([E1 | Es1], [E2 | Es2], Vt, St0) ->
    St = reject_invalid_alias(E1, E2, Vt, St0),
    reject_invalid_alias_list(Es1, Es2, Vt, St);
reject_invalid_alias_list(_, _, _, St) ->
    St.

%% is_pattern_expr(Expression) -> boolean().
%%  Test if a general expression is a valid pattern expression.

is_pattern_expr(Expr) ->
    case is_pattern_expr_1(Expr) of
        false ->
            false;
        true ->
            %% Expression is syntactically correct - make sure that it
            %% also can be evaluated.
            case erl_eval:partial_eval(Expr) of
                {integer, _, _} -> true;
                {char, _, _} -> true;
                {float, _, _} -> true;
                {atom, _, _} -> true;
                _ -> false
            end
    end.

is_pattern_expr_1({char, _Line, _C}) ->
    true;
is_pattern_expr_1({integer, _Line, _I}) ->
    true;
is_pattern_expr_1({float, _Line, _F}) ->
    true;
is_pattern_expr_1({atom, _Line, _A}) ->
    true;
is_pattern_expr_1({tuple, _Line, Es}) ->
    all(fun is_pattern_expr/1, Es);
is_pattern_expr_1({enum, _Line, {remote, _, M, E}, C, Es}) ->
    all(fun is_pattern_expr/1, [M, E, C | Es]);
is_pattern_expr_1({enum, _Line, E, C, Es}) ->
    all(fun is_pattern_expr/1, [E, C | Es]);
is_pattern_expr_1({nil, _Line}) ->
    true;
is_pattern_expr_1({cons, _Line, H, T}) ->
    is_pattern_expr_1(H) andalso is_pattern_expr_1(T);
is_pattern_expr_1({op, _Line, Op, A}) ->
    erl_internal:arith_op(Op, 1) andalso is_pattern_expr_1(A);
is_pattern_expr_1({op, _Line, Op, A1, A2}) ->
    erl_internal:arith_op(Op, 2) andalso all(fun is_pattern_expr/1, [A1, A2]);
is_pattern_expr_1(_Other) ->
    false.

pattern_map(Ps, Vt, Old, St) ->
    foldl(
        fun
            ({map_field_assoc, L, _, _}, {Psvt, Psnew, St0}) ->
                {Psvt, Psnew, add_error(L, illegal_pattern, St0)};
            ({map_field_exact, _L, K, V}, {Psvt, Psnew, St0}) ->
                St1 = St0#lint{gexpr_context = map_key},
                {Kvt, St2} = gexpr(K, Vt, St1),
                {Vvt, Vnew, St3} = pattern(V, Vt, Old, St2),
                {vtmerge_pat(vtmerge_pat(Kvt, Vvt), Psvt), vtmerge_pat(Psnew, Vnew), St3}
        end,
        {[], [], St},
        Ps
    ).

%% pattern_bin([Element], VarTable, Old, State) ->
%%           {UpdVarTable,NewVars,State}.
%%  Check a pattern group.

pattern_bin(Es, Vt, Old, St0) ->
    {_Sz, Esvt, Esnew, St1} = foldl(
        fun(E, Acc) ->
            pattern_element(E, Vt, Old, Acc)
        end,
        {0, [], [], St0},
        Es
    ),
    {Esvt, Esnew, St1}.

pattern_element(
    {bin_element, Line, {string, _, _}, Size, Ts} = Be,
    Vt,
    Old,
    {Sz, Esvt, Esnew, St0} = Acc
) ->
    case good_string_size_type(Size, Ts) of
        true ->
            pattern_element_1(Be, Vt, Old, Acc);
        false ->
            St = add_error(Line, typed_literal_string, St0),
            {Sz, Esvt, Esnew, St}
    end;
pattern_element(Be, Vt, Old, Acc) ->
    pattern_element_1(Be, Vt, Old, Acc).

pattern_element_1({bin_element, Line, E, Sz0, Ts}, Vt, Old, {Size0, Esvt, Esnew, St0}) ->
    {Pevt, Penew, St1} = pat_bit_expr(E, Vt, Old, Esnew, St0),
    {Sz1, Szvt, Sznew, St2} = pat_bit_size(Sz0, Vt, Esnew, St1),
    {Sz2, Bt, St3} = bit_type(Line, Sz1, Ts, St2),
    {Sz3, St4} = bit_size_check(Line, Sz2, Bt, St3),
    Sz4 =
        case {E, Sz3} of
            {{string, _, S}, all} -> 8 * length(S);
            {_, _} -> Sz3
        end,
    {Size1, St5} = add_bit_size(Line, Sz4, Size0, false, St4),
    {Size1, vtmerge(Szvt, vtmerge(Pevt, Esvt)), vtmerge(Sznew, vtmerge(Esnew, Penew)), St5}.

good_string_size_type(default, default) ->
    true;
good_string_size_type(default, Ts) ->
    lists:any(
        fun
            (utf8) -> true;
            (utf16) -> true;
            (utf32) -> true;
            (_) -> false
        end,
        Ts
    );
good_string_size_type(_, _) ->
    false.

%% pat_bit_expr(Pattern, VarTable, OldVarTable, NewVars, State) ->
%%              {UpdVarTable,UpdNewVars,State}.
%%  Check pattern bit expression, only allow really valid patterns!

pat_bit_expr({op, _, '^', {var, Ln, V}}, Vt, _Old, _New, St) when V =/= '_' ->
    %% this is checked like a normal expression variable,
    %% since it will actually become a guard test
    {Vt1, St1} = expr_var(V, Ln, Vt, St),
    {Vt1, [], St1};
pat_bit_expr({var, _, '_'}, _Vt, _Old, _New, St) ->
    {[], [], St};
pat_bit_expr({var, Ln, V}, _Vt, Old, New, St) ->
    pat_var(V, Ln, Old, New, St);
pat_bit_expr({string, _, _}, _Vt, _Old, _new, St) ->
    {[], [], St};
pat_bit_expr({bin, L, _}, _Vt, _Old, _New, St) ->
    {[], [], add_error(L, illegal_pattern, St)};
pat_bit_expr(P, _Vt, _Old, _New, St) ->
    case is_pattern_expr(P) of
        true -> {[], [], St};
        false -> {[], [], add_error(element(2, P), illegal_pattern, St)}
    end.

%% pat_bit_size(Size, VarTable, NewVars, State) ->
%%             {Value,UpdVarTable,UpdNewVars,State}.
%%  Check pattern size expression, only allow really valid sizes!

pat_bit_size(default, _Vt, _New, St) ->
    {default, [], [], St};
pat_bit_size({var, Lv, V}, Vt0, New0, St0) ->
    {Vt, New, St1} = pat_binsize_var(V, Lv, Vt0, New0, St0),
    {unknown, Vt, New, St1};
pat_bit_size(Size, Vt0, New0, St0) ->
    Line = element(2, Size),
    case erl_eval:partial_eval(Size) of
        {integer, Line, I} ->
            {I, [], [], St0};
        Expr ->
            %% The size is an expression using operators
            %% and/or guard BIFs calls. If the expression
            %% happens to evaluate to a non-integer value, the
            %% pattern will fail to match.
            St1 = St0#lint{bvt = New0, gexpr_context = bin_seg_size},
            {Vt, #lint{bvt = New} = St2} = gexpr(Size, Vt0, St1),
            St3 = St2#lint{bvt = none, gexpr_context = St0#lint.gexpr_context},
            St =
                case is_bit_size_illegal(Expr) of
                    true ->
                        %% The size is a non-integer literal or a simple
                        %% expression that does not evaluate to an
                        %% integer value. Issue a warning.
                        add_warning(Line, non_integer_bitsize, St3);
                    false ->
                        St3
                end,
            {unknown, Vt, New, St}
    end.

is_bit_size_illegal({atom, _, _}) -> true;
is_bit_size_illegal({bin, _, _}) -> true;
is_bit_size_illegal({cons, _, _, _}) -> true;
is_bit_size_illegal({float, _, _}) -> true;
is_bit_size_illegal({map, _, _}) -> true;
is_bit_size_illegal({nil, _}) -> true;
is_bit_size_illegal({tuple, _, _}) -> true;
is_bit_size_illegal(_) -> false.

%% expr_bin(Line, [Element], VarTable, State, CheckFun) -> {UpdVarTable,State}.
%%  Check an expression group.

expr_bin(Es, Vt, St0, Check) ->
    {_Sz, Esvt, St1} = foldl(
        fun(E, Acc) -> bin_element(E, Vt, Acc, Check) end,
        {0, [], St0},
        Es
    ),
    {Esvt, St1}.

bin_element({bin_element, Line, E, Sz0, Ts}, Vt, {Size0, Esvt, St0}, Check) ->
    {Vt1, St1} = Check(E, Vt, St0),
    {Sz1, Vt2, St2} = bit_size(Sz0, Vt, St1, Check),
    {Sz2, Bt, St3} = bit_type(Line, Sz1, Ts, St2),
    {Sz3, St4} = bit_size_check(Line, Sz2, Bt, St3),
    {Size1, St5} = add_bit_size(Line, Sz3, Size0, true, St4),
    {Size1, vtmerge([Vt2, Vt1, Esvt]), St5}.

bit_size(default, _Vt, St, _Check) ->
    {default, [], St};
bit_size({atom, _Line, all}, _Vt, St, _Check) ->
    {all, [], St};
bit_size(Size, Vt, St, Check) ->
    %% Try to safely evaluate Size if constant to get size,
    %% otherwise just treat it as an expression.
    case is_gexpr(Size, St) of
        true ->
            case erl_eval:partial_eval(Size) of
                {integer, _ILn, I} ->
                    {I, [], St};
                _Other ->
                    {Evt, St1} = Check(Size, Vt, St),
                    {unknown, Evt, St1}
            end;
        false ->
            {Evt, St1} = Check(Size, Vt, St),
            {unknown, Evt, St1}
    end.

%% bit_type(Line, Size, TypeList, State) ->  {Size,#bittype,St}.
%%  Perform warning check on type and size.

bit_type(Line, Size0, Type, St) ->
    case erl_bits:set_bit_type(Size0, Type) of
        {ok, Size1, Bt} ->
            {Size1, Bt, St};
        {error, What} ->
            %% Flag error and generate a default.
            {ok, Size1, Bt} = erl_bits:set_bit_type(default, []),
            {Size1, Bt, add_error(Line, What, St)}
    end.

%% bit_size_check(Line, Size, BitType, State) -> {BitSize,State}.
%%  Do some checking & warnings on types
%%   float == 32 or 64

bit_size_check(_Line, unknown, _, St) ->
    {unknown, St};
bit_size_check(_Line, undefined, #bittype{type = Type}, St) ->
    %Assertion.
    true = (Type =:= utf8) or (Type =:= utf16) or (Type =:= utf32),
    {undefined, St};
bit_size_check(Line, all, #bittype{type = Type}, St) ->
    case Type of
        binary -> {all, St};
        _ -> {unknown, add_error(Line, illegal_bitsize, St)}
    end;
bit_size_check(Line, Size, #bittype{type = Type, unit = Unit}, St) ->
    %Total number of bits!
    Sz = Unit * Size,
    St2 = elemtype_check(Line, Type, Sz, St),
    {Sz, St2}.

elemtype_check(_Line, float, 32, St) -> St;
elemtype_check(_Line, float, 64, St) -> St;
elemtype_check(Line, float, _Size, St) -> add_warning(Line, {bad_bitsize, "float"}, St);
elemtype_check(_Line, _Type, _Size, St) -> St.

%% add_bit_size(Line, ElementSize, BinSize, Build, State) -> {Size,State}.
%%  Add bits to group size.

add_bit_size(Line, _Sz1, all, false, St) ->
    {all, add_error(Line, unsized_binary_not_at_end, St)};
add_bit_size(_Line, _Sz1, all, true, St) ->
    {all, St};
add_bit_size(_Line, all, _Sz2, _B, St) ->
    {all, St};
add_bit_size(_Line, undefined, _Sz2, _B, St) ->
    {undefined, St};
add_bit_size(_Line, unknown, _Sz2, _B, St) ->
    {unknown, St};
add_bit_size(_Line, _Sz1, undefined, _B, St) ->
    {unknown, St};
add_bit_size(_Line, _Sz1, unknown, _B, St) ->
    {unknown, St};
add_bit_size(_Line, Sz1, Sz2, _B, St) ->
    {Sz1 + Sz2, St}.

%% guard([GuardTest], VarTable, State) ->
%%      {UsedVarTable,State}
%%  Check a guard, return all variables.

%% Disjunction of guard conjunctions
guard([L | R], Vt, St0) when is_list(L) ->
    {Gvt, St1} = guard_tests(L, Vt, St0),
    {Gsvt, St2} = guard(R, vtupdate(Gvt, Vt), St1),
    {vtupdate(Gvt, Gsvt), St2};
guard(L, Vt, St0) ->
    guard_tests(L, Vt, St0).

%% guard conjunction
guard_tests([G | Gs], Vt, St0) ->
    {Gvt, St1} = guard_test(G, Vt, St0),
    {Gsvt, St2} = guard_tests(Gs, vtupdate(Gvt, Vt), St1),
    {vtupdate(Gvt, Gsvt), St2};
guard_tests([], _Vt, St) ->
    {[], St}.

%% guard_test(Test, VarTable, State) ->
%%      {UsedVarTable,State'}
%%  Check one guard test, returns NewVariables.  We now allow more
%%  expressions in guards including the new is_XXX type tests, but
%%  only allow the old type tests at the top level.

guard_test(G, Vt, St0) ->
    St1 = obsolete_guard(G, St0),
    guard_test2(G, Vt, St1).

guard_test2({call, Line, {atom, _La, F}, As} = G, Vt, St0) ->
    %Always check this.
    {Asvt, St1} = gexpr_list(As, Vt, St0),
    A = length(As),
    case erl_internal:type_test(F, A) of
        true when F =/= is_record, A =/= 2 ->
            case no_guard_bif_clash(St1, {F, A}) of
                false ->
                    {Asvt, add_error(Line, {illegal_guard_local_call, {F, A}}, St1)};
                true ->
                    {Asvt, St1}
            end;
        _ ->
            gexpr(G, Vt, St0)
    end;
guard_test2(G, Vt, St) ->
    %% Everything else is a guard expression.
    gexpr(G, Vt, St).

%% gexpr(GuardExpression, VarTable, State) ->
%%      {UsedVarTable,State'}
%%  Check a guard expression, returns NewVariables.

gexpr({var, Line, V}, Vt, St) ->
    expr_var(V, Line, Vt, St);
gexpr({char, _Line, _C}, _Vt, St) ->
    {[], St};
gexpr({integer, _Line, _I}, _Vt, St) ->
    {[], St};
gexpr({float, _Line, _F}, _Vt, St) ->
    {[], St};
gexpr({atom, Line, A}, _Vt, St) ->
    {[], keyword_warning(Line, A, St)};
gexpr({atom_expr, _, _}, _Vt, St) ->
    {[], St};
gexpr({string, _Line, _S}, _Vt, St) ->
    {[], St};
gexpr({nil, _Line}, _Vt, St) ->
    {[], St};
gexpr({cons, _Line, H, T}, Vt, St) ->
    gexpr_list([H, T], Vt, St);
gexpr({tuple, _Line, Es}, Vt, St) ->
    gexpr_list(Es, Vt, St);
gexpr({enum, Line, Name, Variant, Fields}, Vt, St) ->
    check_enum(Line, Name, Variant, St, fun(ResolvedName, Def, St1) ->
        init_fields_guard(Fields, Line, ResolvedName, Def, Vt, St1)
    end);
gexpr({map, Line, Es}, Vt, St) ->
    map_fields(Es, Vt, check_assoc_fields(Es, check_map_allowed(Line, St)), fun gexpr_list/3);
gexpr({map, Line, Src, Es}, Vt, St) ->
    {Svt, St1} = gexpr(Src, Vt, check_map_allowed(Line, St)),
    {Fvt, St2} = map_fields(Es, Vt, St1, fun gexpr_list/3),
    {vtmerge(Svt, Fvt), St2};
gexpr({shape, _Line, Fields}, Vt, St) ->
    check_shape_fields(Fields, Vt, St, fun gexpr/3);
gexpr({shape_update, _Line, Expr, Fields}, Vt, St) ->
    {Svt, St1} = gexpr(Expr, Vt, St),
    check_shape_fields(Fields, vtmerge(Vt, Svt), St1, fun gexpr/3);
gexpr({shape_field, _Line, Expr, _Field}, Vt, St) ->
    gexpr(Expr, Vt, St);
gexpr({struct, Line, Name, Fields}, Vt, St) ->
    check_struct(Line, Name, St, fun(ResolvedName, Defs, St1) ->
        init_fields_guard(Fields, Line, ResolvedName, Defs, Vt, St1)
    end);
gexpr({struct_field, Line, Rec, Name, Field}, Vt, St0) ->
    {Rvt, St1} = gexpr(Rec, Vt, St0),
    {Fvt, St2} =
        check_struct(Line, Name, St1, fun(ResolvedName, Defs, St) ->
            field(Field, ResolvedName, Defs, St)
        end),
    {vtmerge(Rvt, Fvt), St2};
gexpr({struct_index, Line, Name, Field}, _Vt, St0) ->
    check_struct(Line, Name, St0, fun(ResolvedName, Defs, St) ->
        field(Field, ResolvedName, Defs, St)
    end);
gexpr({bin, _Line, Fs}, Vt, St) ->
    expr_bin(Fs, Vt, St, fun gexpr/3);
gexpr({call, Line, {atom, _, is_record}, [_, _]}, _Vt, St) ->
    {[], add_error(Line, unsupported_is_record, St)};
gexpr({call, Line, {remote, _, {atom, _, erlang}, {atom, _, is_record}}, [_, _]}, _Vt, St) ->
    {[], add_error(Line, unsupported_is_record, St)};
gexpr({call, Line, {atom, _Lr, is_record}, [_, _, _]}, _Vt, St) ->
    {[], add_error(Line, unsupported_is_record, St)};
gexpr({call, Line, {remote, _, {atom, _, erlang}, {atom, _, is_record}}, [_, _, _]}, _Vt, St) ->
    {[], add_error(Line, unsupported_is_record, St)};
gexpr({call, Line, {atom, _La, F}, As}, Vt, St0) ->
    {Asvt, St1} = gexpr_list(As, Vt, St0),
    A = length(As),
    %% BifClash - Function called in guard
    case erl_internal:guard_bif(F, A) andalso no_guard_bif_clash(St1, {F, A}) of
        true ->
            %% Assert that it is auto-imported.
            true = erl_internal:bif(F, A),
            case St0#lint.gexpr_context =:= field_default of
                true when {F, A} =:= {node, 0}; {F, A} =:= {self, 0} ->
                    {Asvt, add_error(Line, illegal_field_default, St1)};
                _ ->
                    {Asvt, St1}
            end;
        false ->
            case
                is_local_function(St1#lint.locals, {F, A}) orelse
                    is_imported_function(St1#lint.imports, {F, A})
            of
                true ->
                    {Asvt, add_error(Line, {illegal_guard_local_call, {F, A}}, St1)};
                _ ->
                    {Asvt, add_error(Line, illegal_guard_expr, St1)}
            end
    end;
gexpr({call, Line, {remote, _Lr, {atom, _Lm, erlang}, {atom, _Lf, F}}, As}, Vt, St0) ->
    {Asvt, St1} = gexpr_list(As, Vt, St0),
    A = length(As),
    case erl_internal:guard_bif(F, A) orelse is_gexpr_op(F, A) of
        true ->
            case St0#lint.gexpr_context =:= field_default of
                true when {F, A} =:= {node, 0}; {F, A} =:= {self, 0} ->
                    {Asvt, add_error(Line, illegal_field_default, St1)};
                _ ->
                    {Asvt, St1}
            end;
        false ->
            {Asvt, add_error(Line, illegal_guard_expr, St1)}
    end;
gexpr({op, Line, Op, A}, Vt, St0) ->
    {Avt, St1} = gexpr(A, Vt, St0),
    case is_gexpr_op(Op, 1) of
        true -> {Avt, St1};
        false -> {Avt, add_error(Line, illegal_guard_expr, St1)}
    end;
gexpr({op, _, 'andalso', L, R}, Vt, St) ->
    gexpr_list([L, R], Vt, St);
gexpr({op, _, 'orelse', L, R}, Vt, St) ->
    gexpr_list([L, R], Vt, St);
gexpr({op, Line, Op, L, R}, Vt, St0) ->
    {Avt, St1} = gexpr_list([L, R], Vt, St0),
    case is_gexpr_op(Op, 2) of
        true -> {Avt, St1};
        false -> {Avt, add_error(Line, illegal_guard_expr, St1)}
    end;
%% Everything else is illegal! You could put explicit tests here to
%% better error diagnostics.
gexpr(E, _Vt, St) ->
    {[], add_error(element(2, E), illegal_guard_expr, St)}.

%% gexpr_list(Expressions, VarTable, State) ->
%%      {UsedVarTable,State'}

gexpr_list(Es, Vt, St) ->
    foldl(
        fun(E, {Esvt, St0}) ->
            {Evt, St1} = gexpr(E, Vt, St0),
            {vtmerge(Evt, Esvt), St1}
        end,
        {[], St},
        Es
    ).

%% is_guard_test2(Expression, St :: #lint{}) -> boolean().
is_guard_test2({call, Line, {atom, Lr, record}, [E, A]}, St) ->
    is_gexpr({call, Line, {atom, Lr, is_record}, [E, A]}, St);
is_guard_test2({call, _Line, {atom, _La, Test}, As} = Call, St) ->
    #lint{locals = Locals, imports = Imports} = St,
    A = length(As),
    FA = {Test, A},
    IsOverridden = is_local_function(Locals, FA) orelse is_imported_function(Imports, FA),
    not IsOverridden andalso
        case erl_internal:type_test(Test, A) of
            true -> is_gexpr_list(As, St);
            false -> is_gexpr(Call, St)
        end;
is_guard_test2(G, St) ->
    %%Everything else is a guard expression.
    is_gexpr(G, St).

%% is_guard_expr(Expression) -> boolean().
%%  Test if an expression is a guard expression.

is_guard_expr(E) -> is_gexpr(E, {[], fun({_, _}) -> false end}).

is_gexpr({var, _L, _V}, _St) ->
    true;
is_gexpr({char, _L, _C}, _St) ->
    true;
is_gexpr({integer, _L, _I}, _St) ->
    true;
is_gexpr({float, _L, _F}, _St) ->
    true;
is_gexpr({atom, _L, _A}, _St) ->
    true;
is_gexpr({atom_expr, _, _}, _St) ->
    true;
is_gexpr({string, _L, _S}, _St) ->
    true;
is_gexpr({nil, _L}, _St) ->
    true;
is_gexpr({cons, _L, H, T}, St) ->
    is_gexpr_list([H, T], St);
is_gexpr({tuple, _L, Es}, St) ->
    is_gexpr_list(Es, St);
is_gexpr({enum, _L, {remote, _, M, E}, C, Es}, St) ->
    is_gexpr_list([M, E, C | Es], St);
is_gexpr({enum, _L, E, C, Es}, St) ->
    is_gexpr_list([E, C | Es], St);
%%is_gexpr({struct,_L,_Tag,Es}, St) ->
%%    is_gexpr_list(Es, St);
is_gexpr({map, _L, Es}, St) ->
    is_map_fields(Es, St);
is_gexpr({map, _L, Src, Es}, St) ->
    is_gexpr(Src, St) andalso is_map_fields(Es, St);
is_gexpr({bin, _L, Fs}, St) ->
    all(
        fun({bin_element, _Line, E, Sz, _Ts}) ->
            is_gexpr(E, St) and (Sz =:= default orelse is_gexpr(Sz, St))
        end,
        Fs
    );
is_gexpr({call, _L, {atom, _Lf, F}, As}, St) ->
    #lint{locals = Locals, imports = Imports} = St,
    A = length(As),
    FA = {F, A},
    IsOverridden = is_local_function(Locals, FA) orelse is_imported_function(Imports, FA),
    not IsOverridden andalso erl_internal:guard_bif(F, A) andalso is_gexpr_list(As, St);
is_gexpr({call, _L, {remote, _Lr, {atom, _Lm, erlang}, {atom, _Lf, F}}, As}, St) ->
    A = length(As),
    (erl_internal:guard_bif(F, A) orelse is_gexpr_op(F, A)) andalso
        is_gexpr_list(As, St);
is_gexpr({call, L, {tuple, Lt, [{atom, Lm, erlang}, {atom, Lf, F}]}, As}, St) ->
    is_gexpr({call, L, {remote, Lt, {atom, Lm, erlang}, {atom, Lf, F}}, As}, St);
is_gexpr({op, _L, Op, A}, St) ->
    is_gexpr_op(Op, 1) andalso is_gexpr(A, St);
is_gexpr({op, _L, 'andalso', A1, A2}, St) ->
    is_gexpr_list([A1, A2], St);
is_gexpr({op, _L, 'orelse', A1, A2}, St) ->
    is_gexpr_list([A1, A2], St);
is_gexpr({op, _L, Op, A1, A2}, St) ->
    is_gexpr_op(Op, 2) andalso is_gexpr_list([A1, A2], St);
is_gexpr(_Other, _St) ->
    false.

is_gexpr_op(Op, A) ->
    try erl_internal:op_type(Op, A) of
        arith -> true;
        bool -> true;
        comp -> true;
        list -> false;
        send -> false
    catch
        _:_ -> false
    end.

is_gexpr_list(Es, St) -> all(fun(E) -> is_gexpr(E, St) end, Es).

is_map_fields([{Tag, _, K, V} | Fs], St) when Tag =:= map_field_assoc; Tag =:= map_field_exact ->
    is_gexpr(K, St) andalso
        is_gexpr(V, St) andalso
        is_map_fields(Fs, St);
is_map_fields([], _St) ->
    true;
is_map_fields(_T, _St) ->
    false.

%% exprs(Sequence, VarTable, State) ->
%%      {UsedVarTable,State'}
%%  Check a sequence of expressions, return all variables.

exprs([E | Es], Vt, St0) ->
    {Evt, St1} = expr(E, Vt, St0),
    {Esvt, St2} = exprs(Es, vtupdate(Evt, Vt), St1),
    {vtupdate(Evt, Esvt), St2};
exprs([], _Vt, St) ->
    {[], St}.

%% expr(Expression, VarTable, State) ->
%%      {UsedVarTable,State'}
%%  Check an expression, returns NewVariables. Assume naive users and
%%  mark illegally exported variables, e.g. from catch, as unsafe to better
%%  show why unbound.

expr({var, Line, V}, Vt, St) ->
    expr_var(V, Line, Vt, St);
expr({char, _Line, _C}, _Vt, St) ->
    {[], St};
expr({integer, _Line, _I}, _Vt, St) ->
    {[], St};
expr({float, _Line, _F}, _Vt, St) ->
    {[], St};
expr({atom, Line, A}, _Vt, St) ->
    {[], keyword_warning(Line, A, St)};
expr({atom_expr, _, _}, _Vt, St) ->
    {[], St};
expr({string, _Line, _S}, _Vt, St) ->
    {[], St};
expr({nil, _Line}, _Vt, St) ->
    {[], St};
expr({cons, Line, H, T}, Vt, St) ->
    exp_expr_list({list, Line}, [H, T], Vt, St);
expr({lc, _Line, E, Qs}, Vt, St) ->
    handle_comprehension(E, Qs, Vt, St);
expr({bc, _Line, E, Qs}, Vt, St) ->
    handle_comprehension(E, Qs, Vt, St);
expr({tuple, Line, Es}, Vt, St) ->
    exp_expr_list({tuple, Line}, Es, Vt, St);
expr({enum, Line, Name, Variant, Fields}, Vt0, St0) ->
    {Vt1, St1} = check_enum(Line, Name, Variant, St0, fun(ResolvedName, Def, St) ->
        init_fields(Fields, Line, ResolvedName, Def, Vt0, St)
    end),
    {vtupd_export({enum, Line}, Vt1, Vt0), St1};
expr({map, Line, Es}, Vt, St) ->
    map_fields(
        Es,
        Vt,
        check_assoc_fields(Es, check_map_allowed(Line, St)),
        fun(Es0, Vt0, St0) ->
            exp_expr_list({map, Line}, Es0, Vt0, St0)
        end
    );
expr({map, Line, Src, Es}, Vt, St) ->
    {Svt, St1} = exp_expr_list({map, Line}, [Src], Vt, check_map_allowed(Line, St)),
    {Fvt, St2} = map_fields(
        Es,
        Vt,
        St1,
        fun(Es0, Vt0, St0) ->
            exp_expr_list({map, Line}, Es0, Vt0, St0)
        end
    ),
    {vtupdate(Svt, Fvt), St2};
expr({shape, Line, Fields}, Vt, St) ->
    {Vt1, St1} = check_shape_fields(Fields, Vt, St, fun expr/3),
    {vtupd_export({shape, Line}, Vt1, Vt), St1};
expr({shape_update, Line, Expr, Fields}, Vt, St0) ->
    {Evt, St1} = expr(Expr, Vt, St0),
    {Vt2, St2} = check_shape_fields(Fields, vtmerge(Vt, Evt), St1, fun expr/3),
    {vtupd_export({shape, Line}, Vt2, Vt), St2};
expr({shape_field, Line, Expr, _Field}, Vt, St0) ->
    exp_expr_list({shape, Line}, [Expr], Vt, St0);
expr({struct, Line, Name, Fields}, Vt0, St0) ->
    {Vt1, St1} = check_struct(Line, Name, St0, fun(ResolvedName, Defs, St) ->
        init_fields(Fields, Line, ResolvedName, Defs, Vt0, St)
    end),
    {vtupd_export({struct, Line}, Vt1, Vt0), St1};
expr({struct, Line, Expr, Name, Fields}, Vt, St0) ->
    St1 = warn_invalid_struct(Line, Expr, St0),
    {Evt, St2} = expr(Expr, Vt, St1),
    {Uvt, St3} =
        check_struct(Line, Name, St2, fun(ResolvedName, Defs, St) ->
            update_fields(Fields, Line, ResolvedName, Defs, Vt, St)
        end),
    Vt1 = vtmerge(Evt, Uvt),
    {vtupd_export({struct, Line}, Vt1, Vt), St3};
expr({struct_field, Line, Expr, Name, Field}, Vt, St0) ->
    St1 = warn_invalid_struct(Line, Expr, St0),
    {Evt, St2} = expr(Expr, Vt, St1),
    {Fvt, St3} =
        check_struct(Line, Name, St2, fun(ResolvedName, Defs, St) ->
            field(Field, ResolvedName, Defs, St)
        end),
    Vt1 = vtmerge(Evt, Fvt),
    {vtupd_export({struct, Line}, Vt1, Vt), St3};
expr({struct_index, Line, Name, Field}, Vt, St0) ->
    {Vt1, St1} = check_struct(Line, Name, St0, fun(ResolvedName, Defs, St) ->
        field(Field, ResolvedName, Defs, St)
    end),
    {vtupd_export({struct, Line}, Vt1, Vt), St1};
expr({bin, Line, Fs}, Vt, St) ->
    {Vt1, St1} = expr_bin(Fs, Vt, St, fun expr/3),
    {vtupd_export({binary, Line}, Vt1, Vt), St1};
expr({block, Line, Es}, Vt, St) ->
    %% Unfold block into a sequence.
    {Vt1, St1} = exprs(Es, Vt, St),
    {vtupd_export({block, Line}, Vt1, Vt), St1};
expr({'if', Line, Cs}, Vt, St) ->
    icrt_clauses(Cs, {'if', Line}, Vt, St);
expr({'case', Line, E, Cs}, Vt, St0) ->
    {Evt, St1} = expr(E, Vt, St0),
    {Cvt, St2} = icrt_clauses(Cs, {'case', Line}, vtupdate(Evt, Vt), St1),
    {vtmerge(Evt, Cvt), St2};
expr({'receive', Line, Cs}, Vt, St) ->
    icrt_clauses(Cs, {'receive', Line}, Vt, St);
expr({'receive', Line, Cs, To, ToEs}, Vt, St0) ->
    %% Are variables from the timeout expression visible in the clauses? NO!
    {Tvt, St1} = expr(To, Vt, St0),
    {Tevt, St2} = exprs(ToEs, Vt, St1),
    {Cvt, St3} = icrt_clauses(Cs, Vt, St2),
    %% Csvts = [vtnew(Tevt, Vt)|Cvt],           %This is just NEW variables!
    Csvts = [Tevt | Cvt],
    Rvt = icrt_export(Csvts, Vt, {'receive', Line}, St3),
    {vtmerge([Tvt, Tevt, Rvt]), St3};
expr({'fun', Line, Body}, Vt, St) ->
    %%No one can think funs export!
    case Body of
        {clauses, Cs} ->
            fun_clauses(Cs, Vt, St);
        {function, F, A} ->
            %% BifClash - Fun expression
            %% N.B. Only allows BIFs here as well, NO IMPORTS!!
            case
                ((not is_local_function(St#lint.locals, {F, A})) andalso
                    (erl_internal:bif(F, A) andalso
                        (not is_autoimport_suppressed(St#lint.no_auto, {F, A}))))
            of
                true -> {[], St};
                false -> {[], call_function(Line, F, A, St)}
            end;
        {function, M, F, A} ->
            expr_list([M, F, A], Vt, St)
    end;
expr({named_fun, _, '_', Cs}, Vt, St) ->
    fun_clauses(Cs, Vt, St);
expr({named_fun, Line, Name, Cs}, Vt, St0) ->
    Nvt0 = [{Name, {bound, unused, [Line]}}],
    St1 = shadow_vars(Nvt0, Vt, 'named fun', St0),
    Nvt1 = vtupdate(vtsubtract(Vt, Nvt0), Nvt0),
    {Csvt, St2} = fun_clauses(Cs, Nvt1, St1),
    {_, St3} = check_unused_vars(vtupdate(Csvt, Nvt0), [], St2),
    {vtold(Csvt, Vt), St3};
expr({call, Line, {remote, _, {atom, _, erlang}, {atom, _, is_record}}, [_, _]}, _Vt, St) ->
    {[], add_error(Line, unsupported_is_record, St)};
expr({call, Line, {remote, _Lr, {atom, _Lm, M}, {atom, Lf, F}}, As}, Vt, St0) ->
    St1 = keyword_warning(Lf, F, St0),
    St2 = check_remote_function(Line, M, F, As, St1),
    St3 = check_module_name(M, Line, St2),
    exp_expr_list({call, Line}, As, Vt, St3);
expr({call, Line, {remote, _Lr, M, F}, As}, Vt, St0) ->
    St1 = keyword_warning(Line, M, St0),
    St2 = keyword_warning(Line, F, St1),
    St3 =
        case M of
            {atom, Lm, Mod} ->
                check_module_name(Mod, Lm, St2);
            _ ->
                St2
        end,
    exp_expr_list({call, Line}, [M, F | As], Vt, St3);
expr({call, Line, {atom, La, F}, As}, Vt, St0) ->
    St1 = keyword_warning(La, F, St0),
    {Asvt, St2} = exp_expr_list({call, Line}, As, Vt, St1),
    A = length(As),
    IsLocal = is_local_function(St2#lint.locals, {F, A}),
    IsAutoBif = erl_internal:bif(F, A),
    AutoSuppressed = is_autoimport_suppressed(St2#lint.no_auto, {F, A}),
    Warn =
        is_warn_enabled(bif_clash, St2) and
            (not bif_clash_specifically_disabled(St2, {F, A})),
    Imported = imported(F, A, St2),
    case
        ((not IsLocal) andalso
            (Imported =:= no) andalso
            IsAutoBif andalso (not AutoSuppressed))
    of
        true ->
            St3 = deprecated_function(Line, erlang, F, As, St2),
            {Asvt, St3};
        false ->
            {Asvt,
                case Imported of
                    {yes, M} ->
                        St3 = check_remote_function(Line, M, F, As, St2),
                        U0 = St3#lint.usage,
                        Imp = ordsets:add_element({{F, A}, M}, U0#usage.imported),
                        St3#lint{usage = U0#usage{imported = Imp}};
                    no ->
                        N = {F, A},
                        %% BifClash - function call
                        %% Issue these warnings/errors even if it's a recursive call
                        St3 =
                            if
                                (not AutoSuppressed) andalso IsAutoBif andalso Warn ->
                                    case erl_internal:old_bif(F, A) of
                                        true ->
                                            add_error(
                                                Line,
                                                {call_to_redefined_old_bif, {F, A}},
                                                St2
                                            );
                                        false ->
                                            add_warning(
                                                Line,
                                                {call_to_redefined_bif, {F, A}},
                                                St2
                                            )
                                    end;
                                true ->
                                    St2
                            end,
                        %% ...but don't lint recursive calls
                        if
                            N =:= St3#lint.func ->
                                St3;
                            true ->
                                call_function(Line, F, A, St3)
                        end
                end}
    end;
expr({call, Line, F, As}, Vt, St0) ->
    St = warn_invalid_call(Line, F, St0),
    %They see the same variables
    exp_expr_list({call, Line}, [F | As], Vt, St);
expr({'try', Line, Es, Scs, Ccs, As}, Vt, St0) ->
    %% The only exports we allow are from the try expressions to the
    %% success clauses.
    {Evt0, St1} = exprs(Es, Vt, St0),
    TryLine = {'try', Line},
    Uvt = vtunsafe(TryLine, Evt0, Vt),
    {Sccs, St2} = try_clauses(
        Scs,
        Ccs,
        TryLine,
        vtupdate(Evt0, Vt),
        Uvt,
        St1
    ),
    Evt1 = vtupdate(Uvt, Evt0),
    Rvt0 = Sccs,
    Rvt1 = vtupd_unsafe(TryLine, Rvt0, Vt),
    Evt2 = vtmerge(Evt1, Rvt1),
    {Avt0, St} = exprs(As, vtupdate(Evt2, Vt), St2),
    Avt1 = vtupd_unsafe(TryLine, Avt0, Vt),
    Avt = vtmerge(Evt2, Avt1),
    {Avt, St};
expr({'catch', Line, E}, Vt, St0) ->
    %% No new variables added, flag new variables as unsafe.
    {Evt, St} = expr(E, Vt, St0),
    {vtupd_unsafe({'catch', Line}, Evt, Vt), St};
expr({match, _Line, P, E}, Vt, St0) ->
    {Evt, St1} = expr(E, Vt, St0),
    {Pvt, Pnew, St2} = pattern(P, vtupdate(Evt, Vt), St1),
    St3 = unpinned_vars(Pnew, Vt, St2),
    St = reject_invalid_alias_expr(P, E, Vt, St3),
    {vtupdate(Pnew, vtmerge(Evt, Pvt)), St};
%% No comparison or boolean operators yet.
expr({op, Line, '^', A}, Vt, St) ->
    {Vt1, St1} = exp_expr_list({'^', Line}, [A], Vt, St),
    {Vt1, add_error(Line, illegal_caret, St1)};
expr({op, Line, Op, A}, Vt, St) ->
    exp_expr_list({Op, Line}, [A], Vt, St);
expr({op, Line, Op, L, R}, Vt, St0) when Op =:= 'orelse'; Op =:= 'andalso' ->
    {Evt1, St1} = exp_expr_list({Op, Line}, [L], Vt, St0),
    Vt1 = vtupdate(Evt1, Vt),
    {Evt2, St2} = expr(R, Vt1, St1),
    Evt3 = vtupd_unsafe({Op, Line}, Evt2, Vt1),
    {vtmerge(Evt1, Evt3), St2};
expr({op, Line, Op, L, R}, Vt, St) ->
    exp_expr_list({Op, Line}, [L, R], Vt, St);
%They see the same variables
%% The following are not allowed to occur anywhere!
expr({remote, Line, _M, _F}, _Vt, St) ->
    {[], add_error(Line, illegal_expr, St)}.

%% mark new vars in expr_list as exported

exp_expr_list(Where, Es, Vt, St) ->
    {Evt, St1} = expr_list(Es, Vt, St),
    Evt1 = vtupd_export(Where, Evt, Vt),
    {Evt1, St1}.

%% expr_list(Expressions, Variables, State) ->
%%      {UsedVarTable,State}

expr_list(Es, Vt, St) ->
    foldl(
        fun(E, {Esvt, St0}) ->
            {Evt, St1} = expr(E, Vt, St0),
            {vtmerge_pat(Evt, Esvt), St1}
        end,
        {[], St},
        Es
    ).

check_assoc_fields([{map_field_exact, Line, _, _} | Fs], St) ->
    check_assoc_fields(Fs, add_error(Line, illegal_map_construction, St));
check_assoc_fields([{map_field_assoc, _, _, _} | Fs], St) ->
    check_assoc_fields(Fs, St);
check_assoc_fields([], St) ->
    St.

map_fields([{Tag, _, K, V} | Fs], Vt, St, F) when
    Tag =:= map_field_assoc; Tag =:= map_field_exact
->
    {Pvt, St2} = F([K, V], Vt, St),
    {Vts, St3} = map_fields(Fs, Vt, St2, F),
    {vtupdate(Pvt, Vts), St3};
map_fields([], _, St, _) ->
    {[], St}.

%% warn_invalid_struct(Line, Struct, State0) -> State
%% Adds warning if the struct is invalid.

warn_invalid_struct(Line, Struct, St) ->
    case is_valid_struct(Struct) of
        true -> St;
        false -> add_warning(Line, invalid_struct, St)
    end.

%% is_valid_struct(Struct) -> boolean().

is_valid_struct(Struct) ->
    case Struct of
        {char, _, _} -> false;
        {integer, _, _} -> false;
        {float, _, _} -> false;
        {atom, _, _} -> false;
        {string, _, _} -> false;
        {cons, _, _, _} -> false;
        {nil, _} -> false;
        {lc, _, _, _} -> false;
        {struct_index, _, _, _} -> false;
        {'fun', _, _} -> false;
        {named_fun, _, _, _} -> false;
        _ -> true
    end.

%% warn_invalid_call(Line, Call, State0) -> State
%% Adds warning if the call is invalid.

warn_invalid_call(Line, F, St) ->
    case is_valid_call(F) of
        true -> St;
        false -> add_warning(Line, invalid_call, St)
    end.

%% is_valid_call(Call) -> boolean().

is_valid_call(Call) ->
    case Call of
        {char, _, _} -> false;
        {integer, _, _} -> false;
        {float, _, _} -> false;
        {string, _, _} -> false;
        {cons, _, _, _} -> false;
        {nil, _} -> false;
        {lc, _, _, _} -> false;
        {tuple, _, Exprs} when length(Exprs) =/= 2 -> false;
        {enum, _, _, _, _} -> false;
        _ -> true
    end.

check_enum(Line, undefined, _, St, _CheckFun) ->
    {[], add_error(Line, unqualified_enum, St)};
check_enum(Line, RawName, {atom, _, V}, St, CheckFun) ->
    case resolve_importable_name(RawName, St) of
        {local, N, St1} ->
            case maps:find(N, St1#lint.enums) of
                {ok, {_Loc, Arity, #{V := Defs}}} ->
                    CheckFun({enum, N, V}, Defs, used_type({N, Arity}, Line, St1));
                {ok, {_Loc, _Arity, _}} ->
                    {[], add_error(Line, {undefined_enum_variant, N, V}, St1)};
                error ->
                    {[], add_error(Line, {undefined_enum, N}, St1)}
            end;
        {remote, M, N, St1} ->
            case St1#lint.defs_db of
                undefined ->
                    CheckFun({enum, {M, N}, V}, unavailable, St1);
                GlobalDefs ->
                    case erlt_defs:find_enum(M, N, GlobalDefs) of
                        {ok, EnumDef} ->
                            case remote_enum_variants_map(EnumDef) of
                                #{V := Defs} ->
                                    CheckFun({enum, {M, N}, V}, Defs, St1);
                                _ ->
                                    {[], add_error(Line, {undefined_enum_variant, {M, N}, V}, St1)}
                            end;
                        private ->
                            {[], add_error(Line, {private_enum, {M, N}}, St1)};
                        error ->
                            {[], add_error(Line, {undefined_enum, {M, N}}, St1)}
                    end
            end;
        {invalid, St1} ->
            {[], St1}
    end.

check_struct(Line, RawName, St, CheckFun) ->
    case resolve_importable_name(RawName, St) of
        {local, N, St1} ->
            case maps:find(N, St1#lint.structs) of
                {ok, {_Loc, Arity, Defs}} ->
                    CheckFun({struct, N}, Defs, used_type({N, Arity}, Line, St1));
                error ->
                    {[], add_error(Line, {undefined_struct, N}, St1)}
            end;
        {remote, M, N, St1} ->
            case St1#lint.defs_db of
                undefined ->
                    CheckFun({struct, {M, N}}, unavailable, St1);
                GlobalDefs ->
                    case erlt_defs:find_struct(M, N, GlobalDefs) of
                        {ok, StructDef} ->
                            Defs = remote_struct_field_map(StructDef),
                            CheckFun({struct, {M, N}}, Defs, St1);
                        private ->
                            {[], add_error(Line, {private_struct, {M, N}}, St1)};
                        error ->
                            {[], add_error(Line, {undefined_struct, {M, N}}, St1)}
                    end
            end
    end.

resolve_importable_name({atom, _, Name}, St) ->
    case imported_type(Name, St) of
        {yes, Mod, Arity} ->
            Usage = ordsets:add_element({{Name, Arity}, Mod}, St#lint.usage#usage.imported_types),
            St1 = St#lint{usage = (St#lint.usage)#usage{imported_types = Usage}},
            {remote, Mod, Name, St1};
        no ->
            {local, Name, St}
    end;
resolve_importable_name({remote, _, {atom, _, Mod}, {atom, _, Name}}, St) ->
    {remote, Mod, Name, St};
resolve_importable_name(Other, St) ->
    {invalid, add_error(element(2, Other), invalid_name, St)}.

remote_struct_field_map({attribute, _, struct, {_, {type, _, struct, _, Fields}, _}}) ->
    field_defs(Fields).

remote_enum_variants_map({attribute, _, enum, {_, {type, _, enum, _, Variants}, _}}) ->
    enum_variants_map(Variants).

field(_Field, _Name, unavailable, St) ->
    {[], St};
field({atom, Line, Field}, Name, {_, Defs}, St) ->
    case is_map_key(Field, Defs) of
        true -> {[], St};
        false -> {[], add_error(Line, {undefined_field, Name, Field}, St)}
    end;
field({integer, Line, Field}, Name, {Pos, _}, St) ->
    case Field < Pos of
        true -> {[], St};
        false -> {[], add_error(Line, {undefined_field, Name, Field}, St)}
    end.

init_fields(Fields, Line, Name, Defs, Vt0, St0) ->
    {SeenFields, Uvt, St} = check_fields(Line, Fields, Name, Defs, Vt0, St0, fun expr/3),
    {Uvt, ensure_all_labelled_fields(Line, Name, SeenFields, Defs, fun expr/3, Uvt, St)}.

init_fields_guard(Fields, Line, Name, Definitions, Vt0, St0) ->
    {SeenFields, Uvt, St} = check_fields(Line, Fields, Name, Definitions, Vt0, St0, fun gexpr/3),
    {Uvt, ensure_all_labelled_fields(Line, Name, SeenFields, Definitions, fun gexpr/3, Uvt, St)}.

update_fields(Fields, Line, Name, Defs0, Vt0, St0) ->
    %% Updates don't support specifying positional fields
    Defs =
        case Defs0 of
            unavailable -> unavailable;
            {_Positional, Labelled} -> {0, Labelled}
        end,
    {_SeenFields, Uvt, St} = check_fields(Line, Fields, Name, Defs, Vt0, St0, fun expr/3),
    {Uvt, St}.

ensure_all_labelled_fields(_Line, _Name, _SeenFields, unavailable, _Expand, _Vt, St) ->
    St;
ensure_all_labelled_fields(_Line, _Name, _SeenFields, none, _Expand, _Vt, St) ->
    St;
ensure_all_labelled_fields(Line, Name, SeenFields, {_, Definitions}, Expand, Vt, St0) ->
    Fun = fun(Field, Default0, St) ->
        case is_map_key(Field, SeenFields) of
            true ->
                St;
            false when Default0 =/= undefined ->
                Default = copy_expr(Default0, Line),
                verify_no_recursive_defaults(Line, Default, Name, Field, Expand, Vt, St);
            false when Default0 =:= undefined ->
                add_error(Line, {no_field_value, Name, Field}, St)
        end
    end,
    maps:fold(Fun, St0, Definitions).

verify_no_recursive_defaults(_, _, _, _, _, _, #lint{default_expansion = skip} = St) ->
    St;
verify_no_recursive_defaults(Line, Expr, Name, Field, Expand, Vt, St) ->
    ExpansionState = St#lint.default_expansion,
    case sets:is_element(Name, ExpansionState) of
        true ->
            add_error(Line, {recursive_field, Name, Field}, St);
        false ->
            St1 = St#lint{default_expansion = sets:add_element(Name, ExpansionState)},
            {_, St2} = Expand(Expr, Vt, St1),
            St2#lint{default_expansion = ExpansionState}
    end.

check_shape_fields(Fields, Vt, St, CheckFun) ->
    check_shape_fields(Fields, Vt, St, CheckFun, []).

check_shape_fields(
    [{field, Lf, {atom, _La, F}, Val} | Fields],
    Vt,
    St,
    CheckFun,
    UsedFields
) ->
    case member(F, UsedFields) of
        true ->
            {[], add_error(Lf, {reuse_shape_field, F}, St)};
        false ->
            {Vt1, St1} = CheckFun(Val, Vt, St),
            check_shape_fields(Fields, vtmerge(Vt, Vt1), St1, CheckFun, [F | UsedFields])
    end;
check_shape_fields([], Vt, St, _CheckFun, _) ->
    {Vt, St}.

check_shape_pattern_fields(
    [{field, Lf, {atom, _La, F}, Val} | Fields],
    Vt,
    Old,
    St,
    UsedFields
) ->
    case member(F, UsedFields) of
        true ->
            {[], [], add_error(Lf, {reuse_shape_field, F}, St)};
        false ->
            {Vt1, New, St1} = pattern(Val, Vt, Old, St),
            check_shape_pattern_fields(
                Fields,
                vtmerge_pat(Vt, Vt1),
                vtmerge_pat(Old, New),
                St1,
                [F | UsedFields]
            )
    end;
check_shape_pattern_fields([], Vt, Old, St, _) ->
    {Vt, Old, St}.

check_fields(OuterLine, Fields, Name, Defs, Vt0, St0, CheckFun) ->
    Check = fun(Value, Vt, St) ->
        {Vt1, St1} = CheckFun(Value, Vt0, St),
        {vtmerge_pat(Vt, Vt1), St1}
    end,
    check_fields_common(Check, [], St0, Fields, Name, OuterLine, Defs).

pattern_fields(Fields, OuterLine, Name, Defs, Vt0, Old, St0) ->
    Check = fun(Value, {Vt, New}, St) ->
        {Vt1, New1, St1} = pattern(Value, Vt0, Old, St),
        {{vtmerge_pat(Vt, Vt1), vtmerge_pat(New, New1)}, St1}
    end,
    {_Seen, {Uvt, Unew}, St1} =
        check_fields_common(Check, {[], []}, St0, Fields, Name, OuterLine, Defs),
    {Uvt, Unew, St1}.

check_fields_common(_Fun, Acc, St, none, _Name, _OuterLine, none) ->
    {#{}, Acc, St};
check_fields_common(_Fun, Acc, St, none, _Name, _OuterLine, unavailable) ->
    {#{}, Acc, St};
check_fields_common(_Fun, Acc, St, _Fields, Name, OuterLine, none) ->
    {#{}, Acc, add_error(OuterLine, {no_fields_expected, Name}, St)};
check_fields_common(_Fun, Acc, St, none, Name, OuterLine, {_, _}) ->
    {#{}, Acc, add_error(OuterLine, {no_fields_given, Name}, St)};
check_fields_common(Fun, Acc, St, Fields, Name, OuterLine, unavailable) ->
    check_fields(Fun, Acc, St, Fields, 0, Name, OuterLine, unavailable, unavailable);
check_fields_common(Fun, Acc, St, Fields, Name, OuterLine, {Positional, Defs}) ->
    check_fields(Fun, Acc, St, Fields, 0, Name, OuterLine, Positional, Defs).

check_fields(Fun, Acc, St0, [FieldNode | Rest], Seen0, Name, OuterLine, Pos, Defs) ->
    {Seen1, Acc1, St1} =
        case FieldNode of
            {field, Line, {atom, _, Field}, Value} ->
                {Seen, St} = ensure_all_positional_fields(OuterLine, Name, Seen0, Pos, St0),
                check_labelled(Fun, Acc, St, Line, Field, Value, Seen, Name, Defs);
            {field, Line, positional, Value} ->
                check_positional(Fun, Acc, St0, Line, Value, Seen0, Name, Pos)
        end,
    check_fields(Fun, Acc1, St1, Rest, Seen1, Name, OuterLine, Pos, Defs);
check_fields(_Fun, Acc, St0, [], Seen0, Name, OuterLine, Pos, _Defs) ->
    {Seen, St} = ensure_all_positional_fields(OuterLine, Name, Seen0, Pos, St0),
    {Seen, Acc, St}.

check_labelled(Fun, Acc0, St0, Line, Field, Value, Seen, Name, Defs) ->
    {Acc, St} = Fun(Value, Acc0, St0),
    case is_map_key(Field, Seen) of
        true ->
            St1 = add_error(Line, {redefine_field, Name, Field}, St),
            {Seen, Acc, St1};
        false when Defs =:= unavailable; is_map_key(Field, Defs) ->
            {Seen#{Field => []}, Acc, St};
        false ->
            St1 = add_error(Line, {undefined_field, Name, Field}, St),
            {Seen#{Field => []}, Acc, St1}
    end.

check_positional(Fun, Acc0, St0, Line, Value, Seen, Name, Pos) ->
    {Acc, St} = Fun(Value, Acc0, St0),
    case is_map(Seen) of
        true ->
            St1 = add_error(Line, positional_after_labelled_field, St),
            {Seen, Acc, St1};
        false when Pos =:= unavailable; Seen < Pos ->
            {Seen + 1, Acc, St};
        false ->
            St1 = add_error(Line, {extra_positional_field, Seen + 1, Pos, Name}, St),
            {Seen + 1, Acc, St1}
    end.

ensure_all_positional_fields(Line, Name, Seen, Pos, St) ->
    case is_integer(Seen) of
        true when Pos =:= unavailable; Seen >= Pos -> {#{}, St};
        true -> {#{}, add_error(Line, {missing_positional_field, Seen, Pos, Name}, St)};
        false -> {Seen, St}
    end.

%% Does not do any checking, this is done in check_type
struct_def(Loc, {type, _, struct, {atom, _, Name}, Fields}, TypeArity, St) ->
    St#lint{structs = (St#lint.structs)#{Name => {Loc, TypeArity, field_defs(Fields)}}}.

field_defs(none) -> none;
field_defs(Fields) -> field_defs(Fields, [], 0).

field_defs([{field_definition, _, positional, undefined, _Type} | Rest], Acc, Num) ->
    field_defs(Rest, Acc, Num + 1);
field_defs([{field_definition, _, {atom, _, Field}, Default, _Type} | Rest], Acc, Num) ->
    field_defs(Rest, [{Field, Default} | Acc], Num);
field_defs([], Acc, Num) ->
    {Num, maps:from_list(Acc)}.

%% Does not do any checking, this is done in check type
enum_def(Loc, {type, _, enum, {atom, _, Name}, Variants}, TypeArity, St) ->
    St#lint{enums = (St#lint.enums)#{Name => {Loc, TypeArity, enum_variants_map(Variants)}}}.

enum_variants_map(Variants) ->
    maps:from_list([
        {Name, field_defs(Fields)}
     || {variant, _, {atom, _, Name}, Fields} <- Variants
    ]).

%% type_def(Attr, Line, TypeName, PatField, Args, State) -> State.
%%    Attr :: 'type' | 'opaque' | 'enum' | 'struct'
%% Checks that a type definition is valid.

-dialyzer({no_match, type_def/6}).

type_def(unchecked_opaque, Line, TypeName, ProtoType, Args, St0) ->
    TypeDefs = St0#lint.types,
    Arity = length(Args),
    TypePair = {TypeName, Arity},
    Info = #typeinfo{attr = unchecked_opaque, line = Line},
    NewDefs = maps:put(TypePair, Info, TypeDefs),
    St1 = verify_no_underscore(Args, verify_typevars(Args, St0)),
    case is_default_type(TypePair) of
        true ->
            add_error(Line, {builtin_type, TypePair}, St1);
        false ->
            St2 = check_redefined_type(Line, TypeName, Arity, St1),
            St3 =
                case is_underspecified(ProtoType, 0) of
                    false ->
                        add_error(Line, opaque_unchecked_type_error, St2);
                    true ->
                        St2
                end,
            St3#lint{types = NewDefs}
    end;
type_def(Attr, Line, TypeName, ProtoType, Args, St0) ->
    TypeDefs = St0#lint.types,
    Arity = length(Args),
    TypePair = {TypeName, Arity},
    Info = #typeinfo{attr = Attr, line = Line},
    StoreType = fun(St) ->
        NewDefs = maps:put(TypePair, Info, TypeDefs),
        CheckType = {type, nowarn(), product, [ProtoType | Args]},
        check_type_in_def(CheckType, St#lint{types = NewDefs})
    end,
    St1 = verify_typevars(Args, St0),
    case is_default_type(TypePair) of
        true ->
            case is_obsolete_builtin_type(TypePair) of
                true ->
                    StoreType(St1);
                false ->
                    case is_newly_introduced_builtin_type(TypePair) of
                        %% allow some types just for bootstrapping
                        true ->
                            Warn = {new_builtin_type, TypePair},
                            St2 = add_warning(Line, Warn, St1),
                            StoreType(St2);
                        false ->
                            add_error(Line, {builtin_type, TypePair}, St1)
                    end
            end;
        false ->
            St2 =
                case Attr =:= opaque andalso is_underspecified(ProtoType, Arity) of
                    true ->
                        Warn = {underspecified_opaque, TypePair},
                        add_warning(Line, Warn, St1);
                    false ->
                        St1
                end,
            StoreType(check_redefined_type(Line, TypeName, Arity, St2))
    end.

%% erlt modification: don't allow types overloaded on arity
check_redefined_type(Line, Name, Arity, #lint{types = Types, imp_types = Imports} = St) ->
    case lists:any(fun({N, _}) -> N =:= Name end, maps:keys(Types)) of
        true ->
            add_error(Line, {redefine_type, {Name, Arity}}, St);
        false ->
            case lists:search(fun({{N, _}, _}) -> N =:= Name end, Imports) of
                {value, Redefine} ->
                    add_error(Line, {redefine_import_type, Redefine}, St);
                false ->
                    St
            end
    end.

verify_typevars(Vars, St0) ->
    Fun = fun({var, Line, Name}, {St, Names}) ->
        case sets:is_element(Name, Names) of
            true -> {add_error(Line, {reused_typevar, Name}, St), Names};
            false -> {St, sets:add_element(Name, Names)}
        end
    end,
    element(1, lists:foldl(Fun, {St0, sets:new()}, Vars)).

verify_no_underscore([{var, Anno, '_'} | _Rest], St) ->
    add_error(Anno, underscore_type, St);
verify_no_underscore([_Var | Rest], St) ->
    verify_no_underscore(Rest, St);
verify_no_underscore([], St) ->
    St.

is_underspecified({type, _, term, []}, 0) -> true;
is_underspecified({type, _, any, []}, 0) -> true;
is_underspecified(_ProtType, _Arity) -> false.

check_type_in_spec(Type, St) ->
    do_check_type(Type, [{'_', predefined}], St).

check_type_in_def(Type, St) ->
    do_check_type(Type, [], St).

do_check_type(Types, Vars, St) ->
    {SeenVars, St1} = check_type(Types, maps:from_list(Vars), St),
    maps:fold(
        fun
            (Var, {seen_once, Line}, AccSt) ->
                case atom_to_list(Var) of
                    "_" ++ _ -> AccSt;
                    _ -> add_error(Line, {singleton_typevar, Var}, AccSt)
                end;
            ('_', predefined, AccSt) ->
                AccSt;
            (_Var, seen_multiple, AccSt) ->
                AccSt
        end,
        St1,
        SeenVars
    ).

check_type({ann_type, _L, [_Var, Type]}, SeenVars, St) ->
    check_type(Type, SeenVars, St);
check_type({remote_type, L, [{atom, _, Mod}, {atom, _, Name}, Args]}, SeenVars, St0) ->
    St1 = check_module_name(Mod, L, St0),
    St2 = deprecated_type(L, Mod, Name, Args, St1),
    St = check_remote_type(L, Mod, Name, length(Args), St2),
    lists:foldl(
        fun(T, {AccSeenVars, AccSt}) -> check_type(T, AccSeenVars, AccSt) end,
        {SeenVars, St},
        Args
    );
check_type({integer, _L, _}, SeenVars, St) ->
    {SeenVars, St};
check_type({atom, _L, _}, SeenVars, St) ->
    {SeenVars, St};
check_type({var, L, '_'}, SeenVars, St) ->
    case maps:find('_', SeenVars) of
        {ok, predefined} -> {SeenVars, St};
        error -> {SeenVars, add_error(L, underscore_type, St)}
    end;
check_type({var, L, Name}, SeenVars, St) ->
    NewSeenVars =
        case maps:find(Name, SeenVars) of
            {ok, {seen_once, _}} -> maps:put(Name, seen_multiple, SeenVars);
            {ok, seen_multiple} -> SeenVars;
            error -> maps:put(Name, {seen_once, L}, SeenVars)
        end,
    {NewSeenVars, St};
check_type({type, L, bool, []}, SeenVars, St) ->
    {SeenVars, add_warning(L, {renamed_type, bool, boolean}, St)};
check_type({type, L, 'fun', [Dom, Range]}, SeenVars, St) ->
    St1 =
        case Dom of
            {type, _, product, _} -> St;
            {type, _, any} -> St;
            _ -> add_error(L, {type_syntax, 'fun'}, St)
        end,
    check_type({type, nowarn(), product, [Dom, Range]}, SeenVars, St1);
check_type({type, L, range, [From, To]}, SeenVars, St) ->
    St1 =
        case {erl_eval:partial_eval(From), erl_eval:partial_eval(To)} of
            {{integer, _, X}, {integer, _, Y}} when X < Y -> St;
            _ -> add_error(L, {type_syntax, range}, St)
        end,
    {SeenVars, St1};
check_type({type, _L, map, any}, SeenVars, St) ->
    {SeenVars, St};
check_type({type, _L, map, Pairs}, SeenVars, St) ->
    lists:foldl(
        fun(Pair, {AccSeenVars, AccSt}) ->
            check_type(Pair, AccSeenVars, AccSt)
        end,
        {SeenVars, St},
        Pairs
    );
check_type({type, _L, map_field_assoc, [Dom, Range]}, SeenVars, St) ->
    check_type({type, nowarn(), product, [Dom, Range]}, SeenVars, St);
check_type({type, _L, tuple, any}, SeenVars, St) ->
    {SeenVars, St};
check_type({type, _L, any}, SeenVars, St) ->
    {SeenVars, St};
check_type({type, L, binary, [Base, Unit]}, SeenVars, St) ->
    St1 =
        case {erl_eval:partial_eval(Base), erl_eval:partial_eval(Unit)} of
            {{integer, _, BaseVal}, {integer, _, UnitVal}} when BaseVal >= 0, UnitVal >= 0 ->
                St;
            _ ->
                add_error(L, {type_syntax, binary}, St)
        end,
    {SeenVars, St1};
check_type({type, La, enum, {atom, _, Name}, Variants}, SeenVars, St) ->
    check_enum_types(La, Name, Variants, SeenVars, St);
check_type({type, _L, open_shape, Fields, Var}, SeenVars, St) ->
    {SeenVars1, St1} = check_type(Var, SeenVars, St),
    check_shape_types(Fields, SeenVars1, St1);
check_type({type, _L, closed_shape, Fields}, SeenVars, St) ->
    check_shape_types(Fields, SeenVars, St);
check_type({type, La, struct, {atom, _, Tag}, Fields}, SeenVars, St) ->
    check_struct_types(La, Tag, Fields, SeenVars, St);
check_type({type, _L, Tag, Args}, SeenVars, St) when
    Tag =:= product; Tag =:= union; Tag =:= tuple
->
    lists:foldl(
        fun(T, {AccSeenVars, AccSt}) ->
            check_type(T, AccSeenVars, AccSt)
        end,
        {SeenVars, St},
        Args
    );
check_type({type, La, TypeName, Args}, SeenVars, St) ->
    #lint{module = Module, types = Types} = St,
    Arity = length(Args),
    TypePair = {TypeName, Arity},
    Obsolete =
        (is_warn_enabled(deprecated_type, St) andalso
            obsolete_builtin_type(TypePair)),
    St1 =
        case Obsolete of
            {deprecated, Repl, _} when element(1, Repl) =/= Module ->
                case maps:find(TypePair, Types) of
                    {ok, _} ->
                        used_type(TypePair, La, St);
                    error ->
                        {deprecated, Replacement, Rel} = Obsolete,
                        Tag = deprecated_builtin_type,
                        W = {Tag, TypePair, Replacement, Rel},
                        add_warning(La, W, St)
                end;
            _ ->
                St
        end,
    check_type({type, nowarn(), product, Args}, SeenVars, St1);
check_type({user_type, L, TypeName, Args}, SeenVars, St0) ->
    Arity = length(Args),
    TypePair = {TypeName, Arity},
    St2 =
        case imported_type(TypeName, St0) of
            {yes, M, Arity} ->
                U0 = St0#lint.usage,
                Imp = ordsets:add_element({TypePair, M}, U0#usage.imported_types),
                St1 = check_remote_type(L, M, TypeName, Arity, St0),
                St1#lint{usage = U0#usage{imported_types = Imp}};
            _ ->
                used_type(TypePair, L, St0)
        end,
    lists:foldl(
        fun(T, {AccSeenVars, AccSt}) -> check_type(T, AccSeenVars, AccSt) end,
        {SeenVars, St2},
        Args
    );
check_type([{typed_record_field, Field, _T} | _], SeenVars, St) ->
    {SeenVars, add_error(element(2, Field), old_abstract_code, St)};
check_type(I, SeenVars, St) ->
    case erl_eval:partial_eval(I) of
        {integer, _ILn, _Integer} -> {SeenVars, St};
        _Other -> {SeenVars, add_error(element(2, I), {type_syntax, integer}, St)}
    end.

check_remote_type(_, _, _, _, #lint{defs_db = undefined} = St) ->
    St;
check_remote_type(Line, M, N, A, #lint{defs_db = Defs} = St) ->
    case erlt_defs:find_type(M, N, Defs) of
        {ok, A} -> St;
        {ok, Expected} -> add_error(Line, {remote_type_wrong_arity, M, N, A, Expected}, St);
        private -> add_error(Line, {private_remote_type, M, N, A}, St);
        error -> add_error(Line, {undefined_remote_type, M, N, A}, St)
    end.

check_shape_types(Fields, SeenVars, St) ->
    check_shape_types(Fields, SeenVars, St, #{}).

check_shape_types(
    [{field_definition, Line, {atom, _, Name}, undefined, Type} | Rest],
    SeenVars,
    St,
    FieldsAcc
) ->
    case is_map_key(Name, FieldsAcc) of
        true ->
            {SeenVars1, St1} = check_type(Type, SeenVars, St),
            {SeenVars1, add_error(Line, {redefine_shape_field, Name}, St1)};
        false ->
            {SeenVars1, St1} = check_type(Type, SeenVars, St),
            check_shape_types(Rest, SeenVars1, St1, FieldsAcc#{Name => Type})
    end;
check_shape_types([], SeenVars, St, _Fields) ->
    {SeenVars, St}.

%% Enum types only appear in enum definitions
check_enum_types(_EnumLine, EnumName, Variants, SeenVars, St) ->
    check_variants(Variants, EnumName, _SeenVariants = #{}, SeenVars, St).

check_variants([{variant, Line, {atom, _, Name}, Fields} | Rest], Enum, Seen, SeenVars, St) ->
    case is_map_key(Name, Seen) of
        true ->
            {SeenVars, add_error(Line, {redefine_enum, Enum, Name}, St)};
        false ->
            {SeenVars1, St1} = check_field_defs(Fields, {enum, Enum, Name}, SeenVars, St),
            check_variants(Rest, Enum, Seen#{Name => []}, SeenVars1, St1)
    end;
check_variants([], _Enum, _Seen, SeenVars, St) ->
    {SeenVars, St}.

%% Struct types only appear in struct definitions
check_struct_types(_StructLine, StructName, Fields, SeenVars, St) ->
    check_field_defs(Fields, {struct, StructName}, SeenVars, St).

check_field_defs(none, _Parent_name, SeenVars, St) ->
    {SeenVars, St};
check_field_defs(Fields, ParentName, SeenVars0, St0) ->
    check_field_defs(SeenVars0, St0, 0, Fields, ParentName).

check_field_defs(SeenVars, St0, SeenFields0, [Field | Rest], ParentName) ->
    {field_definition, Line, FieldName, Default, Type} = Field,
    St1 = check_field_default(Default, ParentName, St0),
    {SeenVars1, St2} = check_type(Type, SeenVars, St1),
    {SeenFields, St3} =
        case FieldName of
            {atom, _, Name} when is_integer(SeenFields0) ->
                {#{Name => Type}, St2};
            {atom, _, Name} when not is_map_key(Name, SeenFields0) ->
                {SeenFields0#{Name => Type}, St2};
            {atom, _, Name} ->
                {SeenFields0, add_error(Line, {redefine_field, ParentName, Name}, St2)};
            positional when is_integer(SeenFields0) ->
                {SeenFields0 + 1, St2};
            positional ->
                {SeenFields0, add_error(Line, positional_after_labelled_field, St2)}
        end,
    check_field_defs(SeenVars1, St3, SeenFields, Rest, ParentName);
check_field_defs(SeenVars, St, _SeenFields, [], _ParentName) ->
    {SeenVars, St}.

check_field_default(undefined, _StructName, St) ->
    St;
check_field_default(Expr, Name, St) ->
    St1 = St#lint{gexpr_context = field_default, default_expansion = sets:from_list([Name])},
    {_, St2} = gexpr(Expr, [], St1),
    St2#lint{gexpr_context = gexpr, default_expansion = skip}.

used_type(TypePair, L, #lint{usage = Usage, file = File} = St) ->
    OldUsed = Usage#usage.used_types,
    UsedTypes = maps:put(TypePair, erl_anno:set_file(File, L), OldUsed),
    St#lint{usage = Usage#usage{used_types = UsedTypes}}.

is_default_type({exception, 0}) ->
    true;
is_default_type({message, 0}) ->
    true;
is_default_type({Name, NumberOfTypeVariables}) ->
    erl_internal:is_type(Name, NumberOfTypeVariables).

is_newly_introduced_builtin_type({Name, _}) when is_atom(Name) -> false.

is_obsolete_builtin_type(TypePair) ->
    obsolete_builtin_type(TypePair) =/= no.

%% To keep Dialyzer silent...
obsolete_builtin_type({1, 255}) ->
    {deprecated, {2, 255}, ""};
obsolete_builtin_type({Name, A}) when is_atom(Name), is_integer(A) ->
    no.

%% spec_decl(Line, Fun, Types, State) -> State.

spec_decl(Line, MFA0, TypeSpecs, St00 = #lint{specs = Specs, module = Mod}) ->
    MFA =
        case MFA0 of
            {F, Arity} -> {Mod, F, Arity};
            {_M, _F, Arity} -> MFA0
        end,
    St0 = check_module_name(element(1, MFA), Line, St00),
    St1 = St0#lint{specs = maps:put(MFA, Line, Specs)},
    case is_map_key(MFA, Specs) of
        true ->
            add_error(Line, {redefine_spec, MFA0}, St1);
        false ->
            case MFA of
                {Mod, _, _} ->
                    check_specs(TypeSpecs, spec_wrong_arity, Arity, St1);
                _ ->
                    add_error(Line, {bad_module, MFA}, St1)
            end
    end.

%% callback_decl(Line, Fun, Types, State) -> State.

callback_decl(Line, MFA0, TypeSpecs, St0 = #lint{callbacks = Callbacks, module = Mod}) ->
    case MFA0 of
        {M, _F, _A} ->
            St1 = check_module_name(M, Line, St0),
            add_error(Line, {bad_callback, MFA0}, St1);
        {F, Arity} ->
            MFA = {Mod, F, Arity},
            St1 = St0#lint{callbacks = maps:put(MFA, Line, Callbacks)},
            case is_map_key(MFA, Callbacks) of
                true -> add_error(Line, {redefine_callback, MFA0}, St1);
                false -> check_specs(TypeSpecs, callback_wrong_arity, Arity, St1)
            end
    end.

%% optional_callbacks(Line, FAs, State) -> State.

optional_callbacks(Line, Term, St0) ->
    try
        true = is_fa_list(Term),
        Term
    of
        FAs ->
            optional_cbs(Line, FAs, St0)
    catch
        _:_ ->
            % ignore others
            St0
    end.

optional_cbs(_Line, [], St) ->
    St;
optional_cbs(Line, [{F, A} | FAs], St0) ->
    #lint{optional_callbacks = OptionalCbs, module = Mod} = St0,
    MFA = {Mod, F, A},
    St1 = St0#lint{optional_callbacks = maps:put(MFA, Line, OptionalCbs)},
    St2 =
        case is_map_key(MFA, OptionalCbs) of
            true ->
                add_error(Line, {redefine_optional_callback, {F, A}}, St1);
            false ->
                St1
        end,
    optional_cbs(Line, FAs, St2).

is_fa_list([E | L]) -> is_fa(E) andalso is_fa_list(L);
is_fa_list([]) -> true;
is_fa_list(_) -> false.

is_fa({FuncName, Arity}) when is_atom(FuncName), is_integer(Arity), Arity >= 0 ->
    true;
is_fa(_) ->
    false.

check_module_name(M, Line, St) ->
    case is_latin1_name(M) of
        true -> St;
        false -> add_error(Line, non_latin1_module_unsupported, St)
    end.

is_latin1_name(Name) ->
    io_lib:latin1_char_list(atom_to_list(Name)).

check_specs([FunType | Left], ETag, Arity, St0) ->
    {FunType1, CTypes} =
        case FunType of
            {type, _, bounded_fun, [FT = {type, _, 'fun', _}, Cs]} ->
                Types0 = [T || {type, _, constraint, [_, T]} <- Cs],
                {FT, lists:append(Types0)};
            {type, _, 'fun', _} = FT ->
                {FT, []}
        end,
    {type, L, 'fun', [{type, _, product, D}, _]} = FunType1,
    SpecArity = length(D),
    St1 =
        case Arity =:= SpecArity of
            true ->
                St0;
            %% Cannot happen if called from the compiler.
            false ->
                add_error(L, ETag, St0)
        end,
    St2 = check_type_in_spec({type, nowarn(), product, [FunType1 | CTypes]}, St1),
    check_specs(Left, ETag, Arity, St2);
check_specs([], _ETag, _Arity, St) ->
    St.

nowarn() ->
    A0 = erl_anno:new(0),
    A1 = erl_anno:set_generated(true, A0),
    erl_anno:set_file("", A1).

check_specs_without_function(#lint{module = Mod, defined = Funcs, specs = Specs} = St) ->
    Fun = fun
        ({M, F, A}, Line, AccSt) when M =:= Mod ->
            FA = {F, A},
            case gb_sets:is_element(FA, Funcs) of
                true -> AccSt;
                false -> add_error(Line, {spec_fun_undefined, FA}, AccSt)
            end;
        ({_M, _F, _A}, _Line, AccSt) ->
            AccSt
    end,
    maps:fold(Fun, St, Specs).

%% This generates warnings for functions without specs; if the user has
%% specified both options, we do not generate the same warnings twice.
check_functions_without_spec(Forms, St0) ->
    case is_warn_enabled(missing_spec_all, St0) of
        true ->
            add_missing_spec_warnings(Forms, St0, all);
        false ->
            case is_warn_enabled(missing_spec, St0) of
                true ->
                    add_missing_spec_warnings(Forms, St0, exported);
                false ->
                    St0
            end
    end.

add_missing_spec_warnings(Forms, St0, Type) ->
    Specs = [{F, A} || {_M, F, A} <- maps:keys(St0#lint.specs)],
    %% functions + line numbers for which we should warn
    Warns =
        case Type of
            all ->
                [
                    {FA, L}
                 || {FN, L, F, A, _} <- Forms,
                    ?IS_FUNCTION(FN),
                    not lists:member(FA = {F, A}, Specs)
                ];
            exported ->
                Exps0 = gb_sets:to_list(St0#lint.exports) -- pseudolocals(),
                Exps = Exps0 -- Specs,
                [{FA, L} || {FN, L, F, A, _} <- Forms, ?IS_FUNCTION(FN), member(FA = {F, A}, Exps)]
        end,
    foldl(
        fun({FA, L}, St) ->
            add_warning(L, {missing_spec, FA}, St)
        end,
        St0,
        Warns
    ).

check_unused_types(Forms, St) ->
    case is_warn_enabled(unused_type, St) of
        true -> check_unused_types_1(Forms, St);
        false -> St
    end.

check_unused_types_1(Forms, #lint{usage = Usage, types = Ts, exp_types = ExpTs} = St) ->
    case [File || {attribute, _L, file, {File, _Line}} <- Forms] of
        [FirstFile | _] ->
            D = Usage#usage.used_types,
            L = gb_sets:to_list(ExpTs) ++ maps:keys(D),
            UsedTypes = gb_sets:from_list(L),
            FoldFun = fun
                ({{record, _} = _Type, 0}, _, AccSt) ->
                    % Before Erlang/OTP 19.0
                    AccSt;
                (Type, #typeinfo{line = FileLine}, AccSt) ->
                    case loc(FileLine, AccSt) of
                        {FirstFile, _} ->
                            case gb_sets:is_member(Type, UsedTypes) of
                                true ->
                                    AccSt;
                                false ->
                                    Warn = {unused_type, Type},
                                    add_warning(FileLine, Warn, AccSt)
                            end;
                        _ ->
                            %% No warns about unused types in include files
                            AccSt
                    end
            end,
            maps:fold(FoldFun, St, Ts);
        [] ->
            St
    end.

check_local_opaque_types(St) ->
    #lint{types = Ts, exp_types = ExpTs} = St,
    FoldFun = fun
        (_Type, #typeinfo{attr = type}, AccSt) ->
            AccSt;
        (_Type, #typeinfo{attr = enum}, AccSt) ->
            AccSt;
        (_Type, #typeinfo{attr = Struct}, AccSt) when ?IS_STRUCT(Struct) ->
            AccSt;
        (_Type, #typeinfo{attr = unchecked_opaque}, AccSt) ->
            AccSt;
        (Type, #typeinfo{attr = opaque, line = FileLine}, AccSt) ->
            case gb_sets:is_element(Type, ExpTs) of
                true ->
                    AccSt;
                false ->
                    Warn = {not_exported_opaque, Type},
                    add_warning(FileLine, Warn, AccSt)
            end
    end,
    maps:fold(FoldFun, St, Ts).

check_dialyzer_attribute(Forms, St0) ->
    Vals = [
        {L, V}
     || {attribute, L, dialyzer, Val} <- Forms,
        V0 <- lists:flatten([Val]),
        V <-
            case V0 of
                {O, F} ->
                    [{A, B} || A <- lists:flatten([O]), B <- lists:flatten([F])];
                T ->
                    [T]
            end
    ],
    {Wellformed, Bad} =
        lists:partition(
            fun
                ({_, {Option, FA}}) when is_atom(Option) ->
                    is_fa(FA);
                ({_, Option}) when is_atom(Option) ->
                    true;
                (_) ->
                    false
            end,
            Vals
        ),
    St1 = foldl(
        fun({L, Term}, St) ->
            add_error(L, {bad_dialyzer_attribute, Term}, St)
        end,
        St0,
        Bad
    ),
    DefFunctions = (gb_sets:to_list(St0#lint.defined) -- pseudolocals()),
    Fun = fun
        ({L, {Option, FA}}, St) ->
            case is_function_dialyzer_option(Option) of
                true ->
                    case lists:member(FA, DefFunctions) of
                        true -> St;
                        false -> add_error(L, {undefined_function, FA}, St)
                    end;
                false ->
                    add_error(L, {bad_dialyzer_option, Option}, St)
            end;
        ({L, Option}, St) ->
            case is_module_dialyzer_option(Option) of
                true -> St;
                false -> add_error(L, {bad_dialyzer_option, Option}, St)
            end
    end,
    foldl(Fun, St1, Wellformed).

is_function_dialyzer_option(nowarn_function) -> true;
is_function_dialyzer_option(Option) -> is_module_dialyzer_option(Option).

is_module_dialyzer_option(Option) ->
    lists:member(Option, [
        no_return,
        no_unused,
        no_improper_lists,
        no_fun_app,
        no_match,
        no_opaque,
        no_fail_call,
        no_contracts,
        no_behaviours,
        no_undefined_callbacks,
        unmatched_returns,
        error_handling,
        race_conditions,
        no_missing_calls,
        specdiffs,
        overspecs,
        underspecs,
        unknown
    ]).

%% try_catch_clauses(Scs, Ccs, In, ImportVarTable, State) ->
%%      {UpdVt,State}.

try_clauses(Scs, Ccs, In, Vt, Uvt, St0) ->
    {Csvt0, St1} = icrt_clauses(Scs, Vt, St0),
    {Csvt1, St2} = catch_clauses(Ccs, vtupdate(Uvt, Vt), St1),
    Csvt = Csvt0 ++ Csvt1,
    UpdVt = icrt_export(Csvt, Vt, In, St2),
    {UpdVt, St2}.

%% icrt_clauses(Clauses, In, ImportVarTable, State) ->
%%      {UpdVt,State}.

icrt_clauses(Cs, In, Vt, St0) ->
    {Csvt, St1} = icrt_clauses(Cs, Vt, St0),
    UpdVt = icrt_export(Csvt, Vt, In, St1),
    {UpdVt, St1}.

%% icrt_clauses(Clauses, ImportVarTable, State) ->
%%      {NewVts,State}.

icrt_clauses(Cs, Vt, St) ->
    mapfoldl(fun(C, St0) -> icrt_clause(C, Vt, St0) end, St, Cs).

catch_clauses(Cs, Vt, St) ->
    mapfoldl(fun(C, St0) -> catch_clause(C, Vt, St0) end, St, Cs).

catch_clause({clause, Line, H, G, B}, Vt0, St0) ->
    {TaintVt, St1} = catch_head(H, Line, Vt0, St0),
    icrt_clause(H, G, B, Vt0, TaintVt, St1).

icrt_clause({clause, _Line, H, G, B}, Vt0, St0) ->
    icrt_clause(H, G, B, Vt0, [], St0).

icrt_clause(H, G, B, Vt0, TaintVt, St0) ->
    {Hvt, Hnew, St1} = head(H, Vt0, TaintVt, St0),
    Vt1 = vtupdate(Hvt, Hnew),
    St2 = unpinned_vars(Hnew, vtmerge(TaintVt, Vt0), St1),
    Vt2 = vtupdate(TaintVt, Vt1),
    {Gvt, St3} = guard(G, vtupdate(Vt2, Vt0), St2),
    Vt3 = vtupdate(Gvt, Vt1),
    {Bvt, St4} = exprs(B, vtupdate(Vt3, Vt0), St3),
    {vtupdate(Bvt, Vt3), St4}.

catch_head([_], _Line, _Vt0, #lint{type_checked = true} = St0) ->
    {[], St0};
catch_head(_, Line, _Vt0, #lint{type_checked = true} = St0) ->
    {[], add_error(Line, illegal_checked_catch, St0)};
catch_head([Kind, _Reason, Stack], _Line, Vt0, St0) ->
    St1 = validate_catch_kind(Kind, St0),
    taint_stack_var(Stack, Vt0, St1);
catch_head([Kind, _Reason], _Line, _Vt0, St0) ->
    {[], validate_catch_kind(Kind, St0)};
catch_head([_], _Line, _Vt0, St0) ->
    {[], St0}.

validate_catch_kind({var, _, _}, St) -> St;
validate_catch_kind({atom_expr, _, exit}, St) -> St;
validate_catch_kind({atom_expr, _, error}, St) -> St;
validate_catch_kind({atom_expr, _, throw}, St) -> St;
validate_catch_kind({op, _, '^', {var, _, _}}, St) -> St;
validate_catch_kind(Other, St) -> add_error(element(2, Other), illegal_catch_kind, St).

taint_stack_var({var, _, '_'}, _Vt, St) ->
    {[], St};
taint_stack_var({var, Loc, Name}, Vt, St) ->
    TaintVt = [{Name, {stacktrace, unused, [Loc]}}],
    case orddict:is_key(Name, Vt) of
        true ->
            {TaintVt, add_error(Loc, {stacktrace_bound, Name}, St)};
        false ->
            {TaintVt, St}
    end;
taint_stack_var(Other, _Vt, St) ->
    {[], add_error(element(2, Other), illegal_catch_stack, St)}.

icrt_export(Vts, Vt, {Tag, Attrs}, St) ->
    {_File, Loc} = loc(Attrs, St),
    icrt_export(lists:merge(Vts), Vt, {Tag, Loc}, length(Vts), []).

icrt_export(
    [{V, {{export, _}, _, _}} | Vs0],
    [{V, {{export, _} = S0, _, Ls}} | Vt],
    In,
    I,
    Acc
) ->
    %% V was an exported variable and has been used in an expression in at least
    %% one clause. Its state needs to be merged from all clauses to silence any
    %% exported var warning already emitted.
    {VVs, Vs} = lists:partition(fun({K, _}) -> K =:= V end, Vs0),
    S = foldl(fun({_, {S1, _, _}}, AccS) -> merge_state(AccS, S1) end, S0, VVs),
    icrt_export(Vs, Vt, In, I, [{V, {S, used, Ls}} | Acc]);
icrt_export([{V, _} | Vs0], [{V, {_, _, Ls}} | Vt], In, I, Acc) ->
    %% V was either unsafe or bound and has now been reused. It may also have
    %% been an export but as it was not matched by the previous clause, it means
    %% it has been changed to 'bound' in at least one clause because it was used
    %% in a pattern.
    Vs = lists:dropwhile(fun({K, _}) -> K =:= V end, Vs0),
    icrt_export(Vs, Vt, In, I, [{V, {bound, used, Ls}} | Acc]);
icrt_export([{V1, _} | _] = Vs, [{V2, _} | Vt], In, I, Acc) when V1 > V2 ->
    %% V2 was already in scope and has not been reused in any clause.
    icrt_export(Vs, Vt, In, I, Acc);
icrt_export([{V, _} | _] = Vs0, Vt, In, I, Acc) ->
    %% V is a new variable.
    {VVs, Vs} = lists:partition(fun({K, _}) -> K =:= V end, Vs0),
    F = fun({_, {S, U, Ls}}, {AccI, AccS0, AccLs0}) ->
        AccS =
            case {S, AccS0} of
                {{unsafe, _}, {unsafe, _}} ->
                    %% V was found unsafe in a previous clause, mark
                    %% it as unsafe for the whole parent expression.
                    {unsafe, In};
                {{unsafe, _}, _} ->
                    %% V was unsafe in a clause, keep that state and
                    %% generalize it to the whole expression if it
                    %% is found unsafe in another one.
                    S;
                _ ->
                    %% V is either bound or exported, keep original
                    %% state.
                    AccS0
            end,
        AccLs =
            case U of
                used -> AccLs0;
                unused -> merge_lines(AccLs0, Ls)
            end,
        {AccI + 1, AccS, AccLs}
    end,
    %% Initial state is exported from the current expression.
    {Count, S1, Ls} = foldl(F, {0, {export, In}, []}, VVs),
    S =
        case Count of
            I ->
                %% V was found in all clauses, keep computed state.
                S1;
            _ ->
                %% V was not bound in some clauses, mark as unsafe.
                {unsafe, In}
        end,
    U =
        case Ls of
            [] -> used;
            _ -> unused
        end,
    icrt_export(Vs, Vt, In, I, [{V, {S, U, Ls}} | Acc]);
icrt_export([], _, _, _, Acc) ->
    reverse(Acc).

handle_comprehension(E, Qs, Vt0, St0) ->
    {Vt1, Uvt, St1} = lc_quals(Qs, Vt0, St0),
    {Evt, St2} = expr(E, Vt1, St1),
    Vt2 = vtupdate(Evt, Vt1),
    %% Shadowed global variables.
    {_, St3} = check_old_unused_vars(Vt2, Uvt, St2),
    %% There may be local variables in Uvt that are not global.
    {_, St4} = check_unused_vars(Uvt, Vt0, St3),
    %% Local variables that have not been shadowed.
    {_, St} = check_unused_vars(Vt2, Vt0, St4),
    Vt3 = vtmerge(vtsubtract(Vt2, Uvt), Uvt),
    %% Don't export local variables.
    Vt4 = vtold(Vt3, Vt0),
    %% Forget about old variables which were not used as well as unsafe
    %% variables, preventing them from being marked as used and bound by
    %% icrt_export/4.
    Vt = vt_no_unsafe(vt_no_unused(Vt4)),
    {Vt, St}.

%% lc_quals(Qualifiers, ImportVarTable, State) ->
%%      {VarTable,ShadowedVarTable,State}
%%  Test list comprehension qualifiers, return all variables. Allow
%%  filters to be both guard tests and general expressions, but the errors
%%  will be for expressions. Return the complete updated vartable including
%%  local variables and all updates. ShadowVarTable contains the state of
%%  each shadowed variable. All variable states of variables in ImportVarTable
%%  that have been shadowed are included in ShadowVarTable. In addition, all
%%  shadowed variables that are not included in ImportVarTable are included
%%  in ShadowVarTable (these are local variables that are not global variables).

lc_quals(Qs, Vt0, St0) ->
    lc_quals(Qs, Vt0, [], St0).

lc_quals([{generate, _Line, P, E} | Qs], Vt0, Uvt0, St0) ->
    {Vt, Uvt, St} = handle_generator(P, E, Vt0, Uvt0, St0),
    lc_quals(Qs, Vt, Uvt, St);
lc_quals([{b_generate, _Line, P, E} | Qs], Vt0, Uvt0, St0) ->
    St1 = handle_bitstring_gen_pat(P, St0),
    {Vt, Uvt, St} = handle_generator(P, E, Vt0, Uvt0, St1),
    lc_quals(Qs, Vt, Uvt, St);
lc_quals([F | Qs], Vt, Uvt, St0) ->
    {Fvt, St1} =
        case is_guard_test2(F, St0) of
            true -> guard_test(F, Vt, St0);
            false -> expr(F, Vt, St0)
        end,
    lc_quals(Qs, vtupdate(Fvt, Vt), Uvt, St1);
lc_quals([], Vt, Uvt, St) ->
    {Vt, Uvt, St}.

handle_generator(P, E, Vt, Uvt, St0) ->
    {Evt, St1} = expr(E, Vt, St0),
    %% Forget variables local to E immediately.
    Vt1 = vtupdate(vtold(Evt, Vt), Vt),
    {_, St2} = check_unused_vars(Evt, Vt, St1),
    {Pvt, Pnew, St3} = pattern(P, Vt1, [], St2),
    %% Have to keep fresh variables separated from used variables somehow
    %% in order to handle for example X = foo(), [X || <<X:X>> <- bar()].
    %%                                1           2      2 1
    Vt2 = vtupdate(Pvt, Vt1),
    St4 = shadow_vars(Pnew, Vt1, generate, St3),
    Svt = vtold(Vt2, Pnew),
    {_, St5} = check_old_unused_vars(Svt, Uvt, St4),
    NUvt = vtupdate(vtnew(Svt, Uvt), Uvt),
    Vt3 = vtupdate(vtsubtract(Vt2, Pnew), Pnew),
    {Vt3, NUvt, St5}.

handle_bitstring_gen_pat({bin, _, Segments = [_ | _]}, St) ->
    case lists:last(Segments) of
        {bin_element, Line, _, default, Flags} when is_list(Flags) ->
            case
                member(binary, Flags) orelse
                    member(bytes, Flags) orelse
                    member(bits, Flags) orelse member(bitstring, Flags)
            of
                true ->
                    add_error(Line, unsized_binary_in_bin_gen_pattern, St);
                false ->
                    St
            end;
        _ ->
            St
    end;
handle_bitstring_gen_pat(_, St) ->
    St.

%% fun_clauses(Clauses, ImportVarTable, State) ->
%%      {UsedVars, State}.
%%  Fun's cannot export any variables.

fun_clauses(Cs, Vt, St) ->
    {Bvt, St2} = foldl(
        fun(C, {Bvt0, St0}) ->
            {Cvt, St1} = fun_clause(C, Vt, St0),
            {vtmerge(Cvt, Bvt0), St1}
        end,
        {[], St},
        Cs
    ),
    Uvt = vt_no_unsafe(vt_no_unused(vtold(Bvt, Vt))),
    {Uvt, St2}.

fun_clause({clause, _Line, H, G, B}, Vt0, St0) ->
    % No imported pattern variables
    {Hvt, Hnew, St1} = head(H, Vt0, [], St0),
    Vt1 = vtupdate(Hvt, Vt0),
    St2 = shadow_vars(Hnew, Vt0, 'fun', St1),
    Vt2 = vtupdate(vtsubtract(Vt1, Hnew), Hnew),
    {Gvt, St3} = guard(G, Vt2, St2),
    Vt3 = vtupdate(Gvt, Vt2),
    {Bvt, St4} = exprs(B, Vt3, St3),
    Cvt = vtupdate(Bvt, Vt3),
    %% Check new local variables.
    {_, St5} = check_unused_vars(Cvt, Vt0, St4),
    %% Check all shadowing variables.
    Svt = vtold(Vt1, Hnew),
    {_, St6} = check_old_unused_vars(Cvt, Svt, St5),
    Vt4 = vtmerge(Svt, vtsubtract(Cvt, Svt)),
    {vtold(Vt4, Vt0), St6}.

%% In the variable table we store information about variables. The
%% information is a tuple {State,Usage,Lines}, the variables state and
%% usage. A variable can be in the following states:
%%
%% bound                everything is normal
%% {export,From}        variable has been exported
%% {unsafe,In}          variable is unsafe
%%
%% The usage information has the following form:
%%
%% used         variable has been used
%% unused       variable has been bound but not used
%%
%% Lines is a list of line numbers where the variable was bound.
%%
%% Report variable errors/warnings as soon as possible and then change
%% the state to ok. This simplifies the code and reports errors only
%% once. Having the usage information like this makes it easy too when
%% merging states.

%% For keeping track of which variables are bound, ordsets are used.
%% In order to be able to give warnings about unused variables, a
%% possible value is {bound, unused, [Line]}. The usual value when a
%% variable is used is {bound, used, [Line]}. An exception occurs for
%% variables in the size position in a bin element in a pattern.
%% Currently, such a variable is never matched out, always used, and
%% therefore it makes no sense to warn for "variable imported in
%% match".

%% For storing the variable table we use the orddict module.
%% We know an empty set is [].

%% pat_var(Variable, LineNo, VarTable, NewVars, State) ->
%%         {UpdVarTable,UpdNewVars,State}
%% A pattern variable has been found. Handle errors and warnings. Return
%% all used variables as bound so errors and warnings are only reported
%% once. New shadows Vt here, which is necessary in order to separate
%% uses of shadowed and shadowing variables. See also pat_binsize_var.

pat_var(V, Line, Vt, New, St) ->
    case orddict:find(V, New) of
        {ok, {bound, _Usage, Ls}} ->
            %% variable already in NewVars, mark as used
            {[], [{V, {bound, used, Ls}}], St};
        error ->
            case orddict:find(V, Vt) of
                {ok, {bound, _Usage, Ls}} ->
                    {[{V, {bound, used, Ls}}], [], St};
                {ok, {{unsafe, In}, _Usage, Ls}} ->
                    {[{V, {bound, used, Ls}}], [], add_error(Line, {unsafe_var, V, In}, St)};
                {ok, {{export, From}, _Usage, Ls}} ->
                    {[{V, {bound, used, Ls}}], [],
                        %% As this is matching, exported vars are risky.
                        add_warning(Line, {exported_var, V, From}, St)};
                {ok, {stacktrace, _Usage, [Line]}} ->
                    %% this is the definitoon & use, all is fine
                    {[], [{V, {bound, unused, [Line]}}], St};
                {ok, {stacktrace, _Usage, Ls}} ->
                    {[{V, {bound, used, Ls}}], [], add_error(Line, {stacktrace_bound, V}, St)};
                error ->
                    %% add variable to NewVars, not yet used
                    {[], [{V, {bound, unused, [Line]}}], St}
            end
    end.

%% pat_binsize_var(Variable, LineNo, VarTable, NewVars, State) ->
%%      {UpdVarTable,UpdNewVars,State'}
%% Special case of pat_var/expr_var for variables in binary size expressions
%% (never adds variables to NewVars, only marks uses).

pat_binsize_var(V, Line, Vt, New, St) ->
    case orddict:find(V, New) of
        {ok, {bound, _Used, Ls}} ->
            {[], [{V, {bound, used, Ls}}], St};
        error ->
            case orddict:find(V, Vt) of
                {ok, {bound, _Used, Ls}} ->
                    {[{V, {bound, used, Ls}}], [], St};
                {ok, {{unsafe, In}, _Used, Ls}} ->
                    {[{V, {bound, used, Ls}}], [], add_error(Line, {unsafe_var, V, In}, St)};
                {ok, {{export, From}, _Used, Ls}} ->
                    {[{V, {bound, used, Ls}}], [],
                        %% As this is not matching, exported vars are
                        %% probably safe.
                        exported_var(Line, V, From, St)};
                error ->
                    {[{V, {bound, used, [Line]}}], [], add_error(Line, {unbound_var, V}, St)}
            end
    end.

%% expr_var(Variable, LineNo, VarTable, State) ->
%%      {UpdVarTable,State}
%%  Check if a variable is defined, or if there is an error or warning
%%  connected to its usage. Return all variables as bound so errors
%%  and warnings are only reported once.  As this is not matching
%%  exported vars are probably safe, warn only if warn_export_vars is
%%  set.

expr_var(V, Line, Vt, #lint{bvt = none} = St) ->
    do_expr_var(V, Line, Vt, St);
expr_var(V, Line, Vt0, #lint{bvt = Bvt0} = St0) when is_list(Bvt0) ->
    %% handles variables in a binary segment size expression
    {Vt, Bvt, St} = pat_binsize_var(V, Line, Vt0, Bvt0, St0),
    {Vt, St#lint{bvt = vtmerge(Bvt0, Bvt)}}.

do_expr_var(V, Line, Vt, St) ->
    case orddict:find(V, Vt) of
        {ok, {bound, _Usage, Ls}} ->
            {[{V, {bound, used, Ls}}], St};
        {ok, {{unsafe, In}, _Usage, Ls}} ->
            {[{V, {bound, used, Ls}}], add_error(Line, {unsafe_var, V, In}, St)};
        {ok, {{export, From}, _Usage, Ls}} ->
            case is_warn_enabled(export_vars, St) of
                true ->
                    {[{V, {bound, used, Ls}}], add_error(Line, {exported_var, V, From}, St)};
                false ->
                    {[{V, {{export, From}, used, Ls}}], St}
            end;
        {ok, {stacktrace, _Usage, Ls}} ->
            {[{V, {bound, used, Ls}}], add_error(Line, {stacktrace_guard, V}, St)};
        error ->
            {[{V, {bound, used, [Line]}}], add_error(Line, {unbound_var, V}, St)}
    end.

exported_var(Line, V, From, St) ->
    case is_warn_enabled(export_vars, St) of
        true -> add_error(Line, {exported_var, V, From}, St);
        false -> St
    end.

unpinned_vars(Vt, Vt0, St0) ->
    OldVt = vt_no_unsafe(Vt0),
    Check = fun({V, {_, _, [L | _]}}, St) ->
        case orddict:find(V, OldVt) of
            {ok, {stacktrace, _, _}} ->
                %% error already reported
                St;
            {ok, _} ->
                add_error(L, {unpinned_var, V}, St);
            error ->
                St
        end
    end,
    foldl(Check, St0, Vt).

shadow_vars(Vt, Vt0, In, St0) ->
    case is_warn_enabled(shadow_vars, St0) of
        true ->
            foldl(
                fun
                    ({V, {_, _, [L | _]}}, St) ->
                        add_error(L, {shadowed_var, V, In}, St);
                    (_, St) ->
                        St
                end,
                St0,
                vtold(Vt, vt_no_unsafe(Vt0))
            );
        false ->
            St0
    end.

check_unused_vars(Vt, Vt0, St0) ->
    U = unused_vars(Vt, Vt0, St0),
    warn_unused_vars(U, Vt, St0).

check_old_unused_vars(Vt, Vt0, St0) ->
    U = unused_vars(vtold(Vt, Vt0), [], St0),
    warn_unused_vars(U, Vt, St0).

unused_vars(Vt, Vt0, _St0) ->
    U0 = orddict:filter(
        fun
            (V, {_State, unused, _Ls}) ->
                case atom_to_list(V) of
                    "_" ++ _ -> false;
                    _ -> true
                end;
            (_V, _How) ->
                false
        end,
        Vt
    ),
    % Only new variables.
    vtnew(U0, Vt0).

warn_unused_vars([], Vt, St0) ->
    {Vt, St0};
warn_unused_vars(U, Vt, St0) ->
    St1 =
        case is_warn_enabled(unused_vars, St0) of
            false ->
                St0;
            true ->
                foldl(
                    fun({V, {_, unused, Ls}}, St) ->
                        foldl(
                            fun(L, St2) ->
                                add_warning(L, {unused_var, V}, St2)
                            end,
                            St,
                            Ls
                        )
                    end,
                    St0,
                    U
                )
        end,
    %% Return all variables as bound so warnings are only reported once.
    UVt = map(fun({V, {State, _, Ls}}) -> {V, {State, used, Ls}} end, U),
    {vtmerge(Vt, UVt), St1}.

is_var_bound(V, Vt) ->
    case orddict:find(V, Vt) of
        {ok, {bound, _Usage, _}} -> true;
        _ -> false
    end.

%% vtupdate(UpdVarTable, VarTable) -> VarTable.
%%  Add the variables in the updated vartable to VarTable. The variables
%%  will be updated with their property in UpdVarTable. The state of
%%  the variables in UpdVarTable will be returned.

vtupdate(Uvt, Vt0) ->
    orddict:merge(
        fun(_V, {S, U1, L1}, {_S, U2, L2}) ->
            {S, merge_used(U1, U2), merge_lines(L1, L2)}
        end,
        Uvt,
        Vt0
    ).

%% vtunsafe(From, UpdVarTable, VarTable) -> UnsafeVarTable.
%%  Return all new variables in UpdVarTable as unsafe.

vtunsafe({Tag, FileLine}, Uvt, Vt) ->
    Line = erl_anno:location(FileLine),
    [{V, {{unsafe, {Tag, Line}}, U, Ls}} || {V, {_, U, Ls}} <- vtnew(Uvt, Vt)].

vtupd_unsafe(Where, NewVt, OldVt) ->
    vtupdate(vtunsafe(Where, NewVt, OldVt), NewVt).

%% vtexport(From, UpdVarTable, VarTable) -> ExpVarTable.
%%  Return all new variables in UpdVarTable as exported.

vtexport({Tag, FileLine}, Uvt, Vt) ->
    Line = erl_anno:location(FileLine),
    [{V, {{export, {Tag, Line}}, U, Ls}} || {V, {_, U, Ls}} <- vtnew(Uvt, Vt)].

vtupd_export(Where, NewVt, OldVt) ->
    vtupdate(vtexport(Where, NewVt, OldVt), NewVt).

%% vtmerge(VarTable, VarTable) -> VarTable.
%%  Merge two variables tables generating a new vartable. Give priority to
%%  errors then warnings.

vtmerge(Vt1, Vt2) ->
    orddict:merge(
        fun(_V, {S1, U1, L1}, {S2, U2, L2}) ->
            {merge_state(S1, S2), merge_used(U1, U2), merge_lines(L1, L2)}
        end,
        Vt1,
        Vt2
    ).

vtmerge(Vts) -> foldl(fun(Vt, Mvts) -> vtmerge(Vt, Mvts) end, [], Vts).

%% this version marks variables that exist in both tables as used
%% (since that implies the compiler will add an equality check)
vtmerge_pat(Vt1, Vt2) ->
    orddict:merge(
        fun(_V, {S1, _Usage1, L1}, {S2, _Usage2, L2}) ->
            {merge_state(S1, S2), used, merge_lines(L1, L2)}
        end,
        Vt1,
        Vt2
    ).

merge_lines(Ls1, Ls2) ->
    ordsets:union(Ls1, Ls2).

%Take the error case
merge_state({unsafe, _F1} = S1, _S2) ->
    S1;
merge_state(_S1, {unsafe, _F2} = S2) ->
    S2;
%Take the warning
merge_state(bound, S2) ->
    S2;
merge_state(S1, bound) ->
    S1;
merge_state({export, F1}, {export, _F2}) ->
    %Sanity check
    %% We want to report the outermost construct
    {export, F1}.

merge_used(used, _Usage2) -> used;
merge_used(_Usage1, used) -> used;
merge_used(unused, unused) -> unused.

%% vtnew(NewVarTable, OldVarTable) -> NewVarTable.
%%  Return all the truly new variables in NewVarTable.

vtnew(New, Old) ->
    orddict:filter(fun(V, _How) -> not orddict:is_key(V, Old) end, New).

%% vtsubtract(VarTable1, VarTable2) -> NewVarTable.
%%  Return all the variables in VarTable1 which don't occur in VarTable2.
%%  Same thing as vtnew, but a more intuitive name for some uses.
vtsubtract(New, Old) ->
    vtnew(New, Old).

%% vtold(NewVarTable, OldVarTable) -> OldVarTable.
%%  Return all the truly old variables in NewVarTable.

vtold(New, Old) ->
    orddict:filter(fun(V, _How) -> orddict:is_key(V, Old) end, New).

vt_no_unsafe(Vt) ->
    [
        V
     || {_, {S, _U, _L}} = V <- Vt,
        case S of
            {unsafe, _} -> false;
            _ -> true
        end
    ].

vt_no_unused(Vt) -> [V || {_, {_, U, _L}} = V <- Vt, U =/= unused].

%% copy_expr(Expr, Line) -> Expr.
%%  Make a copy of Expr converting all line numbers to Line.

copy_expr(Expr, Anno) ->
    erlt_ast:map_anno(Expr, fun(_) -> Anno end).

%% check_remote_function(Line, ModuleName, FuncName, [Arg], State) -> State.
%%  Perform checks on known remote calls.

check_remote_function(Line, M, F, As, St0) ->
    St1 = deprecated_function(Line, M, F, As, St0),
    St2 = check_qlc_hrl(Line, M, F, As, St1),
    St3 = check_load_nif(Line, M, F, As, St2),
    St4 = verify_checked_call(Line, M, F, length(As), St3),
    format_function(Line, M, F, As, St4).

verify_checked_call(_Line, _Mod, _Fun, _Arity, #lint{type_checked = false} = St) ->
    St;
verify_checked_call(_Line, _Mod, _Fun, _Arity, #lint{defs_db = undefined} = St) ->
    St;
verify_checked_call(Line, Mod, Fun, Arity, #lint{defs_db = Defs} = St) ->
    case erlt_defs:find_function(Mod, Fun, Arity, Defs) of
        checked -> St;
        unchecked -> add_error(Line, {unchecked_function, {Mod, Fun, Arity}}, St);
        private -> add_error(Line, {private_function, {Mod, Fun, Arity}}, St);
        error -> add_error(Line, {undefined_checked, {Mod, Fun, Arity}}, St)
    end.

%% check_load_nif(Line, ModName, FuncName, [Arg], State) -> State
%%  Add warning if erlang:load_nif/2 is called when any kind of inlining has
%%  been enabled.
check_load_nif(Line, erlang, load_nif, [_, _], St) ->
    case is_warn_enabled(nif_inline, St) of
        true -> check_nif_inline(Line, St);
        false -> St
    end;
check_load_nif(_Line, _ModName, _FuncName, _Args, St) ->
    St.

check_nif_inline(Line, St) ->
    case any(fun is_inline_opt/1, St#lint.compile) of
        true -> add_warning(Line, nif_inline, St);
        false -> St
    end.

is_inline_opt({inline, [_ | _] = _FAs}) -> true;
is_inline_opt(inline) -> true;
is_inline_opt(_) -> false.

%% check_qlc_hrl(Line, ModName, FuncName, [Arg], State) -> State
%%  Add warning if qlc:q/1,2 has been called but qlc.hrl has not
%%  been included.

check_qlc_hrl(Line, M, F, As, St) ->
    Arity = length(As),
    case As of
        [{lc, _L, _E, _Qs} | _] when M =:= qlc, F =:= q, Arity < 3, not St#lint.xqlc ->
            add_warning(Line, {missing_qlc_hrl, Arity}, St);
        _ ->
            St
    end.

%% deprecated_function(Line, ModName, FuncName, [Arg], State) -> State.
%%  Add warning for calls to deprecated functions.

-dialyzer({no_match, deprecated_function/5}).

deprecated_function(Line, M, F, As, St) ->
    Arity = length(As),
    MFA = {M, F, Arity},
    case otp_internal:obsolete(M, F, Arity) of
        {deprecated, String} when is_list(String) ->
            case
                not is_warn_enabled(deprecated_function, St) orelse
                    ordsets:is_element(MFA, St#lint.not_deprecated)
            of
                true ->
                    St;
                false ->
                    add_warning(Line, {deprecated, MFA, String}, St)
            end;
        {deprecated, Replacement, Rel} ->
            case
                not is_warn_enabled(deprecated_function, St) orelse
                    ordsets:is_element(MFA, St#lint.not_deprecated)
            of
                true ->
                    St;
                false ->
                    add_warning(Line, {deprecated, MFA, Replacement, Rel}, St)
            end;
        {removed, String} when is_list(String) ->
            add_removed_warning(Line, MFA, {removed, MFA, String}, St);
        {removed, Replacement, Rel} ->
            add_removed_warning(Line, MFA, {removed, MFA, Replacement, Rel}, St);
        no ->
            St
    end.

add_removed_warning(Line, {M, _, _} = MFA, Warning, #lint{not_removed = NotRemoved} = St) ->
    case
        is_warn_enabled(removed, St) andalso
            not gb_sets:is_element(M, NotRemoved) andalso
            not gb_sets:is_element(MFA, NotRemoved)
    of
        true ->
            add_warning(Line, Warning, St);
        false ->
            St
    end.

-dialyzer({no_match, deprecated_type/5}).

deprecated_type(L, M, N, As, St) ->
    NAs = length(As),
    case otp_internal:obsolete_type(M, N, NAs) of
        {deprecated, String} when is_list(String) ->
            case is_warn_enabled(deprecated_type, St) of
                true ->
                    add_warning(L, {deprecated_type, {M, N, NAs}, String}, St);
                false ->
                    St
            end;
        {removed, String} ->
            add_warning(L, {removed_type, {M, N, NAs}, String}, St);
        no ->
            St
    end.

obsolete_guard({call, Line, {atom, Lr, F}, As}, St0) ->
    Arity = length(As),
    case erl_internal:old_type_test(F, Arity) of
        false ->
            deprecated_function(Line, erlang, F, As, St0);
        true ->
            St =
                case is_warn_enabled(obsolete_guard, St0) of
                    true ->
                        add_warning(Lr, {obsolete_guard, {F, Arity}}, St0);
                    false ->
                        St0
                end,
            test_overriden_by_local(Lr, F, Arity, St)
    end;
obsolete_guard(_G, St) ->
    St.

test_overriden_by_local(Line, OldTest, Arity, St) ->
    ModernTest = list_to_atom("is_" ++ atom_to_list(OldTest)),
    case is_local_function(St#lint.locals, {ModernTest, Arity}) of
        true ->
            add_error(Line, {obsolete_guard_overridden, OldTest}, St);
        false ->
            St
    end.

check_map_allowed(Line, St) ->
    case St#lint.type_checked of
        true ->
            add_error(Line, map_syntax_disallowed, St);
        false ->
            St
    end.

%% keyword_warning(Line, Atom, State) -> State.
%%  Add warning for atoms that will be reserved keywords in the future.
%%  (Currently, no such keywords to warn for.)
keyword_warning(_Line, _A, St) -> St.

%% format_function(Line, ModName, FuncName, [Arg], State) -> State.
%%  Add warning for bad calls to io:fwrite/format functions.

format_function(Line, M, F, As, St) ->
    case is_format_function(M, F) of
        true ->
            case St#lint.warn_format of
                Lev when Lev > 0 ->
                    case check_format_1(As) of
                        {warn, Level, Fmt, Fas} when Level =< Lev ->
                            add_warning(Line, {format_error, {Fmt, Fas}}, St);
                        _ ->
                            St
                    end;
                _Lev ->
                    St
            end;
        false ->
            St
    end.

is_format_function(io, fwrite) -> true;
is_format_function(io, format) -> true;
is_format_function(io_lib, fwrite) -> true;
is_format_function(io_lib, format) -> true;
is_format_function(M, F) when is_atom(M), is_atom(F) -> false.

%% check_format_1([Arg]) -> ok | {warn,Level,Format,[Arg]}.

check_format_1([Fmt]) ->
    check_format_1([Fmt, {nil, 0}]);
check_format_1([Fmt, As]) ->
    check_format_2(Fmt, canonicalize_string(As));
check_format_1([_Dev, Fmt, As]) ->
    check_format_1([Fmt, As]);
check_format_1(_As) ->
    {warn, 1, "format call with wrong number of arguments", []}.

canonicalize_string({string, Line, Cs}) ->
    foldr(fun(C, T) -> {cons, Line, {integer, Line, C}, T} end, {nil, Line}, Cs);
canonicalize_string(Term) ->
    Term.

%% check_format_2([Arg]) -> ok | {warn,Level,Format,[Arg]}.

check_format_2(Fmt, As) ->
    case Fmt of
        {string, _L, S} -> check_format_2a(S, As);
        {atom, _L, A} -> check_format_2a(atom_to_list(A), As);
        _ -> {warn, 2, "format string not a textual constant", []}
    end.

check_format_2a(Fmt, As) ->
    case args_list(As) of
        true -> check_format_3(Fmt, As);
        false -> {warn, 1, "format arguments not a list", []};
        maybe -> {warn, 2, "format arguments perhaps not a list", []}
    end.

%% check_format_3(FormatString, [Arg]) -> ok | {warn,Level,Format,[Arg]}.

check_format_3(Fmt, As) ->
    case check_format_string(Fmt) of
        {ok, Need} ->
            case args_length(As) of
                Len when length(Need) =:= Len -> ok;
                _Len -> {warn, 1, "wrong number of arguments in format call", []}
            end;
        {error, S} ->
            {warn, 1, "format string invalid (~ts)", [S]}
    end.

args_list({cons, _L, _H, T}) ->
    args_list(T);
%% Strange case: user has written something like [a | "bcd"]; pretend
%% we don't know:
args_list({string, _L, _Cs}) ->
    maybe;
args_list({nil, _L}) ->
    true;
args_list({atom, _, _}) ->
    false;
args_list({integer, _, _}) ->
    false;
args_list({float, _, _}) ->
    false;
args_list(_Other) ->
    maybe.

args_length({cons, _L, _H, T}) -> 1 + args_length(T);
args_length({nil, _L}) -> 0.

check_format_string(Fmt) ->
    extract_sequences(Fmt, []).

extract_sequences(Fmt, Need0) ->
    case string:find(Fmt, [$~]) of
        %That's it
        nomatch ->
            {ok, lists:reverse(Need0)};
        [$~ | Fmt1] ->
            case extract_sequence(1, Fmt1, Need0) of
                {ok, Need1, Rest} -> extract_sequences(Rest, Need1);
                Error -> Error
            end
    end.

extract_sequence(1, [$-, C | Fmt], Need) when C >= $0, C =< $9 ->
    extract_sequence_digits(1, Fmt, Need);
extract_sequence(1, [C | Fmt], Need) when C >= $0, C =< $9 ->
    extract_sequence_digits(1, Fmt, Need);
extract_sequence(1, [$-, $* | Fmt], Need) ->
    extract_sequence(2, Fmt, [int | Need]);
extract_sequence(1, [$* | Fmt], Need) ->
    extract_sequence(2, Fmt, [int | Need]);
extract_sequence(1, Fmt, Need) ->
    extract_sequence(2, Fmt, Need);
extract_sequence(2, [$., C | Fmt], Need) when C >= $0, C =< $9 ->
    extract_sequence_digits(2, Fmt, Need);
extract_sequence(2, [$., $* | Fmt], Need) ->
    extract_sequence(3, Fmt, [int | Need]);
extract_sequence(2, [$. | Fmt], Need) ->
    extract_sequence(3, Fmt, Need);
extract_sequence(2, Fmt, Need) ->
    extract_sequence(4, Fmt, Need);
extract_sequence(3, [$., $* | Fmt], Need) ->
    extract_sequence(4, Fmt, [int | Need]);
extract_sequence(3, [$., _ | Fmt], Need) ->
    extract_sequence(4, Fmt, Need);
extract_sequence(3, Fmt, Need) ->
    extract_sequence(4, Fmt, Need);
extract_sequence(4, [$t, $l | Fmt], Need) ->
    extract_sequence(4, [$l, $t | Fmt], Need);
extract_sequence(4, [$t, $c | Fmt], Need) ->
    extract_sequence(5, [$c | Fmt], Need);
extract_sequence(4, [$t, $s | Fmt], Need) ->
    extract_sequence(5, [$s | Fmt], Need);
extract_sequence(4, [$t, $p | Fmt], Need) ->
    extract_sequence(5, [$p | Fmt], Need);
extract_sequence(4, [$t, $P | Fmt], Need) ->
    extract_sequence(5, [$P | Fmt], Need);
extract_sequence(4, [$t, $w | Fmt], Need) ->
    extract_sequence(5, [$w | Fmt], Need);
extract_sequence(4, [$t, $W | Fmt], Need) ->
    extract_sequence(5, [$W | Fmt], Need);
extract_sequence(4, [$t, C | _Fmt], _Need) ->
    {error, "invalid control ~t" ++ [C]};
extract_sequence(4, [$l, $p | Fmt], Need) ->
    extract_sequence(5, [$p | Fmt], Need);
extract_sequence(4, [$l, $t, $p | Fmt], Need) ->
    extract_sequence(5, [$p | Fmt], Need);
extract_sequence(4, [$l, $P | Fmt], Need) ->
    extract_sequence(5, [$P | Fmt], Need);
extract_sequence(4, [$l, $t, $P | Fmt], Need) ->
    extract_sequence(5, [$P | Fmt], Need);
extract_sequence(4, [$l, $t, C | _Fmt], _Need) ->
    {error, "invalid control ~lt" ++ [C]};
extract_sequence(4, [$l, C | _Fmt], _Need) ->
    {error, "invalid control ~l" ++ [C]};
extract_sequence(4, Fmt, Need) ->
    extract_sequence(5, Fmt, Need);
extract_sequence(5, [C | Fmt], Need0) ->
    case control_type(C, Need0) of
        error -> {error, "invalid control \~" ++ [C]};
        Need1 -> {ok, Need1, Fmt}
    end;
extract_sequence(_, [], _Need) ->
    {error, "truncated"}.

extract_sequence_digits(Fld, [C | Fmt], Need) when C >= $0, C =< $9 ->
    extract_sequence_digits(Fld, Fmt, Need);
extract_sequence_digits(Fld, Fmt, Need) ->
    extract_sequence(Fld + 1, Fmt, Need).

control_type($~, Need) ->
    Need;
control_type($c, Need) ->
    [int | Need];
control_type($f, Need) ->
    [float | Need];
control_type($e, Need) ->
    [float | Need];
control_type($g, Need) ->
    [float | Need];
control_type($s, Need) ->
    [string | Need];
control_type($w, Need) ->
    [term | Need];
control_type($p, Need) ->
    [term | Need];
%% Note: reversed
control_type($W, Need) ->
    [int, term | Need];
%% Note: reversed
control_type($P, Need) ->
    [int, term | Need];
control_type($b, Need) ->
    [term | Need];
control_type($B, Need) ->
    [term | Need];
%% Note: reversed
control_type($x, Need) ->
    [string, term | Need];
%% Note: reversed
control_type($X, Need) ->
    [string, term | Need];
control_type($+, Need) ->
    [term | Need];
control_type($#, Need) ->
    [term | Need];
control_type($n, Need) ->
    Need;
control_type($i, Need) ->
    [term | Need];
control_type(_C, _Need) ->
    error.

%% Prebuild set of local functions (to override auto-import)
local_functions(Forms) ->
    gb_sets:from_list([{Func, Arity} || {F, _, Func, Arity, _} <- Forms, ?IS_FUNCTION(F)]).

%% Predicate to find out if the function is locally defined
is_local_function(LocalSet, {Func, Arity}) ->
    gb_sets:is_element({Func, Arity}, LocalSet).

%% Predicate to see if a function is explicitly imported
is_imported_function(ImportSet, {Func, Arity}) ->
    case orddict:find({Func, Arity}, ImportSet) of
        {ok, _Mod} -> true;
        error -> false
    end.

%% Predicate to see if a function is explicitly imported from the erlang module
is_imported_from_erlang(ImportSet, {Func, Arity}) ->
    case orddict:find({Func, Arity}, ImportSet) of
        {ok, erlang} -> true;
        _ -> false
    end.

%% Build set of functions where auto-import is explicitly suppressed
auto_import_suppressed(CompileFlags) ->
    case lists:member(no_auto_import, CompileFlags) of
        true ->
            all;
        false ->
            L0 = [X || {no_auto_import, X} <- CompileFlags],
            L1 = [{Y, Z} || {Y, Z} <- lists:flatten(L0), is_atom(Y), is_integer(Z)],
            gb_sets:from_list(L1)
    end.

%% Predicate to find out if autoimport is explicitly suppressed for a function
is_autoimport_suppressed(all, {_Func, _Arity}) ->
    true;
is_autoimport_suppressed(NoAutoSet, {Func, Arity}) ->
    gb_sets:is_element({Func, Arity}, NoAutoSet).

%% Predicate to find out if a function specific bif-clash suppression (old deprecated) is present
bif_clash_specifically_disabled(St, {F, A}) ->
    lists:member({F, A}, St#lint.nowarn_bif_clash).

%% Predicate to find out if an autoimported guard_bif is not overriden in some way
%% Guard Bif without module name is disallowed if
%% * It is overridden by local function
%% * It is overridden by -import and that import is not of itself (i.e. from module erlang)
%% * The autoimport is suppressed or it's not reimported by -import directive
%% Otherwise it's OK (given that it's actually a guard bif and actually is autoimported)
no_guard_bif_clash(St, {F, A}) ->
    ((not is_local_function(St#lint.locals, {F, A})) andalso
        ((not is_imported_function(St#lint.imports, {F, A})) orelse
            is_imported_from_erlang(St#lint.imports, {F, A})) andalso
        ((not is_autoimport_suppressed(St#lint.no_auto, {F, A})) orelse
            is_imported_from_erlang(St#lint.imports, {F, A}))).

%% maps_prepend(Key, Value, Map) -> Map.

maps_prepend(Key, Value, Map) ->
    case maps:find(Key, Map) of
        {ok, Values} ->
            maps:put(Key, [Value | Values], Map);
        error ->
            maps:put(Key, [Value], Map)
    end.
