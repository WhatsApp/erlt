-module(erlt_lint_types).

-export([module/2, format_error/1]).

%-include("erlt_ast.hrl").

-define(IS_TYPE(Kind),
    Kind =:= type; Kind =:= enum; Kind =:= struct; Kind =:= opaque; Kind =:= unchecked_opaque
).

-record(type_info, {
    name :: atom(),
    kind :: opaque | unchecked_opaque | enum | struct | type | {imported, Module :: atom()},
    used = false :: boolean(),
    arity :: non_neg_integer(),
    line :: erl_anno:anno()
}).

-record(state, {
    module :: atom(),
    defined = #{} :: #{atom() => {unused, {var, erl_anno:anno(), atom()} | used}},
    exported_types = #{} :: #{{atom(), integer()} => ({not_defined, erl_anno:anno()} | defined)},
    exported_functions = #{} :: #{
        {atom(), integer()} => {no_spec_or_def, Line} | ok | {no_spec, Line}
    },
    local_types = #{} :: #{atom() => #type_info{}},
    new_vars = [] :: [atom()],
    errors_on = true :: boolean(),
    allow_new_vars = false :: boolean(),
    file :: string(),
    exported_context = none :: none | {spec | enum | struct | type, {atom(), integer()}},
    errors = [] :: [{File :: string(), ErrorString :: string()}]
}).

format_error({reusing_var, Name}) ->
    io_lib:format("type variable ~tw appears multiple times in type arguments", [Name]);
format_error({unused_arg, Name}) ->
    io_lib:format("The type argument ~tw is not used in the definition", [Name]);
format_error({unsupported_type, Name}) ->
    io_lib:format("The ~tw type is not supported by the type system", [Name]);
format_error({singleton_type, Name}) ->
    io_lib:format("Singleton types of type ~tw are not supported by the type system", [Name]);
format_error({using_undefined_var, Name}) ->
    io_lib:format("The type variable ~tw is undefined", [Name]);
format_error({multiple_specs, Kind}) ->
    io_lib:format("When defining a -~w using multiple specs separated by ';' is not allowed", [Kind]);
format_error({duplicate_exports, Kind, {N, A}}) ->
    io_lib:format("~w ~tw~s exported multiple times.", [Kind, N, gen_type_paren(A)]);
format_error({exported_function_no_spec, {N, A}}) ->
    io_lib:format("The exported checked function ~tw~s has no spec.", [N, gen_type_paren(A)]);
format_error({local_type_in_exported_context, {spec, {N, A}}, Type}) ->
    io_lib:format("The spec of an exported function: ~tw~s contains the local type ~w", [
        N,
        gen_type_paren(A),
        Type
    ]);
format_error({local_type_in_exported_context, {Ctx, {N, A}}, Type}) ->
    io_lib:format("The definition of an exported ~w: ~tw~s contains the local type ~w", [
        Ctx,
        N,
        gen_type_paren(A),
        Type
    ]);
format_error({redefine_import_type, {T, M}}) ->
    io_lib:format("type ~tw already imported from ~w", [T, M]);
format_error({import_type_duplicate, T}) ->
    io_lib:format("type ~tw imported twice", [T]);
format_error({redefine_type, T}) ->
    io_lib:format("type ~tw already defined", [T]);
format_error({undefined_type, {TypeName, Arity}}) ->
    io_lib:format("type ~tw~s undefined", [TypeName, gen_type_paren(Arity)]);
format_error({import_type_wrong_arity, {N, A}}) ->
    io_lib:format("imported type ~tw~s used with the wrong number of arguments", [
        N,
        gen_type_paren(A)
    ]);
format_error({reused_imported_typename, {N, _A}, Kind}) ->
    io_lib:format("defined a new ~w with the same name: ~tw as an imported type", [Kind, N]);
format_error({predefined_type_name, {N, _A}, {imported, M}}) ->
    io_lib:format("imported a type from ~w with the same name: ~tw as a pre-defined type", [M, N]);
format_error({predefined_type_name, {N, _A}, Kind}) ->
    io_lib:format("defined a new ~w with the same name: ~tw as a pre-defined type", [Kind, N]);
format_error(underscore_in_type_arguments) ->
    "_ is not a valid name for a type argument";
format_error(arith_ops) ->
    "Arithmetic expressions are not supported by the type system";
format_error(any_tuple) ->
    "Variable size tuples are not supported by the type system";
format_error(nonempty_list) ->
    "Non empty lists are not supported by the type system";
format_error(untyped_fun_type) ->
    "Fun types without type information are not supported by the type system";
format_error(when_guarded_fun) ->
    "When guards for fun types are not supported by the type system";
format_error(any_argument_fun) ->
    "Funs with an undetermined number of arguments are not allowed by the type system".

gen_type_paren(Arity) when is_integer(Arity), Arity >= 0 ->
    gen_type_paren_1(Arity, ")").

gen_type_paren_1(0, Acc) -> "(" ++ Acc;
gen_type_paren_1(1, Acc) -> "(_" ++ Acc;
gen_type_paren_1(N, Acc) -> gen_type_paren_1(N - 1, ",_" ++ Acc).

module(Ast, FileName) ->
    St = gather_state(Ast, #state{file = FileName}),
    {_, St1} = erlt_ast:postwalk(Ast, St, fun wrap_lint/3),
    case global_analysis(St1) of
        #state{errors = []} ->
            {ok, []};
        #state{errors = Errors} ->
            {error, pack_errors(Errors), []}
    end.

global_analysis(St) ->
    check_exported_types(St),
    check_exported_functions(St).

check_exported_types(#state{exported_types = ExportedTypes} = St) ->
    maps:fold(fun check_exported_type/3, St, ExportedTypes).

check_exported_type({Name, Arity}, {not_defined, Line}, St) ->
    add_error(Line, {undefined_type, {Name, Arity}}, St);
check_exported_type(_, _, St) ->
    St.

check_exported_functions(#state{exported_functions = ExportedFunctions} = St) ->
    maps:fold(fun check_exported_function/3, St, ExportedFunctions).

check_exported_function({Name, Arity}, {no_spec, Line}, St) ->
    add_error(Line, {exported_function_no_spec, {Name, Arity}}, St);
check_exported_function(_, _, St) ->
    St.

gather_state(Ast, St) ->
    [Module] = [M || {attribute, _, module, M} <- Ast],
    ExportedTypes = [
        {Type, {not_defined, Line}}
        || {attribute, Line, export_type, List} <- Ast, Type <- List
    ],
    ExportedFunctions = [
        {Fun, {no_spec_or_def, Line}}
        || {attribute, Line, export, List} <- Ast, Fun <- List
    ],
    {TypeMap, St1} = find_duplicate_exports(ExportedTypes, type, St),
    {FunctionMap, St2} = find_duplicate_exports(ExportedFunctions, function, St1),
    collect_locally_available_types(
        Ast,
        St2#state{module = Module, exported_types = TypeMap, exported_functions = FunctionMap}
    ).

collect_locally_available_types([{attribute, Line, Kind, {Name, _Def, Args}} | Rest], St) when
    ?IS_TYPE(Kind)
->
    collect_locally_available_types(Rest, handle_type_name_def(Name, Line, Kind, length(Args), St));
collect_locally_available_types([{attribute, Line, import_type, {Module, List}} | Rest], St) ->
    collect_locally_available_types(
        Rest,
        lists:foldl(
            fun({Name, Arity}, St0) ->
                handle_type_name_def(Name, Line, {imported, Module}, Arity, St0)
            end,
            St,
            List
        )
    );
collect_locally_available_types([_ | Rest], St) ->
    collect_locally_available_types(Rest, St);
collect_locally_available_types([], St) ->
    St.

handle_type_name_def(Name, Line, Kind, Arity, St) ->
    LocalTypes = St#state.local_types,
    TypeInfo = #type_info{name = Name, kind = Kind, arity = Arity, line = Line},
    case is_predefined_type_name(Name) of
        true ->
            add_error(Line, {predefined_type_name, {Name, Arity}, Kind}, St);
        false ->
            case maps:get(Name, LocalTypes, undefined) of
                undefined ->
                    St#state{local_types = maps:put(Name, TypeInfo, LocalTypes)};
                #type_info{kind = OldKind} ->
                    add_error(Line, redefinition_error(Name, Kind, OldKind), St)
            end
    end.

is_predefined_type_name(Name) ->
    %% There are no predefined types with more than two args.
    lists:any(fun(N) -> erl_internal:is_type(Name, N) end, [0, 1, 2]).

redefinition_error(Name, {imported, _Module}, {imported, _M}) ->
    {import_type_duplicate, Name};
redefinition_error(Name, _, {imported, M}) ->
    {redefine_import_type, {Name, M}};
redefinition_error(Name, _, _) ->
    {redefine_type, Name}.

find_duplicate_exports(Exports, Kind, St) ->
    find_duplicate_exports(Exports, #{}, Kind, St).

find_duplicate_exports([{Export, {Val, Line}} | Rest], Map, Kind, St) ->
    case Map of
        #{Export := _} ->
            find_duplicate_exports(
                Rest,
                Map,
                Kind,
                add_error(Line, {duplicate_exports, Kind, Export}, St)
            );
        _ ->
            find_duplicate_exports(Rest, maps:put(Export, {Val, Line}, Map), Kind, St)
    end;
find_duplicate_exports([], Map, _Kind, St) ->
    {Map, St}.

wrap_lint(I, St, Ctx) ->
    {I, lint(I, St, Ctx)}.

lint({attribute, _, file, {File, _Line}}, St, _) ->
    St#state{file = File};
lint({attribute, _Line, unchecked_opaque, {Name, _Def, Args}}, St, form) ->
    St1 = process_args(Args, St#state{allow_new_vars = true}),
    St2 = defined_type({Name, length(Args)}, unchecked_opaque, St1),
    St2#state{defined = #{}, new_vars = [], allow_new_vars = false};
lint({attribute, _Line, Kind, {Name, Def, Args}}, St, form) when ?IS_TYPE(Kind) ->
    St1 = defined_type({Name, length(Args)}, Kind, St),
    St2 = process_args(Args, St1),
    ArgNames = maps:keys(St2#state.defined),
    St3 = process_def(Def, St2),
    post_process_types(ArgNames, St3);
lint({attribute, _, callback, {_MFA, [TypeSig]}}, St, form) ->
    St1 = process_def(TypeSig, St),
    St1#state{defined = #{}, new_vars = [], allow_new_vars = false};
lint({attribute, _, spec, {MFA, [TypeSig]}}, St, form) ->
    St1 =
        case local_exported_function(MFA, St) of
            {ok, Name} ->
                process_def(
                    TypeSig,
                    mark_exported_function_as_ok(Name, St#state{exported_context = {spec, Name}})
                );
            error ->
                process_def(TypeSig, St)
        end,
    St1#state{defined = #{}, new_vars = [], allow_new_vars = false, exported_context = none};
lint({attribute, Line, Kind, {_MFA, _}}, St, form) when Kind =:= spec; Kind =:= callback ->
    add_error(Line, {multiple_specs, Kind}, St);
lint({unchecked_function, _Line, Name, Arity, _}, St, _Ctx) ->
    mark_exported_function_as_ok({Name, Arity}, St);
lint({function, Line, Name, Arity, _}, St, _Ctx) ->
    mark_exported_function_as_defined({Name, Arity}, Line, St);
lint(_, St, _Ctx) ->
    St.

local_exported_function({F, A}, St) ->
    exported_function({F, A}, St);
local_exported_function({M, F, A}, St) ->
    case St#state.module of
        M ->
            exported_function({F, A}, St);
        _ ->
            error
    end.

exported_function(Fn, #state{exported_functions = Exp}) ->
    case maps:is_key(Fn, Exp) of
        true ->
            {ok, Fn};
        false ->
            error
    end.

mark_exported_function_as_ok(Fun, St) ->
    case maps:is_key(Fun, St#state.exported_functions) of
        true ->
            St#state{exported_functions = maps:put(Fun, ok, St#state.exported_functions)};
        false ->
            St
    end.

mark_exported_function_as_defined(Fun, Line, St) ->
    case maps:get(Fun, St#state.exported_functions, not_exported) of
        ok ->
            St;
        {no_spec_or_def, _} ->
            St#state{
                exported_functions = maps:put(Fun, {no_spec, Line}, St#state.exported_functions)
            };
        not_exported ->
            St
    end.

defined_type(Type, Kind, St) ->
    case is_exported(Type, St) of
        true ->
            define_exported_type(Type, St#state{exported_context = exported_context(Kind, Type)});
        false ->
            St
    end.

exported_context(Kind, Type) ->
    case is_opaque(Kind) of
        true -> none;
        false -> {Kind, Type}
    end.

is_opaque(opaque) -> true;
is_opaque(unchecked_opaque) -> true;
is_opaque(_) -> false.

process_args([{var, Line, '_'} | Rest], St) ->
    process_args(Rest, add_error(Line, underscore_in_type_arguments, St));
process_args([{var, Line, Name} = Var | Rest], St) ->
    case St#state.defined of
        #{Name := _Def} ->
            process_args(Rest, add_error(Line, {reusing_var, Name}, St));
        Defined ->
            process_args(Rest, St#state{defined = maps:put(Name, {unused, Var}, Defined)})
    end;
process_args([], St) ->
    St.

process_list([Type | Rest], St) ->
    process_list(Rest, process_def(Type, St));
process_list([], St) ->
    St.

process_atom_or_list(Atom, St) when is_atom(Atom) ->
    St;
process_atom_or_list(List, St) when is_list(List) ->
    process_list(List, St).

process_def(Type, St) ->
    case Type of
        {var, Line, '_'} ->
            case St of
                #state{allow_new_vars = true} ->
                    St;
                #state{allow_new_vars = false} ->
                    add_error(Line, {unsupported_type, '_'}, St)
            end;
        {var, Line, Name} = V ->
            case St of
                % Variable is an argument of a fun, but is already defined
                % This is ok, we mark it as used.
                #state{allow_new_vars = true, defined = #{Name := _Val}} ->
                    mark_variable_used(Name, St);
                #state{allow_new_vars = true, new_vars = NewVars, defined = Defined} ->
                    St#state{
                        new_vars = [Name | NewVars],
                        defined = maps:put(Name, {unused, V}, Defined)
                    };
                #state{allow_new_vars = false, defined = #{Name := _Val}} ->
                    mark_variable_used(Name, St);
                #state{} ->
                    add_error(Line, {using_undefined_var, Name}, St)
            end;
        {integer, Line, _I} ->
            add_error(Line, {singleton_type, integer}, St);
        {char, Line, _C} ->
            add_error(Line, {singleton_type, char}, St);
        {atom, Line, _A} ->
            add_error(Line, {singleton_type, atom}, St);
        {atom_expr, Line, _A} ->
            add_error(Line, {singleton_type, atom}, St);
        {op, Line, _Args} ->
            add_error(Line, arith_ops, St);
        {type, Line, map, any} ->
            add_error(Line, {unsupported_type, map}, St);
        {type, Line, tuple, any} ->
            add_error(Line, any_tuple, St);
        {type, Line, nil, _Args} ->
            add_error(Line, {singleton_type, '[]'}, St);
        {type, Line, nonempty_list, _Args} ->
            add_error(Line, nonempty_list, St);
        {type, _Line, enum, _Name0, Variants0} ->
            process_list(Variants0, St);
        {variant, _Line, _Name0, Fields0} ->
            process_atom_or_list(Fields0, St);
        {type, _Line, struct, _Name0, Fields0} ->
            process_list(Fields0, St);
        {field_definition, _Line, _Name, _Default, FieldType} ->
            process_def(FieldType, St);
        {type, _Line, open_shape, Args, Var} ->
            process_def(Var, process_list(Args, St));
        {type, _Line, binary, []} ->
            St;
        {type, Line, 'bounded_fun', [Fun, Guards]} ->
            handle_fun(Fun, Guards, add_error(Line, when_guarded_fun, St));
        {type, Line, 'fun', []} ->
            add_error(Line, untyped_fun_type, St);
        {type, Line, 'fun', [{type, _, any}, ResultType]} ->
            {Old, St1} = turn_off_errors(add_error(Line, any_argument_fun, St)),
            reset_errors(process_def(ResultType, St1), Old);
        {type, _Line, 'fun', [_Args, _ResultType]} = Fun ->
            handle_fun(Fun, [], St);
        %% Process subtypes of union and map in order to avoid lots of unused args and type warnings.
        {type, Line, Unsupported, Args} when Unsupported =:= map; Unsupported =:= union ->
            {Old, St1} = turn_off_errors(add_error(Line, {unsupported_type, Unsupported}, St)),
            reset_errors(process_list(Args, St1), Old);
        {type, Line, Name, Args} ->
            case is_unsupported(Name) of
                true ->
                    %% Stop processing other unsupported types
                    add_error(Line, {unsupported_type, Name}, St);
                false ->
                    process_list(Args, St)
            end;
        {type, _, any} ->
            St;
        {ann_type, _Line, [_NonTypeVariable, RealType]} ->
            process_def(RealType, St);
        {remote_type, _Line, [{atom, _, _Mod0}, {atom, _, _Name}, Args]} ->
            process_list(Args, St);
        {user_type, Line, Name, Args} ->
            TA = {Name, length(Args)},
            St1 =
                case classify_user_type(TA, St) of
                    imported ->
                        use_imported_type(TA, Line, St);
                    local when St#state.exported_context =/= none ->
                        add_error(
                            Line,
                            {local_type_in_exported_context, St#state.exported_context, Name},
                            St
                        );
                    undefined ->
                        add_error(Line, {undefined_type, TA}, St);
                    _ ->
                        St
                end,
            process_list(Args, St1)
    end.

classify_user_type(TA, St) ->
    Defined = is_defined(TA, St),
    Exported = is_exported(TA, St),
    Imported = is_imported(TA, St),
    if
        Imported -> imported;
        not Defined -> undefined;
        Exported -> exported;
        true -> local
    end.

is_defined({Type, Arity}, #state{local_types = Local}) ->
    case Local of
        #{Type := #type_info{arity = Arity}} -> true;
        _ -> false
    end.

is_exported(Type, St) ->
    maps:is_key(Type, St#state.exported_types).

is_imported({TypeName, _Arity}, St) ->
    case St#state.local_types of
        #{TypeName := #type_info{kind = {imported, _M}}} -> true;
        _ -> false
    end.

use_imported_type({TypeName, Arity}, Line, St) ->
    LocalTypes = St#state.local_types,
    case maps:get(TypeName, LocalTypes) of
        #type_info{used = false, arity = Arity} = TI ->
            St#state{local_types = maps:put(TypeName, TI#type_info{used = true}, LocalTypes)};
        #type_info{arity = OtherArity} when Arity =/= OtherArity ->
            add_error(Line, {import_type_wrong_arity, {TypeName, OtherArity}}, St);
        #type_info{} ->
            St
    end.

define_exported_type(Type, St) ->
    St#state{exported_types = maps:update(Type, defined, St#state.exported_types)}.

handle_fun({type, _Line, 'fun', [Args, ResultType]}, Guards, St) ->
    Val = St#state.allow_new_vars,
    Old = St#state.new_vars,
    St1 = process_def(Args, St#state{allow_new_vars = true, new_vars = []}),
    %NewVars = St1#state.new_vars,
    St2 = process_list([ResultType | Guards], St1),
    St2#state{allow_new_vars = Val, new_vars = Old}.

mark_variable_used(Name, St) -> St#state{defined = maps:put(Name, used, St#state.defined)}.

is_unsupported(constraint) -> true;
is_unsupported(record) -> true;
is_unsupported(range) -> true;
is_unsupported(binary) -> true;
is_unsupported(_) -> false.

post_process_types([Arg | Rest], St) ->
    Defined = St#state.defined,
    case Defined of
        #{Arg := used} ->
            post_process_types(Rest, St);
        #{Arg := {unused, {var, Line, Arg}}} ->
            post_process_types(Rest, add_error(Line, {unused_arg, Arg}, St))
    end;
post_process_types([], St) ->
    St#state{allow_new_vars = false, new_vars = [], defined = #{}, exported_context = none}.

add_error(Anno, E, St) ->
    case St#state.errors_on of
        true ->
            {File, Location} = loc(Anno, St),
            St#state{errors = [{File, {Location, erlt_lint_types, E}} | St#state.errors]};
        false ->
            St
    end.

turn_off_errors(St) ->
    {St#state.errors_on, St#state{errors_on = false}}.

reset_errors(St, Val) ->
    St#state{errors_on = Val}.

%% copied from erlt_lint.erl
pack_errors(Es) ->
    {Es1, _} = lists:mapfoldl(fun({File, E}, I) -> {{File, {I, E}}, I - 1} end, -1, Es),
    lists:map(
        fun({File, EIs}) -> {File, lists:map(fun({_I, E}) -> E end, EIs)} end,
        pack_warnings(Es1)
    ).

pack_warnings(Ws) ->
    [
        {File, lists:sort([W || {F, W} <- Ws, F =:= File])}
        || File <- lists:usort([F || {F, _} <- Ws])
    ].

loc(Anno, St) ->
    Location0 = erl_anno:location(Anno),
    Location =
        case erlt_parse:get_end_location(Anno) of
            undefined -> Location0;
            EndLoc -> [{location, Location0}, {end_location, EndLoc}]
        end,
    case erl_anno:file(Anno) of
        undefined -> {St#state.file, Location};
        File -> {File, Location}
    end.
