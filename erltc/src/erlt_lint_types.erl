-module(erlt_lint_types).

-export([module/2, format_error/1]).

-record(state, {
    defined = #{} :: #{atom() => {unused, {var, erl_anno:anno(), atom()} | used}},
    new_vars = [] :: [atom()],
    allow_new_vars = false :: boolean(),
    file :: string(),
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

module(Ast, FileName) ->
    case erlt_ast:postwalk(Ast, #state{file = FileName}, fun wrap_lint/3) of
        {_, #state{errors = []}} ->
            {ok, []};
        {_, #state{errors = Errors}} ->
            {error, pack_errors(Errors), []}
    end.

wrap_lint(I, St, Ctx) ->
    {I, lint(I, St, Ctx)}.

lint({attribute, _, file, {File, _Line}}, St, _) ->
    St#state{file = File};
lint({attribute, _, Type, {_Name, Def, Args}}, St, form) when
    Type =:= type; Type =:= enum; Type =:= struct; Type =:= opaque
->
    St1 = process_args(Args, St),
    ArgNames = maps:keys(St1#state.defined),
    St2 = process_def(Def, St1),
    check_args_used(ArgNames, St2);
lint({attribute, _, unchecked_opaque, {_Name, _Def, Args}}, St, form) ->
    St1 = process_args(Args, St#state{allow_new_vars = true}),
    St1#state{defined = #{}, new_vars = [], allow_new_vars = false};
lint({attribute, _, Kind, {_MFA, [TypeSig]}}, St, form) when Kind =:= spec; Kind =:= callback ->
    St1 = process_def(TypeSig, St),
    St1#state{defined = #{}, new_vars = [], allow_new_vars = false};
lint({attribute, Line, Kind, {_MFA, _}}, St, form) when Kind =:= spec; Kind =:= callback ->
    add_error(Line, {multiple_specs, Kind}, St);
lint(_I, St, _Ctx) ->
    St.

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
            process_list(Fields0, St);
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
            process_def(ResultType, add_error(Line, any_argument_fun, St));
        {type, _Line, 'fun', [_Args, _ResultType]} = Fun ->
            handle_fun(Fun, [], St);
        %% Process subtypes of union and map in order to avoid lots of unused args and type warnings.
        {type, Line, Unsupported, Args} when Unsupported =:= map; Unsupported =:= union ->
            add_error(Line, {unsupported_type, Unsupported}, process_list(Args, St));
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
        {user_type, _Line, _Name, Args} ->
            process_list(Args, St)
    end.

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
is_unsupported(map) -> true;
is_unsupported(range) -> true;
is_unsupported(binary) -> true;
is_unsupported(_) -> false.

check_args_used([Arg | Rest], St) ->
    Defined = St#state.defined,
    case Defined of
        #{Arg := used} ->
            check_args_used(Rest, St);
        #{Arg := {unused, {var, Line, Arg}}} ->
            check_args_used(Rest, add_error(Line, {unused_arg, Arg}, St))
    end;
check_args_used([], St) ->
    St#state{allow_new_vars = false, new_vars = [], defined = #{}}.

add_error(Anno, E, St) ->
    {File, Location} = loc(Anno, St),
    St#state{errors = [{File, {Location, erlt_lint_types, E}} | St#state.errors]}.

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
