-module(erlt_lint_types).

-export([module/2, format_error/1]).

-record(state,
    {
        args = #{} :: #{atom() => {unused, {var, erl_anno:anno(), atom()} | used}},
        new_args = #{} :: #{atom() => {unused, {var, erl_anno:anno(), atom()}}},
        allow_new_vars = false :: boolean(),
        file :: string(),
        errors = [] :: [{File::string(), ErrorString::string()}]}
).

format_error({reusing_var, Name}) ->
    io_lib:format("The type variable ~tw is already used in this variable list", [Name]);
format_error({unused_arg, Name}) ->
    io_lib:format("The type argument ~tw is not used in the definition", [Name]);
format_error({unsupported_type, Name}) ->
    io_lib:format("The ~tw type is not supported in the type system", [Name]);
format_error({singleton_type, Name}) ->
    io_lib:format("Singelton types of type ~tw are not supported in the type system", [Name]);
format_error({using_undefined_var, Name}) ->
    io_lib:format("The type variable ~tw is undefined", [Name]);
format_error(arith_ops) ->
    "Arithmetic expressions are not supported by the type system";
format_error(any_tuple) ->
    "Variable size tuples are not supported by the type system";
format_error(any_map) ->
    "Untyped maps are not supported by the type system";
format_error(non_empty_list) ->
    "Non empty lists are not supported by the type system";
format_error(untyped_fun_type) ->
    "fun types without type information are not supported by the type system";
format_error(any_argument_fun) ->
    "Funs with an undetermined number of arguments are not allowed by the type system".

module(Ast, FileName) ->
    case erlt_ast:postwalk(Ast, #state{file=FileName}, fun wrap_lint/3) of
        {_, #state{errors = []}} ->
            {ok, []};
        {_, #state{errors = Errors}} ->
            {error, pack_errors(Errors), []}
    end.

wrap_lint(I, St, Ctx) ->
    {I, lint(I, St, Ctx)}.


lint({attribute, _, file, {File, _Line}}, St, _) ->
    St#state{file=File};
lint({attribute, _, Type, {_Name, Def, Args}}, St, form) when Type =:= type; Type =:= enum; Type =:= struct; Type =:= opaque ->
    St1 = process_list(Args, St#state{allow_new_vars=true}),
    NewArgs = St1#state.new_args,
    St2 = process_def(Def, St1#state{allow_new_vars=false, new_args = #{}, args = NewArgs}),
    check_args_used(maps:keys(NewArgs), St2);
lint({attribute, _, unchecked_opaque, {_Name, _Def, Args}}, St, form) ->
    process_list(Args, St#state{allow_new_vars=true});
lint(_I, St, _Ctx) -> 
    St.

process_list([Type|Rest], St) ->
    process_list(Rest, process_def(Type, St));
process_list([], St) ->
    St.

process_def(Type, St) ->
    case Type of
        {var, Line, Name} = V -> 
            case St of
                % Variable is an argument of a fun, but is already defined
                % This is ok, we mark it as used.
                #state{allow_new_vars = true, args = #{Name := _Val}} ->
                    mark_variable_used(Name, St);
                % Variable is an argument of the type or of a fun, but is already used
                % as an argument in the same argument list. This is forbidden.
                #state{allow_new_vars = true, new_args = #{Name := _Val}} ->
                    add_error(Line, {reusing_var, Name}, St);
                #state{allow_new_vars = true, new_args = NewArgs} ->
                    St#state{new_args=maps:put(Name, {unused, V}, NewArgs)};
                #state{allow_new_vars = false, args = #{Name := _Val}} ->
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
            add_error(Line, any_map, St);
        {type, Line, tuple, any} ->
            add_error(Line, any_tuple, St);
        {type, Line, nil, _Args} ->
            add_error(Line, {singleton_type, '[]'}, St);
        {type, Line, non_empty_list, _Args} ->
            add_error(Line, non_empty_list, St);
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
        {type, Line, 'fun', []} ->
            add_error(Line, untyped_fun_type, St);
        {type, Line, 'fun', [{type, _, any}, ResultType]} ->
            process_def(ResultType, add_error(Line, any_argument_fun, St));
        {type, _Line, 'fun', [Args, ResultType]} ->
            St1 = process_def(Args, St#state{allow_new_vars=true}),
            NewArgs = St1#state.new_args,
            St2 = process_def(ResultType, 
                St1#state{allow_new_vars=false, new_args = #{}, args = maps:merge(St1#state.args, NewArgs)}),
            check_args_used(maps:keys(NewArgs), St2);
        {type, Line, Name, Args} ->
            St1 =
                case is_unsupported(Name) of
                    true ->
                        add_error(Line, {unsupported_type, Name}, St);
                    false ->
                        St
                end,
            process_list(Args, St1);
        {type, _, any} ->
            St;
        {ann_type, _Line, [_NonTypeVariable, RealType]} ->
            process_def(RealType, St);
        {remote_type, _Line, [{atom, _, _Mod0}, {atom, _, _Name}, Args]} ->
            process_list(Args, St);
        {user_type, _Line, _Name, Args} ->
            process_list(Args, St)
    end.

mark_variable_used(Name, St) -> St#state{args=maps:put(Name, used, St#state.args)}.

is_unsupported(constraint) -> true;
is_unsupported(union) -> true;
is_unsupported(record) -> true;
is_unsupported(map) -> true;
is_unsupported(range) -> true;
is_unsupported(binary) -> true;
is_unsupported(_) -> false.

check_args_used([Arg|Rest], St) ->
    Args = St#state.args,  
    case Args of
        #{Arg := used} ->
            check_args_used(Rest, St#state{args=maps:remove(Arg, Args)});
        #{Arg := {unused, {var, Line, Arg}}} ->
            check_args_used(Rest, add_error(Line, {unused_arg, Arg}, St#state{args=maps:remove(Arg, Args)}))
    end;
check_args_used([], St) ->
    St.    

add_error(Anno, E, St) ->
     {File, Location} = loc(Anno, St),
     St#state{errors = [{File, {Location, erlt_lint_types, E}} | St#state.errors]}.

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

    