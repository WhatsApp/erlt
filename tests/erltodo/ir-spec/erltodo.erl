-file("erltodo/src/erltodo.erlt", 1).

-module(erltodo).

-eqwalizer_unchecked([{prompt_for_ok, 0},
                      {read_or_error, 1}]).

-export([main/1]).

-type state() :: {'$#erltodo:state',
                  [string()],
                  integer()}.

-type choice() :: {'$#erltodo:choice.add', string()} |
                  {'$#erltodo:choice.delete', integer()} |
                  {'$#erltodo:choice.view_stats'}.

-spec main(term()) -> no_return().

main(_Argv) -> loop({'$#erltodo:state', [], 0}).

loop({'$#erltodo:state', Todos, DeletedCount} =
         State) ->
    display_todos(Todos),
    case prompt(Todos) of
        {'$#erltodo:choice.add', TodoText} ->
            loop({'$#erltodo:state', [TodoText | Todos], 0});
        {'$#erltodo:choice.delete', Index} ->
            loop({'$#erltodo:state',
                  splice(Index, Todos),
                  DeletedCount + 1});
        {'$#erltodo:choice.view_stats'} ->
            display_stats(State),
            loop(State)
    end.

-spec prompt([string()]) -> choice().

prompt(Todos) ->
    t_io:format("What would you like to do?~n1. add todo~n2. "
                "delete todo~n3. view stats~n~n"),
    case read_or_error("Enter a number followed by a dot: ")
        of
        1 -> prompt_add_todo();
        2 -> prompt_delete_todo(Todos);
        3 -> {'$#erltodo:choice.view_stats'};
        _ ->
            invalid_input(),
            prompt(Todos)
    end.

-spec prompt_add_todo() -> choice().

prompt_add_todo() ->
    case
        read_or_error("enter todo text in quotes followed by "
                      "a dot. Example: \"buy eggs\".: ")
        of
        TodoText when erlang:is_list(TodoText) ->
            {'$#erltodo:choice.add', TodoText};
        Res ->
            erlang:display(Res),
            invalid_input(),
            prompt_add_todo()
    end.

-spec prompt_delete_todo([string()]) -> choice().

prompt_delete_todo(Todos) ->
    display_todos(Todos),
    case
        read_or_error("enter the id of the todo you would like "
                      "to delete followed by a dot. ")
        of
        Id when erlang:is_integer(Id) ->
            {'$#erltodo:choice.delete', Id};
        _ ->
            invalid_input(),
            prompt_delete_todo(Todos)
    end.

-spec display_stats(state()) -> string().

display_stats({'$#erltodo:state', [], 0}) ->
    t_io:format("You haven't started yet: try creating "
                "and deleting todos~n"),
    prompt_for_ok(),
    "ok";
display_stats({'$#erltodo:state',
               Todos,
               DeletedCount}) ->
    line(),
    t_io:format("You have ~p active todos and ~p deleted "
                "todos",
                {erlang:length(Todos), DeletedCount}),
    line(),
    prompt_for_ok(),
    "ok".

-spec prompt_for_ok() -> atom().

prompt_for_ok() ->
    case
        read_or_error("type ok followed by a dot to continue. ")
        of
        ok -> ok;
        _ -> prompt_for_ok()
    end.

-spec read_or_error(string()) -> _.

read_or_error(Prompt) ->
    case io:read(Prompt) of
        {ok, Res} -> Res;
        eof ->
            erlang:error("eof when reading from prompt ~p~n",
                         [Prompt]);
        {error, Reason} ->
            erlang:error("error when reading from prompt ~p: ~p~n",
                         [Prompt, Reason])
    end.

with_indices(List) ->
    Indices = t_lists:seq(len(List), 1, -1),
    t_lists:zip(Indices, List).

-spec len([_]) -> integer().

len(L) -> erlang:length(L).

-spec display_todos([string()]) -> atom().

display_todos([]) ->
    line(),
    t_io:format("You have no todos~n"),
    line(),
    ok;
display_todos(Todos) ->
    line(),
    t_io:format("Your todos are:~n"),
    [t_io:format("~p. ~p~n", {Index, Todo})
     || {Index, Todo} <- with_indices(Todos)],
    line(),
    ok.

line() -> t_io:format("------~n").

invalid_input() -> t_io:format("invalid input~n").

-spec splice(number(), [A]) -> [A].

splice(Index, List) ->
    {Left, [_ | Right]} = t_lists:split(Index - 1, List),
    Left ++ Right.



