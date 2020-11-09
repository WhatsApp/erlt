-file("erltodo/src/erltodo.erlt", 1).

-module(erltodo).

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
    io:format("What would you like to do?\n1. add todo~n2. "
              "delete todo~n3. view stats~n~n"),
    case io:read("Enter a number followed by a dot: ") of
        {ok, 1} -> prompt_add_todo();
        {ok, 2} -> prompt_delete_todo(Todos);
        {ok, 3} -> {'$#erltodo:choice.view_stats'};
        {ok, _} ->
            invalid_input(),
            prompt(Todos)
    end.

prompt_add_todo() ->
    case io:read("enter todo text in quotes followed by "
                 "a dot. Example: \"buy eggs\".: ")
        of
        {ok, TodoText} when erlang:is_list(TodoText) ->
            {'$#erltodo:choice.add', TodoText};
        {ok, Res} ->
            erlang:display(Res),
            invalid_input(),
            prompt_add_todo()
    end.

prompt_delete_todo(Todos) ->
    display_todos(Todos),
    case io:read("enter the id of the todo you would like "
                 "to delete followed by a dot. ")
        of
        {ok, Id} when erlang:is_integer(Id) ->
            {'$#erltodo:choice.delete', Id};
        {ok, _} ->
            invalid_input(),
            prompt_delete_todo(Todos)
    end.

-spec display_stats(state()) -> atom().

display_stats({'$#erltodo:state', [], 0}) ->
    io:format("You haven't started yet: try creating "
              "and deleting todos~n"),
    {ok, _} =
        io:read("type ok followed by a dot to continue. "),
    ok;
display_stats({'$#erltodo:state',
               Todos,
               DeletedCount}) ->
    line(),
    io:format("You have ~p active todos and ~p deleted "
              "todos",
              [erlang:length(Todos), DeletedCount]),
    line(),
    {ok, _} =
        io:read("type ok followed by a dot to continue. "),
    ok.

with_indices(List) ->
    Indices = lists:seq(len(List), 1, -1),
    lists:zip(Indices, List).

-spec len([_]) -> integer().

len(L) -> erlang:length(L).

-spec display_todos([string()]) -> string().

display_todos([]) ->
    line(),
    t_io:format("You have no todos~n"),
    line(),
    "ok";
display_todos(Todos) ->
    line(),
    t_io:format("Your todos are:~n"),
    [t_io:format("~p. ~p~n", {Index, Todo})
     || {Index, Todo} <- with_indices(Todos)],
    line(),
    "ok".

line() -> t_io:format("------~n").

invalid_input() -> t_io:format("invalid input~n").

-spec splice(number(), [A]) -> [A].

splice(Index, List) ->
    {Left, [_ | Right]} = lists:split(Index - 1, List),
    Left ++ Right.



