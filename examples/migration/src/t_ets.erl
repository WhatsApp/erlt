-lang([erl2, ffi]).
-module(t_ets).

-export([
    all/0,
    delete/1,
    delete/2,
    delete_all_objects/1,
    delete_object/2,
    file2tab/1,
    file2tab/2,
    first/1,
    foldl/3,
    give_away/2,
    insert_one/2,
    insert_many/2,
    insert_new_one/2,
    insert_new_many/2,
    last/1,
    lookup/2,
    member/2,
    new/2,
    next/2,
    prev/2,
    rename/2,
    safe_fixtable/2,
    set_heir/2,
    slot/2,
    tab2list/1,
    take/1,
    whereis/1
]).

-import_type(t_erlang, [maybe/1]).
-export_type([tab/0, result/0, t_type/0, t_access/0, t_option/0]).
-enum tab() :: named{atom()} | ref{reference()}.

-enum result() :: ok{tab()} | error{term()}.
%% t_type is only because of ocaml, sterlang is OK.
-enum t_type() :: set{} | ordered_set{} | bag{} | duplicate_bag{}.
-enum t_access() :: public{} | protected{} | private{}.
-enum t_option() :: type{t_type()}
                  | access{t_access()}
                  | named_table{}
                  | keypos{integer()}
                  % WA doesn't use HeirData, so we don't need it)
                  | heir{t_erlang:maybe(pid())}
                  | write_concurrency{boolean()}
                  | read_concurrency{boolean()}
                  | compressed{}.

%% %%%%% Conversions

to_tab(A) when erlang:is_atom(A) ->
    tab.named{A};
to_tab(R) when erlang:is_reference(R) -> tab.ref{R}.

from_tab(tab.named{A}) -> A;
from_tab(tab.ref{R}) -> R.


from_t_option(t_option.type{t_type.set{}}) -> 'set';
from_t_option(t_option.type{t_type.ordered_set{}}) -> 'ordered_set';
from_t_option(t_option.type{t_type.bag{}}) -> 'bag';
from_t_option(t_option.type{t_type.duplicate_bag{}}) -> 'duplicate_bag';
from_t_option(t_option.access{t_access.public{}}) -> 'public';
from_t_option(t_option.access{t_access.protected{}}) -> 'protected';
from_t_option(t_option.access{t_access.private{}}) -> 'private';
from_t_option(t_option.named_table{}) -> 'named_table';
from_t_option(t_option.keypos{Pos}) -> {'keypos', Pos};
from_t_option(t_option.heir{maybe.nothing{}}) -> {'heir', 'none'};
from_t_option(t_option.heir{maybe.just{Pid}}) -> {'heir', Pid, 'undefined'};
from_t_option(t_option.write_concurrency{Flag}) -> {'write_concurrency', Flag};
from_t_option(t_option.read_concurrency{Flag}) -> {'read_concurrency', Flag};
from_t_option(t_option.compressed{}) -> 'compressed'.

%% %%%%% Code

-spec all() -> [tab()].
all() -> [to_tab(T) || T <- ets:all()].

-spec delete(tab()) -> boolean().
delete(T) ->
    ets:delete(from_tab(T)).

-spec delete(tab(), _) -> boolean().
delete(T, K) ->
    ets:delete(from_tab(T), K).

-spec delete_all_objects(boolean()) -> boolean().
delete_all_objects(T) ->
    ets:delete_all_objects(from_tab(T)).

-spec delete_object(tab(), _) -> boolean().
delete_object(T, K) ->
    ets:delete_object(from_tab(T), K).

%% WA uses only strings for filenames here
-spec file2tab(string()) -> result().
file2tab(Filename) ->
    case ets:file2tab(Filename) of
        {ok, Tab} -> result.ok{to_tab(Tab)};
        {error, Err} -> result.error{Err}
    end.

-spec file2tab(string(), boolean()) -> result().
file2tab(Filename, Verify) ->
    case ets:file2tab(Filename, [{verify, Verify}]) of
        {ok, Tab} -> result.ok{to_tab(Tab)};
        {error, Err} -> result.error{Err}
    end.

-spec first(tab()) -> maybe(_).
first(Tab) ->
    case ets:first(from_tab(Tab)) of
        '$end_of_table' -> maybe.nothing{};
        NKey -> maybe.just{NKey}
    end.

-spec foldl(fun((_Elem, A) -> A), A, tab()) -> A.
foldl(F, Zero, Tab) ->
    ets:foldl(F, Zero, from_tab(Tab)).

-spec give_away(tab(), pid()) -> boolean().
give_away(Tab, Pid) ->
    ets:give_away(from_tab(Tab), Pid, undefined).

-spec insert_one(tab(), _) -> boolean().
insert_one(Tab, Object) ->
    ets:insert(from_tab(Tab), Object).

-spec insert_many(tab(), [_]) -> boolean().
insert_many(Tab, Objects) ->
    ets:insert(from_tab(Tab), Objects).

-spec insert_new_one(tab(), _) -> boolean().
insert_new_one(Tab, Object) ->
    ets:insert_new(from_tab(Tab), Object).

-spec insert_new_many(tab(), [_]) -> boolean().
insert_new_many(Tab, Objects) ->
    ets:insert_new(from_tab(Tab), Objects).

-spec last(tab()) -> maybe(_).
last(Tab) ->
    case ets:last(from_tab(Tab)) of
        '$end_of_table' -> maybe.nothing{};
        NKey -> maybe.just{NKey}
    end.

-spec lookup(tab(), _) -> [_].
lookup(Tab, Key) ->
    ets:lookup(from_tab(Tab), Key).

-spec member(tab(), _) -> boolean().
member(Tab, Key) ->
    ets:member(from_tab(Tab), Key).

-spec new(atom(), [t_option()]) -> tab().
new(Name, Options) ->
    Options1 = [from_t_option(Opt) || Opt <- Options],
    Tab = ets:new(Name, Options1),
    to_tab(Tab).

-spec next(tab(), Key) -> maybe(Key).
next(Tab, Key) ->
    case ets:next(from_tab(Tab), Key) of
        '$end_of_table' -> maybe.nothing{};
        NKey -> maybe.just{NKey}
    end.

-spec prev(tab(), Key) -> maybe(Key).
prev(Tab, Key) ->
    case ets:prev(from_tab(Tab), Key) of
        '$end_of_table' -> maybe.nothing{};
        NKey -> maybe.just{NKey}
    end.

-spec rename(tab(), atom()) -> atom().
rename(Tab, Name) ->
    ets:rename(from_tab(Tab), Name).

-spec safe_fixtable(tab(), boolean()) -> boolean().
safe_fixtable(Tab, Fix) ->
    ets:safe_fixtable(from_tab(Tab), Fix).

-spec set_heir(tab(), maybe(pid())) -> boolean().
set_heir(Tab, maybe.nothing{}) ->
    ets:setopts(from_tab(Tab), {heir, none});
set_heir(Tab, maybe.just{Pid}) ->
    ets:setopts(from_tab(Tab), {heir, Pid, undefined}).

-spec slot(tab(), integer()) -> maybe([_]).
slot(Tab, I) ->
    case ets:slot(from_tab(Tab), I) of
        '$end_of_table' -> maybe.nothing{};
        Data -> maybe.just{Data}
    end.

-spec tab2list(tab()) -> [_].
tab2list(Tab) ->
    ets:tab2list(from_tab(Tab)).

-spec take(tab()) -> [_].
take(Tab) ->
    ets:take(from_tab(Tab)).

-spec whereis(atom()) -> maybe(reference()).
whereis(TableName) ->
    case ets:whereis(TableName) of
        undefined -> maybe.nothing{};
        Ref -> maybe.just{Ref}
    end.
