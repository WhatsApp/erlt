-module(type_predicates).

-compile([export_all, nowarn_export_all]).

-spec any_tuple(any()) -> tuple().
any_tuple(T) when is_tuple(T) -> T;
any_tuple(_) -> {}.

-spec tuple_slice
    ({any(), any()} | number())
    -> {any(), any()} | {}.
tuple_slice(T) when is_tuple(T) -> T;
tuple_slice(_) -> {}.

-spec tuple_gen
    ({} | {any()} | {any(), any()}) ->
    tuple().
tuple_gen(T) -> T.

-spec any_list(any()) -> list().
any_list(L) when is_list(L) -> L;
any_list(_) -> [].

-spec list_slice([A] | tuple()) -> [A].
list_slice(L) when is_list(L) -> L;
list_slice(_) -> [].

-spec list_gen
    ([number()] | [atom()]) -> list().
list_gen(L) -> L.
