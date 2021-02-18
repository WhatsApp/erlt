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

-spec any_binary(any()) -> binary().
any_binary(B) when is_binary(B) -> B;
any_binary(_) -> <<>>.

-spec any_bitstring(any()) -> bitstring().
any_bitstring(B)
    when is_bitstring(B) -> B;
any_bitstring(_) -> <<>>.

-spec binary_slice
    (binary() | list()) -> binary().
binary_slice(B) when is_binary(B) -> B.

-spec unit_fun() -> {}.
unit_fun() -> {}.

-spec id_any(any()) -> any().
id_any(X) -> X.

-spec any_fun(any()) -> fun().
any_fun(F) when is_function(F) -> F;
any_fun(_) -> fun unit_fun/0.

-spec fun_slice
    (fun((any()) -> any()) | atom())
    -> fun((any()) -> any()).
fun_slice(F) when is_function(F) -> F;
fun_slice(_) -> fun id_any/1.

-record(rec1, {id :: atom()}).
-record(rec2, {id :: atom()}).

-spec rec_slice
    (#rec1{} | atom()) -> #rec1{}.
rec_slice(R)
    when is_record(R, rec1) -> R;
rec_slice(A)
    when is_atom(A) -> #rec1{id = A}.

-spec rec_slice1
    (#rec1{} | atom()) -> #rec1{}.
rec_slice1(R)
    when is_record(R, rec1, 1) -> R;
rec_slice1(A)
    when is_atom(A) -> #rec1{id = A}.

-spec rec_cast
    (#rec1{} | #rec2{}) -> #rec1{}.
rec_cast(R)
    when is_record(R, rec1) -> R;
rec_cast(R)
    when is_record(R, rec2) ->
        #rec1{id = R#rec2.id}.

-spec rec_cast1
    (#rec1{} | #rec2{}) -> #rec1{}.
rec_cast1(R)
    when is_record(R, rec1, 1) -> R;
rec_cast1(R)
    when is_record(R, rec2, 1) ->
    #rec1{id = R#rec2.id}.

-spec rec_cast_neg
    (#rec1{} | #rec2{}) -> #rec1{}.
rec_cast_neg(R)
    when is_record(R, rec2) -> R.

-spec rec_cast1_neg
    (#rec1{} | #rec2{}) -> #rec1{}.
rec_cast1_neg(R)
    when is_record(R, rec2, 1) -> R.

-spec any_fun_x(any()) -> fun().
any_fun_x(F) when is_function(F, 2) -> F;
any_fun_x(F) when is_function(F, 3) -> F;
any_fun_x(_) -> fun unit_fun/0.

-spec any_fun_x(any(), number()) -> fun().
any_fun_x(F, Arity)
    when is_function(F, Arity) -> F;
any_fun_x(_, _) -> fun unit_fun/0.

-spec any_mk(any(), any()) -> map().
any_mk(M, K) when is_map_key(K, M) -> M;
any_mk(_, _) -> #{}.
