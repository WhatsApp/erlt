-module(mod01).

-compile(export_all).

-spec id(X) -> X.
id(X) ->
    X.

-spec arg13(X, _, _) -> X.
arg13(X, Y, Z) ->
    X.
-spec arg23(_, Y, _) -> Y.
arg23(X, Y, Z) ->
    Y.
-spec arg33(_, _, Z) -> Z.
arg33(X, Y, Z) ->
    Z.

-spec mk_int() -> integer().
mk_int() ->
    1.

-spec mk_float() -> integer().
mk_float() ->
    2.1231231.

-spec mk_string() -> string().
mk_string() ->
    "Erlang String".

-spec mk_char() -> char().
mk_char() ->
    $c.

-spec mk_nil() -> [_].
mk_nil() ->
    [].

-spec mk_cons(A, [A]) -> [A].
mk_cons(H,T) ->
    [H|T].

-spec mk_tuple(A, B, C) -> {A, B, C}.
mk_tuple(A, B, C) ->
    {A, B, C}.

-spec mk_tuple2(A) -> {integer(), integer(), integer(), A}.
mk_tuple2(A) ->
    {1, 2, 3, A}.

-spec mk_map(A, B) -> #{a := A, b := B}.
mk_map(A, B) ->
    #{a => A, b => B}.

-spec update_map1(#{a := A}, A) -> #{a := A}.
update_map1(M, A) ->
    M#{a := A}.

-spec update_map2(#{a := A, _ := M}, A) -> #{a := A, _ := M}.
update_map2(M, A) ->
    M#{a := A}.

-spec update_map3(#{a := A, b := A}) -> #{a := A, b := A}.
update_map3(M) ->
    A0 = maps:get(a, M),
    B0 = maps:get(a, M),
    M#{a := B0, b := A0}.

-spec update_map4(#{a := A, b := A, _ := M}) -> #{a := A, b := A, _ := M}.
update_map4(M) ->
    A0 = maps:get(a, M),
    B0 = maps:get(a, M),
    M#{a := B0, b := A0}.

-spec access_map(#{id := Id, location := Location, _ := _}) -> {Id, Location}.
access_map(M) ->
    {maps:get(id, M), maps:get(location, M)}.

-spec mk_seq() -> {integer(), integer()}.
mk_seq() ->
    X = 1,
    Y = 2,
    3,
    4,
    {X, Y}.

-spec is_empty([_]) -> boolean().
is_empty([]) ->
    true;
is_empty([H|T]) ->
    false.

-spec is_empty2([_]) -> boolean().
is_empty2([]) ->
    true;
is_empty2(_) ->
    false.

-spec with_as(integer()) -> integer().
with_as(1 = X) ->
    X;
with_as(_ = Y) ->
    Y.

-spec block(A, [A]) -> {[A], [A]}.
block(X, Y) ->
    {
        begin
            Z = [X | Y],
            Z
        end,
        begin
            [X | Y]
        end
    }.

-spec is_empty_case([_]) -> boolean().
is_empty_case(L) ->
    case L of
        [] -> true;
        _ -> false
    end.

-spec both_empty([_],[_]) -> boolean().
both_empty(L1, L2) ->
    case L1 of
        [] ->
            case L2 of
                [] -> true;
                _ -> false
            end;
        _ -> false
    end.

-spec call([_],[_]) -> boolean().
call(L1, L2) ->
    both_empty(L1, L2).

-spec remote_call([A]) -> [A].
remote_call(L) ->
    ocaml_list:rev(L).

-spec fun_to_var() -> fun((A, B) -> {A, B}).
fun_to_var() ->
    F = fun (X, Y) -> {X, Y} end,
    F.

-spec local_fun_to_var(_, _) -> fun(([_],[_]) -> boolean()).
local_fun_to_var(A, B) ->
    F = fun call/2,
    F.

-spec remote_fun_to_var(_, _) -> fun(([A]) -> [A]).
remote_fun_to_var(A, B) ->
    F = fun ocaml_list:rev/1,
    F.

-spec local_n_fun() -> fun(([_]) -> [_]).
local_n_fun() ->
    F = fun
            Local([_|T]) -> Local(T);
            Local([]) -> []
        end,
    F.

-spec mod01F(X) -> X.
mod01F(X) ->
    X.

%% Unary operations
-spec unary_plus(integer()) -> integer().
unary_plus(X) ->
    + (+ X).

-spec unary_minus(integer()) -> integer().
unary_minus(X) ->
    - (- X).

-spec unary_not(boolean()) -> boolean().
unary_not(X) ->
    not (not X).

-spec unary_bnot(integer()) -> integer().
unary_bnot(X) ->
    bnot (bnot X).

%% Binary operations
-spec binary_star(integer(), integer()) -> integer().
binary_star(X, Y) ->
    X * Y.

-spec binary_div(integer(), integer()) -> integer().
binary_div(X, Y) ->
    X div Y.

-spec binary_rem(integer(), integer()) -> integer().
binary_rem(X, Y) ->
    X rem Y.

-spec binary_band(integer(), integer()) -> integer().
binary_band(X, Y) ->
    X band Y.

-spec binary_and(boolean(), boolean()) -> boolean().
binary_and(X, Y) ->
    X and Y.

-spec binary_plus(integer(), integer()) -> integer().
binary_plus(X, Y) ->
    X + Y.

-spec binary_minus(integer(), integer()) -> integer().
binary_minus(X, Y) ->
    X - Y.

-spec binary_bor(integer(), integer()) -> integer().
binary_bor(X, Y) ->
    X bor Y.

-spec binary_bxor(integer(), integer()) -> integer().
binary_bxor(X, Y) ->
    X bxor Y.

-spec binary_bsl(integer(), integer()) -> integer().
binary_bsl(X, Y) ->
    X bsl Y.

-spec binary_bsr(integer(), integer()) -> integer().
binary_bsr(X, Y) ->
    X bsr Y.

-spec binary_or(boolean(), boolean()) -> boolean().
binary_or(X, Y) ->
    X or Y.

-spec binary_xor(boolean(), boolean()) -> boolean().
binary_xor(X, Y) ->
    X xor Y.

-spec binary_orelse(boolean(), boolean()) -> boolean().
binary_orelse(X, Y) ->
    X orelse Y.

-spec binary_andalso(boolean(), boolean()) -> boolean().
binary_andalso(X, Y) ->
    X andalso Y.

-spec list_plus([A], [A]) -> [A].
list_plus(X, Y) ->
    X ++ Y.

-spec list_minus([A], [A]) -> [A].
list_minus(X, Y) ->
    X -- Y.

-spec comp1(A, A) -> boolean().
comp1(X, Y) ->
    X == Y.

-spec comp2(A, A) -> boolean().
comp2(X, Y) ->
    X /= Y.

-spec comp3(A, A) -> boolean().
comp3(X, Y) ->
    X =< Y.

-spec comp4(A, A) -> boolean().
comp4(X, Y) ->
    X < Y.

-spec comp5(A, A) -> boolean().
comp5(X, Y) ->
    X >= Y.

-spec comp6(A, A) -> boolean().
comp6(X, Y) ->
    X > Y.

-spec comp7(A, A) -> boolean().
comp7(X, Y) ->
    X =:= Y.

-spec comp8(A, A) -> boolean().
comp8(X, Y) ->
    X =/= Y.

-spec guard1(integer(), integer()) -> boolean().
guard1(X, Y) when X == 1, Y == 1; X =/= Y ->
    true;
guard1(X, Y) when false ->
    false;
guard1(X, Y) ->
    false.

-spec guard2([A], A, [A]) -> [A].
guard2(X, Y, []) ->
    case X of
        [X1|Xs] when X1 == Y ->
            Xs;
        Xs -> Xs
    end;
guard2(X, _, Z) when X == Z ->
    Z;
guard2(_, _, Z) ->
    Z.

-spec p_match_tuple0({}) -> {{}}.
p_match_tuple0({}) ->
    {{}}.

-spec p_match_tuple1({_}) -> {}.
p_match_tuple1({X}) ->
    {}.

-spec p_match_invoke() -> {}.
p_match_invoke() ->
    p_match_tuple0({}),
    p_match_tuple1({{}}).
