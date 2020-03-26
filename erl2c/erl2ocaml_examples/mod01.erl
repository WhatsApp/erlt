-module(mod01).

-compile(export_all).

id(X) ->
    X.

arg13(X, Y, Z) ->
    X.
arg23(X, Y, Z) ->
    Y.
arg33(X, Y, Z) ->
    Z.

mk_int() ->
    1.

mk_float() ->
    2.1231231.

mk_string() ->
    "Erlang String".

mk_char() ->
    $c.

mk_nil() ->
    [].

mk_cons(H,T) ->
    [H|T].

mk_tuple(A, B, C) ->
    {A, B, C}.

mk_tuple2(A) ->
    {1, 2, 3, A}.

mk_map(A, B) ->
    #{a => A, b => B}.

update_map(M, A, B) ->
    M#{a := {A, B}, b := {B, A}}.

access_map(M) ->
    {maps:get(id, M), maps:get(location, M)}.

mk_seq() ->
    X = 1,
    Y = 2,
    3,
    4,
    {X, Y}.

mk_seq(X, Y) ->
    3,
    4,
    {X, Y}.

is_empty([]) ->
    true;
is_empty([H|T]) ->
    false.

is_empty2([]) ->
    true;
is_empty2(_) ->
    false.

with_as(1 = X) ->
    X;
with_as(_ = Y) ->
    Y.

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

is_empty_case(L) ->
    case L of
        [] -> true;
        _ -> false
    end.

both_empty(L1, L2) ->
    case L1 of
        [] ->
            case L2 of
                [] -> true;
                _ -> false
            end;
        _ -> false
    end.

call(L1, L2) ->
    both_empty(L1, L2).

remote_call(L) ->
    ocaml_list:rev(L).

fun_to_var() ->
    F = fun (X, Y) -> {X, Y} end,
    F.

local_fun_to_var(A, B) ->
    F = fun call/2,
    F.

remote_fun_to_var(A, B) ->
    F = fun ocaml_list:rev/1,
    F.

local_n_fun() ->
    F = fun
            Local([_|T]) -> Local(T);
            Local([]) -> []
        end,
    F.

mod01F(X) ->
    X.

%% Unary operations
unary_plus(X) ->
    + (+ X).

unary_minus(X) ->
    - (- X).

unary_not(X) ->
    not (not X).

unary_bnot(X) ->
    bnot (bnot X).

%% Binary operations
binary_star(X, Y) ->
    X * Y.

binary_div(X, Y) ->
    X div Y.

binary_rem(X, Y) ->
    X rem Y.

binary_band(X, Y) ->
    X band Y.

binary_and(X, Y) ->
    X and Y.

binary_plus(X, Y) ->
    X + Y.

binary_minus(X, Y) ->
    X - Y.

binary_bor(X, Y) ->
    X bor Y.

binary_bxor(X, Y) ->
    X bxor Y.

binary_bsl(X, Y) ->
    X bsl Y.

binary_bsr(X, Y) ->
    X bsr Y.

binary_or(X, Y) ->
    X or Y.

binary_xor(X, Y) ->
    X xor Y.

binary_orelse(X, Y) ->
    X orelse Y.

binary_andalso(X, Y) ->
    X andalso Y.

list_plus(X, Y) ->
    X ++ Y.

list_minus(X, Y) ->
    X -- Y.

comp1(X, Y) ->
    X == Y.

comp2(X, Y) ->
    X /= Y.

comp3(X, Y) ->
    X =< Y.

comp4(X, Y) ->
    X < Y.

comp5(X, Y) ->
    X >= Y.

comp6(X, Y) ->
    X > Y.

comp7(X, Y) ->
    X =:= Y.

comp8(X, Y) ->
    X =/= Y.

guard1(X, Y) when X == 1, Y == 1; X =/= Y ->
    true;
guard1(X, Y) when false ->
    false;
guard1(X, Y) ->
    false.

guard2(X, Y, []) ->
    case X of
        [X1|Xs] when X1 == Y ->
            Xs;
        Xs -> Xs
    end;
guard2(X, _, Z) when X == Z ->
    Z.

p_match_tuple0({}) ->
    {{}}.

p_match_tuple1({X}) ->
    {}.

p_match_invoke() ->
    p_match_tuple0({}),
    p_match_tuple0({{}}).
