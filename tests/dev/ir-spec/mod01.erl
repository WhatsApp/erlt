-file("dev/src/mod01.erlt", 1).

-module(mod01).

-eqwalizer_unchecked([]).

-export_type([my_pair2/1, my_pair/2]).

-export([mod01F/1,
         id/1,
         arg13/3,
         arg23/3,
         arg33/3,
         mk_int/0,
         mk_float/0,
         mk_string/0,
         mk_char/0,
         mk_nil/0,
         mk_cons/2,
         mk_tuple/3,
         mk_tuple2/1,
         mk_shape/2,
         update_shape1/2,
         update_shape2/2,
         update_shape3/1,
         update_shape4/1,
         access_shape/1,
         access_shape2/1,
         mk_seq/0,
         is_empty/1,
         is_empty2/1,
         with_as/1,
         block/2,
         is_empty_case/1,
         both_empty/2,
         call/2,
         fun_to_var/0,
         local_fun_to_var/2,
         local_n_fun/0,
         unary_plus/1,
         unary_minus/1,
         unary_not/1,
         unary_bnot/1,
         binary_star/2,
         binary_div/2,
         binary_rem/2,
         binary_band/2,
         binary_and/2,
         binary_plus/2,
         binary_minus/2,
         binary_bor/2,
         binary_bxor/2,
         binary_bsl/2,
         binary_bsr/2,
         binary_or/2,
         binary_xor/2,
         binary_orelse/2,
         binary_andalso/2,
         list_plus/2,
         list_minus/2,
         comp1/2,
         comp2/2,
         comp3/2,
         comp4/2,
         comp5/2,
         comp6/2,
         comp7/2,
         comp8/2,
         guard1/2,
         guard2/3,
         p_match_tuple0/1,
         p_match_tuple1/1,
         p_match_invoke/0,
         any_id/1,
         atom_id/1,
         binary_id/1,
         bitstring_id/1,
         byte_id/1,
         float_id/1,
         identifier_id/1,
         iodata_id/1,
         iolist_id/1,
         none_id/1,
         noreturn_id/1,
         number_id/1,
         pid_id/1,
         port_id/1,
         reference_id/1,
         term_id/1,
         timeout_id/1,
         ints_id/1,
         mk_my_pair/2]).

-spec id(Arg1 :: X) -> X.

id(X) -> X.

-spec arg13(Arg1 :: X, _, _) -> X.

arg13(X, _Y, _Z) -> X.

-spec arg23(_, Arg2 :: Y, _) -> Y.

arg23(_X, Y, _Z) -> Y.

-spec arg33(_, _, Arg3 :: Z) -> Z.

arg33(_X, _Y, Z) -> Z.

-spec mk_int() -> integer().

mk_int() -> 1.

-spec mk_float() -> integer().

mk_float() -> 2.12312309999999992982.

-spec mk_string() -> string().

mk_string() -> "Erlang String".

-spec mk_char() -> char().

mk_char() -> $c.

-spec mk_nil() -> [_].

mk_nil() -> [].

-spec mk_cons(H :: A, T :: [A]) -> [A].

mk_cons(H, T) -> [H | T].

-spec mk_tuple(A, B, C) -> {A, B, C}.

mk_tuple(A, B, C) -> {A, B, C}.

-spec mk_tuple2(A) -> {El1 :: integer(),
                       El2 :: integer(),
                       integer(),
                       El4 :: A}.

mk_tuple2(A) -> {1, 2, 3, A}.

-spec mk_shape(A, B) -> #{a := A, b := B}.

mk_shape(A, B) -> #{a => A, b => B}.

-spec update_shape1(#{a := A}, A) -> #{a := A}.

update_shape1(S, A) -> S#{a => A}.

-spec update_shape2(#{a := A, atom() => any()},
                    A) -> #{a := A, atom() => any()}.

update_shape2(S, A) -> S#{a => A}.

-spec update_shape3(#{a := A, b := A}) -> #{a := A,
                                            b := A}.

update_shape3(S) ->
    A0 = erlang:map_get(a, S),
    B0 = erlang:map_get(a, S),
    S#{a => B0, b => A0}.

-spec update_shape4(#{a := A, b := A,
                      atom() => any()}) -> #{a := A, b := A, atom() => any()}.

update_shape4(S) ->
    A0 = erlang:map_get(a, S),
    B0 = erlang:map_get(a, S),
    S#{a => B0, b => A0}.

-spec access_shape(#{id := Id, location := Location,
                     atom() => any()}) -> {Id, Location}.

access_shape(S) ->
    {erlang:map_get(id, S), erlang:map_get(location, S)}.

-spec access_shape2(#{inner1 := #{inner2 := A}}) -> A.

access_shape2(S) ->
    erlang:map_get(inner2, erlang:map_get(inner1, S)).

-spec mk_seq() -> {integer(), integer()}.

mk_seq() ->
    X = 1,
    Y = 2,
    {X, Y}.

-spec is_empty([_]) -> boolean().

is_empty([]) -> true;
is_empty([_H | _T]) -> false.

-spec is_empty2([_]) -> boolean().

is_empty2([]) -> true;
is_empty2(_) -> false.

-spec with_as(integer()) -> integer().

with_as(1 = X) -> X;
with_as(_ = Y) -> Y.

-spec block(A, [A]) -> {[A], [A]}.

block(X, Y) ->
    {begin Z = [X | Y], Z end, begin [X | Y] end}.

-spec is_empty_case([_]) -> boolean().

is_empty_case(L) ->
    case L of
        [] -> true;
        _ -> false
    end.

-spec both_empty([_], [_]) -> boolean().

both_empty(L1, L2) ->
    case L1 of
        [] ->
            case L2 of
                [] -> true;
                _ -> false
            end;
        _ -> false
    end.

-spec call([_], [_]) -> boolean().

call(L1, L2) -> mod01:both_empty(L1, L2).

-spec fun_to_var() -> fun((A, B) -> {A, B}).

fun_to_var() ->
    F = fun (X, Y) -> {X, Y} end,
    F.

-spec local_fun_to_var(_, _) -> fun(([_],
                                     [_]) -> boolean()).

local_fun_to_var(_A, _B) ->
    F = fun call/2,
    F.

-spec local_n_fun() -> fun(([_]) -> [_]).

local_n_fun() ->
    F = fun Local([_ | T]) -> Local(T);
            Local([]) -> []
        end,
    F.

-spec mod01F(X) -> X.

mod01F(X) -> X.

-spec unary_plus(integer()) -> integer().

unary_plus(X) -> +(+X).

-spec unary_minus(integer()) -> integer().

unary_minus(X) -> -(-X).

-spec unary_not(boolean()) -> boolean().

unary_not(X) -> not (not X).

-spec unary_bnot(integer()) -> integer().

unary_bnot(X) -> bnot (bnot X).

-spec binary_star(integer(), integer()) -> integer().

binary_star(X, Y) -> X * Y.

-spec binary_div(integer(), integer()) -> integer().

binary_div(X, Y) -> X div Y.

-spec binary_rem(integer(), integer()) -> integer().

binary_rem(X, Y) -> X rem Y.

-spec binary_band(integer(), integer()) -> integer().

binary_band(X, Y) -> X band Y.

-spec binary_and(boolean(), boolean()) -> boolean().

binary_and(X, Y) -> X and Y.

-spec binary_plus(integer(), integer()) -> integer().

binary_plus(X, Y) -> X + Y.

-spec binary_minus(integer(), integer()) -> integer().

binary_minus(X, Y) -> X - Y.

-spec binary_bor(integer(), integer()) -> integer().

binary_bor(X, Y) -> X bor Y.

-spec binary_bxor(integer(), integer()) -> integer().

binary_bxor(X, Y) -> X bxor Y.

-spec binary_bsl(integer(), integer()) -> integer().

binary_bsl(X, Y) -> X bsl Y.

-spec binary_bsr(integer(), integer()) -> integer().

binary_bsr(X, Y) -> X bsr Y.

-spec binary_or(boolean(), boolean()) -> boolean().

binary_or(X, Y) -> X or Y.

-spec binary_xor(boolean(), boolean()) -> boolean().

binary_xor(X, Y) -> X xor Y.

-spec binary_orelse(boolean(), boolean()) -> boolean().

binary_orelse(X, Y) -> X orelse Y.

-spec binary_andalso(boolean(), boolean()) -> boolean().

binary_andalso(X, Y) -> X andalso Y.

-spec list_plus([A], [A]) -> [A].

list_plus(X, Y) -> X ++ Y.

-spec list_minus([A], [A]) -> [A].

list_minus(X, Y) -> X -- Y.

-spec comp1(A, A) -> boolean().

comp1(X, Y) -> X == Y.

-spec comp2(A, A) -> boolean().

comp2(X, Y) -> X /= Y.

-spec comp3(A, A) -> boolean().

comp3(X, Y) -> X =< Y.

-spec comp4(A, A) -> boolean().

comp4(X, Y) -> X < Y.

-spec comp5(A, A) -> boolean().

comp5(X, Y) -> X >= Y.

-spec comp6(A, A) -> boolean().

comp6(X, Y) -> X > Y.

-spec comp7(A, A) -> boolean().

comp7(X, Y) -> X =:= Y.

-spec comp8(A, A) -> boolean().

comp8(X, Y) -> X =/= Y.

-spec guard1(integer(), integer()) -> boolean().

guard1(X, Y) when X == 1, Y == 1; X =/= Y -> true;
guard1(X, Y) when X > Y -> false;
guard1(_X, _Y) -> false.

-spec guard2([A], A, [A]) -> [A].

guard2(X, Y, []) ->
    case X of
        [X1 | Xs] when X1 == Y -> Xs;
        Xs -> Xs
    end;
guard2(X, _, Z) when X == Z -> Z;
guard2(_, _, Z) -> Z.

-spec p_match_tuple0({}) -> {{}}.

p_match_tuple0({}) -> {{}}.

-spec p_match_tuple1({_}) -> {}.

p_match_tuple1({_X}) -> {}.

-spec p_match_invoke() -> {}.

p_match_invoke() ->
    p_match_tuple0({}),
    p_match_tuple1({{}}).

-spec any_id(any()) -> any().

any_id(A) -> A.

-spec atom_id(atom()) -> atom().

atom_id(A) -> A.

-spec binary_id(binary()) -> binary().

binary_id(A) -> A.

-spec bitstring_id(bitstring()) -> bitstring().

bitstring_id(A) -> A.

-spec byte_id(byte()) -> byte().

byte_id(A) -> A.

-spec float_id(float()) -> float().

float_id(X) -> X.

-spec identifier_id(identifier()) -> identifier().

identifier_id(A) -> A.

-spec iodata_id(iodata()) -> iodata().

iodata_id(A) -> A.

-spec iolist_id(iolist()) -> iolist().

iolist_id(A) -> A.

-spec none_id(none()) -> none().

none_id(A) -> A.

-spec noreturn_id(no_return()) -> no_return().

noreturn_id(A) -> none_id(A).

-spec number_id(number()) -> number().

number_id(A) -> A.

-spec pid_id(pid()) -> pid().

pid_id(A) -> A.

-spec port_id(port()) -> port().

port_id(A) -> A.

-spec reference_id(reference()) -> reference().

reference_id(A) -> A.

-spec term_id(term()) -> term().

term_id(A) -> A.

-spec timeout_id(timeout()) -> timeout().

timeout_id(A) -> A.

-spec ints_id({neg_integer(),
               non_neg_integer(),
               pos_integer()}) -> {neg_integer(),
                                   non_neg_integer(),
                                   pos_integer()}.

ints_id(X) -> X.

-type my_pair2(A) :: mod01:my_pair(A, A).

-type my_pair(A, B) :: {A, B}.

-spec mk_my_pair(A, A) -> mod01:my_pair2(A).

mk_my_pair(A, B) -> {A, B}.



