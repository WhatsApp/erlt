-module(expr1).

-compile([export_all, nowarn_export_all]).

-record(test_rec, {id}).

id(X) ->
    X.

atom_lit() ->
    atom.

int_lit() ->
    1.

float_lit() ->
    1.0.

char_lit() ->
    $c.

string_lit() ->
    "".

match() ->
    X = 1,
    X.

tuple() ->
    {tuple, 1}.

nil() ->
    [].

cons(H, T) ->
    [H | T].

binary() ->
    <<>>.

un_op(X) ->
    not X.

bin_op(X, Y) ->
    X andalso Y.

record_create() ->
    #test_rec{}.

record_update(Rec) ->
    Rec#test_rec{id = 1}.

record_select(Rec) ->
    Rec#test_rec.id.

record_index() ->
    #test_rec.id.

map_create() ->
    #{id => 1}.

map_update(M) ->
    M#{id => 1}.

call1(X) ->
    id(X).

call2(X) ->
    terms1:id(X).

list_comp1() ->
    [X || X <- [1, 2, 3]].

list_comp2() ->
    [X || <<X>> <= <<1, 2, 3>>].

binary_comp1() ->
    << X || X <- [1, 2, 3] >>.

binary_comp2() ->
    << X || <<X>> <= <<1, 2, 3>> >>.

dynamic_call1(F, Arg) ->
    F(Arg).

dynamic_call2(M, F, Arg) ->
    M:F(Arg).

block(X) ->
    begin id(X) end.

if_exp(X, Y, Z) ->
    if
        X -> Y;
        true -> Z
    end.

case_exp(B) ->
    case B of
        true -> false;
        false -> true
    end.

try_exp(E) ->
    try
        E
    catch
        Y -> Y
    end.

receive_exp() ->
    receive
        X -> X
    end.

receive_timeout(Timeout, Default) ->
    receive
        X -> X
    after Timeout -> Default
    end.

anon_fun() ->
    fun (X) -> X end.

named_fun() ->
    fun Fun(X) -> X end.

fun_lit() ->
    fun anon_fun/0.

remote_fun_lit() ->
    fun terms1:anon_fun/0.

dyn_fun(M, F) ->
    fun M:F/0.

dyn_call1(M) ->
    M:start().

dyn_call2(M, F, X, Y) ->
    M:F(X, Y).

%%%% Patterns

match_pat(X = x, Y = x) -> ok.

wildcard_pat(_) -> ok.

tuple_pat({_, _}) -> ok.

nil_pat([]) -> ok.

cons_pat([X|Y]) -> {X, Y}.

lit_pat1(1) -> ok.
lit_pat2(1.0) -> ok.
lit_pat3($c) -> ok.

atom_pat(ok) -> nok.

string_pat("ok") -> ok.

bin_pat(<<>>) -> bin.

unop_pat(-1) -> 1.

binop_pat(3 + 5) -> 0.

record_pat(#test_rec{}) -> test_rec.

record_index_pat(#test_rec.id) -> test_rec.

map_pat(#{id := X}) -> id.

%%% Guards

var_guard(X, Y) when X -> Y.

tuple_guard(X, Y) when {X, Y} -> Y.

nil_guard(X, Y) when [] -> Y.

cons_guard(X, Y) when [X | Y] -> Y.

bin_guard(X, Y) when <<>> -> Y.

unop_guard(X, Y) when -X -> Y.

binop_guard(X, Y) when X + Y -> Y.

record_guard(X, Y)
    when #test_rec{} -> Y.

record_index_guard(X, Y)
    when #test_rec.id -> Y.

record_field_guard(X)
    when X#test_rec.id -> ok.

map_guard(X)
    when #{} -> ok.

map_update_guard(X)
    when X#{id := 3} -> ok.

call_guard(X)
    when is_number(X) -> ok.

remote_call_guard(X)
    when erlang:is_number(X) -> ok.

atom_guard(X)
    when ok -> ok.

string_guard(X)
    when "string" -> ok.

lit_guard(X)
    when $c -> ok.

list_concat(X, Y) -> X ++ Y.

list_subtract(X, Y) -> X -- Y.

string_prefix("prefix" ++ S) -> S.
