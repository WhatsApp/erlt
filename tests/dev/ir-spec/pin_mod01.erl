-file("dev/src/pin_mod01.erlt", 1).

-module(pin_mod01).

-eqwalizer_unchecked([{f, 1},
                      {g, 1},
                      {h, 1},
                      {i, 3},
                      {i2, 4},
                      {j, 2},
                      {j2, 1},
                      {k, 2},
                      {m, 2},
                      {p, 2},
                      {q, 3}]).

-export([f/1,
         g/1,
         h/1,
         i/3,
         i2/4,
         j/2,
         j2/1,
         k/2,
         m/2,
         p/2,
         q/3]).

f(X) ->
    Y = 42,
    case X of
        Y -> {that, Y};
        Z -> {this, Z}
    end.

g(X) ->
    Y = 42,
    case X of
        {foo, [Y | Zs]} -> {that, Y, Zs};
        {foo, [Z | Zs]} -> {this, Z, Zs};
        Z -> {other, Z}
    end.

h(X) ->
    Y = 42,
    {ok, Y} = X,
    true.

i(Elem, List1, List2) ->
    Foo = 42,
    begin
        __pin_o0 = Elem,
        __pin_o1 = Foo,
        [found
         || __pin_i0 <- List1, __pin_i0 =:= __pin_o0,
            {foo, __pin_i1} <- List2, __pin_i1 =:= __pin_o1]
    end.

i2(Elem, List1, List2, List3) ->
    begin
        __pin_o2 = Elem,
        [Elem
         || __pin_i2 <- List1, __pin_i2 =:= __pin_o2,
            {foo, __pin_i3} <- List2, __pin_i3 =:= __pin_o2,
            {bar, __pin_i4} <- List3, __pin_i4 =:= __pin_o2]
    end.

j(Elem, List) ->
    Foo = 42,
    lists:filter(begin
                     __pin_o5 = Elem,
                     __pin_o6 = Foo,
                     fun (__pin_i5) when __pin_i5 =:= __pin_o5 -> true;
                         ({foo, __pin_i6}) when __pin_i6 =:= __pin_o6 -> true;
                         (_) -> false
                     end
                 end,
                 List).

j2(X) ->
    {begin
         __pin_o7 = X,
         fun Test(__pin_i7) when __pin_i7 =:= __pin_o7 -> Test(X)
         end
     end,
     ok}.

k(X, Y) ->
    case X of
        {a, X} -> {b, X};
        {a, Y} when X < Y; Y > X -> {b, Y};
        {b, Y} ->
            Z = {X, Y},
            case Y of
                {c, X} -> {d, X};
                {c, Y} when X < Y; Y > X -> {d, Y};
                {d, X} -> {d, Y};
                {d, Z} -> {z, Z}
            end
    end.

m(X, Y) ->
    begin
        __pin_o8 = X,
        __pin_o9 = Y,
        fun ({foo, __pin_i8, Z}) when __pin_i8 =:= __pin_o8 ->
                Z;
            ({bar, __pin_i8, Z}) when __pin_i8 =:= __pin_o8 ->
                case X of
                    {ok, X} -> {ok, Z};
                    error -> error
                end;
            ({baz, __pin_i9, Z}) when __pin_i9 =:= __pin_o9 -> Z
        end
    end.

p(F, Class) ->
    Term = {x, 42},
    try F() catch Class:Term -> "oops 🍌" end.

q(Bin, Size, B) ->
    case Bin of <<B:Size/binary>> -> ok end.



