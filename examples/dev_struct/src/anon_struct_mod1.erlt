-lang([erl2]).

-module(anon_struct_mod1).

-export([test/0, test_in_lc/1]).

test() ->
    [test(X) || X <- [#(),
                      #(a=1, b=2),
                      #(a=1, b=2)#(),
                      #(a=1)#(b=2)#(),
                      #()#(a=1,b=#(c=2,d=3)),
                      #(a=1,b=2)#(a),
                      #(a=1)#(b=2)#(a),
                      #()#(a=1,b=#(c=2,d=3))#(b)#(c)#(e=4)  
                     ]
    ].

-spec test(#(a::1, b::#(c::2 | _) | _)) -> really_ok;
          (#(_)) -> ok;
          (any()) -> not_ok.

test(A) when A =:= #(a=1,b=#(c=2)) -> closed_ok;
test(#(a=1,b=#(c=2))) -> open_ok;
test(A) when A#(b) =:= 2 -> ok;
test(A) when A =:= #(a=1)#(b=2) -> ok;
test(#(a=1)) -> ok;
test(#()) -> ok;
test(_) -> not_ok.

test_in_lc(List) ->
    [Id || #(id = Id) <- List ].