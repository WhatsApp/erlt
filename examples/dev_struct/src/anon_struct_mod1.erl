-lang([erl2]).

-module(anon_struct_mod1).

-export([test/0]).

test() ->
    [test(X) || X <- [#(),
                      #(a=1, b=2),
                      #(a=1, b=2)#(),
                      #()#(a=1,b=#(c=2,d=3)),
                      #(a=1,b=2)#(a)
                     ]
    ].

-spec test(#(a::1, b::#(c::2 | _) | _)) -> really_ok;
          (#(_)) -> ok;
          (any()) -> not_ok.


test(#(a=1,b=#(c=2))) -> really_ok;
test(#(a=1)) -> ok;
test(#()) -> ok;
test(_) -> not_ok.
