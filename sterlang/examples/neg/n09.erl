-lang(st).
-module(n09).
test() ->
    Rec1 = #(n = 1),
    Rec2 = Rec1 #(n = "String"),
    1.
