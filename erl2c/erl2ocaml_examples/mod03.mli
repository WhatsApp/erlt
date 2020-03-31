type unit0 = Unit0

and 'A boxed = Boxed of 'A

and ('A, 'B) either = Left of 'A | Right of 'B

and rgb = R | G | B

and ('A, 'B) pair = Pair of 'A * 'B

and ('A, 'B, 'C) triple = Triple of 'A * 'B * 'C

and 'A list = Cons of 'A * 'A list | Nil

and 'A option = None | Some of 'A

val mk_unit'0 : unit -> unit0

val mk_box'1 : 'A -> 'A boxed

val mk_left'1 : 'A -> ('A, _) either

val mk_right'1 : 'B -> (_, 'B) either
