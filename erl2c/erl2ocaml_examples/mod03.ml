type 'A boxed = Boxed of 'A

and ('A, 'B) either = Left of 'A | Right of 'B

and rgb = R | G | B

and ('A, 'B) pair = Pair of 'A * 'B

and ('A, 'B, 'C) triple = Triple of 'A * 'B * 'C

and 'A list = Cons of 'A * 'A list | Nil

and 'A option = None | Some of 'A
