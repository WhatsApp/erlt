type unit0 = Unit0

and 'A boxed = Boxed of 'A

and ('A, 'B) either = Left of 'A | Right of 'B

and rgb = R | G | B

and ('A, 'B) pair = Pair of 'A * 'B

and ('A, 'B, 'C) triple = Triple of 'A * 'B * 'C

and 'A list = Cons of 'A * 'A list | Nil

and 'A option = None | Some of 'A

let rec mk_unit'0 = function () -> Unit0

let rec mk_box'1 = function v_A -> Boxed v_A

let rec mk_left'1 = function v_A -> Left v_A

let rec mk_right'1 = function v_B -> Right v_B
