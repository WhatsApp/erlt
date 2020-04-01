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

let rec zero'2 = function Unit0, v_Val -> v_Val

let rec unbox'1 = function
  | v_Boxed -> ( match v_Boxed with Boxed v_Elem -> v_Elem )

let rec un_either'1 = function
  | v_Either -> (
      match v_Either with Left v_Elem -> v_Elem | Right v_Elem -> v_Elem )

let rec un_pair'1 = function
  | v_Pair -> ( match v_Pair with Pair (v_A, v_B) -> (v_A, v_B) )

let rec first'1 = function Pair (v_F, _) -> v_F

let rec second'1 = function Pair (_, v_S) -> v_S
