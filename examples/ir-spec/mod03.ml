type unit0 = Unit0

and 'A boxed = Boxed of 'A

and ('A, 'B) either = Left of 'A | Right of 'B

and rgb = R | G | B

and ('A, 'B) pair = Pair of 'A * 'B

and ('A, 'B, 'C) triple = Triple of 'A * 'B * 'C

and 'A my_list = Cons of 'A * 'A my_list | Nil

and 'A option = None | Some of 'A

let rec mk_unit'0 : unit -> unit0 = function () -> Unit0

let rec mk_box'1 : 'A -> 'A boxed = function v_A -> Boxed v_A

let rec mk_left'1 : 'A -> ('A, _) either = function v_A -> Left v_A

let rec mk_right'1 : 'B -> (_, 'B) either = function v_B -> Right v_B

let rec zero'2 : unit0 * 'V -> 'V = function Unit0, v_Val -> v_Val

let rec unbox'1 : 'E boxed -> 'E = function
  | v_Boxed -> ( match v_Boxed with Boxed v_Elem -> v_Elem )

let rec un_either'1 : ('A, 'A) either -> 'A = function
  | v_Either -> (
      match v_Either with Left v_Elem -> v_Elem | Right v_Elem -> v_Elem )

let rec un_pair'1 : ('A, 'B) pair -> 'A * 'B = function
  | v_Pair -> ( match v_Pair with Pair (v_A, v_B) -> (v_A, v_B) )

let rec first'1 : ('A, _) pair -> 'A = function Pair (v_F, _) -> v_F

let rec second'1 : (_, 'B) pair -> 'B = function Pair (_, v_S) -> v_S
