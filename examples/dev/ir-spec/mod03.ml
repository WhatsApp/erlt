type unit0 = Unit0

and 'tA boxed = Boxed of 'tA

and ('tA, 'tB) either = Left of 'tA | Right of 'tB

and rgb = R | G | B

and ('tA, 'tB) pair = Pair of 'tA * 'tB

and ('tA, 'tB, 'tC) triple = Triple of 'tA * 'tB * 'tC

and 'tA my_list = Cons of 'tA * 'tA my_list | Nil

and 'tA option = None | Some of 'tA

let rec mk_unit'0 : unit -> unit0 = function () -> Unit0

let rec mk_box'1 : 'tA -> 'tA boxed = function v_A -> Boxed v_A

let rec mk_left'1 : 'tA -> ('tA, _) either = function v_A -> Left v_A

let rec mk_right'1 : 'tB -> (_, 'tB) either = function v_B -> Right v_B

let rec zero'2 : unit0 * 'tV -> 'tV = function Unit0, v_Val -> v_Val

let rec unbox'1 : 'tE boxed -> 'tE = function
  | v_Boxed -> ( match v_Boxed with Boxed v_Elem -> v_Elem )

let rec un_either'1 : ('tA, 'tA) either -> 'tA = function
  | v_Either -> (
      match v_Either with Left v_Elem -> v_Elem | Right v_Elem -> v_Elem )

let rec un_pair'1 : ('tA, 'tB) pair -> 'tA * 'tB = function
  | v_Pair -> ( match v_Pair with Pair (v_A, v_B) -> (v_A, v_B) )

let rec first'1 : ('tA, _) pair -> 'tA = function Pair (v_F, _) -> v_F

let rec second'1 : (_, 'tB) pair -> 'tB = function Pair (_, v_S) -> v_S
