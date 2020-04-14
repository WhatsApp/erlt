type unit0'0 = unit0

and unit0 = Unit0

and 'tA boxed'1 = 'tA boxed

and 'tA boxed = Boxed of 'tA

and ('tA, 'tB) either'2 = ('tA, 'tB) either

and ('tA, 'tB) either = Left of 'tA | Right of 'tB

and rgb'0 = rgb

and rgb = R | G | B

and ('tA, 'tB) pair'2 = ('tA, 'tB) pair

and ('tA, 'tB) pair = Pair of 'tA * 'tB

and ('tA, 'tB, 'tC) triple'3 = ('tA, 'tB, 'tC) triple

and ('tA, 'tB, 'tC) triple = Triple of 'tA * 'tB * 'tC

and 'tA my_list'1 = 'tA my_list

and 'tA my_list = Cons of 'tA * 'tA my_list'1 | Nil

and 'tA option'1 = 'tA option

and 'tA option = None | Some of 'tA

let rec mk_rgb'0 : unit -> rgb'0 = function () -> R

let rec mk_triple'1 : 'tA -> ('tA, 'tA, 'tA) triple'3 = function
  | v_A -> Triple (v_A, v_A, v_A)

let rec mk_none'0 : unit -> _ option'1 = function () -> None

let rec mk_unit'0 : unit -> unit0'0 = function () -> Unit0

let rec mk_box'1 : 'tA -> 'tA boxed'1 = function v_A -> Boxed v_A

let rec mk_left'1 : 'tA -> ('tA, _) either'2 = function v_A -> Left v_A

let rec mk_right'1 : 'tB -> (_, 'tB) either'2 = function v_B -> Right v_B

let rec zero'2 : unit0'0 * 'tV -> 'tV = function Unit0, v_Val -> v_Val

let rec unbox'1 : 'tE boxed'1 -> 'tE = function
  | v_Boxed -> ( match v_Boxed with Boxed v_Elem -> v_Elem )

let rec un_either'1 : ('tA, 'tA) either'2 -> 'tA = function
  | v_Either -> (
      match v_Either with Left v_Elem -> v_Elem | Right v_Elem -> v_Elem )

let rec un_pair'1 : ('tA, 'tB) pair'2 -> 'tA * 'tB = function
  | v_Pair -> ( match v_Pair with Pair (v_A, v_B) -> (v_A, v_B) )

let rec first'1 : ('tA, _) pair'2 -> 'tA = function Pair (v_F, _) -> v_F

let rec second'1 : (_, 'tB) pair'2 -> 'tB = function Pair (_, v_S) -> v_S
