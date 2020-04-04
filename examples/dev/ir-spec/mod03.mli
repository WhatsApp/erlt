type unit0 = Unit0

and 'tA boxed = Boxed of 'tA

and ('tA, 'tB) either = Left of 'tA | Right of 'tB

and rgb = R | G | B

and ('tA, 'tB) pair = Pair of 'tA * 'tB

and ('tA, 'tB, 'tC) triple = Triple of 'tA * 'tB * 'tC

and 'tA my_list = Cons of 'tA * 'tA my_list | Nil

and 'tA option = None | Some of 'tA

val mk_rgb'0 : unit -> rgb

val mk_triple'1 : 'tA -> ('tA, 'tA, 'tA) triple

val mk_none'0 : unit -> _ option

val mk_unit'0 : unit -> unit0

val mk_box'1 : 'tA -> 'tA boxed

val mk_left'1 : 'tA -> ('tA, _) either

val mk_right'1 : 'tB -> (_, 'tB) either

val zero'2 : unit0 * 'tV -> 'tV

val unbox'1 : 'tE boxed -> 'tE

val un_either'1 : ('tA, 'tA) either -> 'tA

val un_pair'1 : ('tA, 'tB) pair -> 'tA * 'tB

val first'1 : ('tA, _) pair -> 'tA

val second'1 : (_, 'tB) pair -> 'tB
