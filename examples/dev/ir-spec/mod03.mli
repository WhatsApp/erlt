type unit0'0 = unit0

and unit0 = Unit0'Unit0

and 'tA boxed'1 = 'tA boxed

and 'tA boxed = Boxed'Boxed of 'tA

and ('tA, 'tB) either'2 = ('tA, 'tB) either

and ('tA, 'tB) either = Either'Left of 'tA | Either'Right of 'tB

and rgb'0 = rgb

and rgb = Rgb'R | Rgb'G | Rgb'B

and ('tA, 'tB) pair'2 = ('tA, 'tB) pair

and ('tA, 'tB) pair = Pair'Pair of 'tA * 'tB

and ('tA, 'tB, 'tC) triple'3 = ('tA, 'tB, 'tC) triple

and ('tA, 'tB, 'tC) triple = Triple'Triple of 'tA * 'tB * 'tC

and 'tA my_list'1 = 'tA my_list

and 'tA my_list = My_list'Cons of 'tA * 'tA my_list'1 | My_list'Nil

and 'tA option'1 = 'tA option

and 'tA option = Option'None | Option'Some of 'tA

val mk_rgb'0 : unit -> rgb'0

val mk_triple'1 : 'tA -> ('tA, 'tA, 'tA) triple'3

val mk_none'0 : unit -> _ option'1

val mk_unit'0 : unit -> unit0'0

val mk_box'1 : 'tA -> 'tA boxed'1

val mk_left'1 : 'tA -> ('tA, _) either'2

val mk_right'1 : 'tB -> (_, 'tB) either'2

val zero'2 : unit0'0 * 'tV -> 'tV

val unbox'1 : 'tE boxed'1 -> 'tE

val un_either'1 : ('tA, 'tA) either'2 -> 'tA

val un_pair'1 : ('tA, 'tB) pair'2 -> 'tA * 'tB

val first'1 : ('tA, _) pair'2 -> 'tA

val second'1 : (_, 'tB) pair'2 -> 'tB

val call_this_mod'1 : ('tA, 'tB) pair'2 -> ('tA, 'tB) pair'2
