val mk_unit_remote'0 : unit -> Mod03.unit0

val mk_left_remote'1 : 'A -> ('A, _) Mod03.either

val mk_right_remote'1 : 'B -> (_, 'B) Mod03.either

val zero_remote'2 : Mod03.unit0 * 'V -> 'V

val un_pair_remote'1 : ('A, 'B) Mod03.pair -> 'A * 'B
