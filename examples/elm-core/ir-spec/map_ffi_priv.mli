type ('tK, 'tV) map'2 = 'tK * 'tV

val empty'0 : unit -> ('t_K, 't_V) map'2

val get'2 : 'tK * ('tK, 'tV) map'2 -> 'tV Maybe.maybe'1

val size'1 : ('t_K, 't_V) map'2 -> int

val insert'3 : 'tK * 'tV * ('tK, 'tV) map'2 -> ('tK, 'tV) map'2

val remove'2 : 'tK * ('tK, 'tV) map'2 -> ('tK, 'tV) map'2

val fold'3 : ('tK * 'tV * 'tR -> 'tR) * 'tR * ('tK, 'tV) map'2 -> 'tR

val union'2 : ('tK, 'tV) map'2 * ('tK, 'tV) map'2 -> ('tK, 'tV) map'2

val filter'2 : ('tK * 'tV -> bool) * ('tK, 'tV) map'2 -> ('tK, 'tV) map'2

val map'2 : ('tK * 'tA -> 'tB) * ('tK, 'tA) map'2 -> ('tK, 'tB) map'2

val keys'1 : ('tK, 't_V) map'2 -> 'tK list

val values'1 : ('t_K, 'tV) map'2 -> 'tV list

val to_list'1 : ('tK, 'tV) map'2 -> ('tK * 'tV) list

val from_list'1 : ('tK * 'tV) list -> ('tK, 'tV) map'2
