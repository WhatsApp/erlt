type ('tK, 'tV) map = ('tK, 'tV) Ffi.map

val empty'0 : unit -> ('t_K, 't_V) map

val get'2 : 'tK * ('tK, 'tV) map -> 'tV Maybe.maybe

val size'1 : ('t_K, 't_V) map -> int

val insert'3 : 'tK * 'tV * ('tK, 'tV) map -> ('tK, 'tV) map

val remove'2 : 'tK * ('tK, 'tV) map -> ('tK, 'tV) map

val fold'3 : ('tK * 'tV * 'tR -> 'tR) * 'tR * ('tK, 'tV) map -> 'tR

val union'2 : ('tK, 'tV) map * ('tK, 'tV) map -> ('tK, 'tV) map

val filter'2 : ('tK * 'tV -> bool) * ('tK, 'tV) map -> ('tK, 'tV) map

val map'2 : ('tK * 'tA -> 'tB) * ('tK, 'tA) map -> ('tK, 'tB) map

val keys'1 : ('tK, 't_V) map -> 'tK list

val values'1 : ('t_K, 'tV) map -> 'tV list

val to_list'1 : ('tK, 'tV) map -> ('tK * 'tV) list

val from_list'1 : ('tK * 'tV) list -> ('tK, 'tV) map
