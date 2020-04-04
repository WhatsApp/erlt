type ('tK, 'tV) dict = ('tK, 'tV) Ffi.map

val empty'0 : unit -> (_, _) dict

val get'2 : 'tK * ('tK, 'tV) dict -> 'tV Maybe.maybe

val size'1 : (_, _) dict -> int

val insert'3 : 'tK * 'tV * ('tK, 'tV) dict -> ('tK, 'tV) dict

val remove'2 : 'tK * ('tK, 'tV) dict -> ('tK, 'tV) dict

val fold'3 : ('tK * 'tV * 'tR -> 'tR) * 'tR * ('tK, 'tV) dict -> 'tR

val union'2 : ('tK, 'tV) dict * ('tK, 'tV) dict -> ('tK, 'tV) dict

val filter'2 : ('tK * 'tV -> bool) * ('tK, 'tV) dict -> ('tK, 'tV) dict

val map'2 : ('tK * 'tA -> 'tB) * ('tK, 'tA) dict -> ('tK, 'tB) dict

val keys'1 : ('tK, _) dict -> 'tK list

val values'1 : (_, 'tV) dict -> 'tV list

val to_list'1 : ('tK, 'tV) dict -> ('tK * 'tV) list

val from_list'1 : ('tK * 'tV) list -> ('tK, 'tV) dict
