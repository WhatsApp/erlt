type ('K, 'V) dict = ('K, 'V) Ffi.map

val empty'0 : unit -> (_, _) dict

val get'2 : 'K * ('K, 'V) dict -> 'V Maybe.maybe

val size'1 : (_, _) dict -> int

val insert'3 : 'K * 'V * ('K, 'V) dict -> ('K, 'V) dict

val remove'2 : 'K * ('K, 'V) dict -> ('K, 'V) dict

val fold'3 : ('K * 'V * 'R -> 'R) * 'R * ('K, 'V) dict -> 'R

val union'2 : ('K, 'V) dict * ('K, 'V) dict -> ('K, 'V) dict

val filter'2 : ('K * 'V -> bool) * ('K, 'V) dict -> ('K, 'V) dict

val map'2 : ('K * 'A -> 'B) * ('K, 'A) dict -> ('K, 'B) dict

val keys'1 : ('K, _) dict -> 'K list

val values'1 : (_, 'V) dict -> 'V list

val to_list'1 : ('K, 'V) dict -> ('K * 'V) list

val from_list'1 : ('K * 'V) list -> ('K, 'V) dict
