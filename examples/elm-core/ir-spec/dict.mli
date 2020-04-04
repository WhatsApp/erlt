type ('tK, 'tV) dict = ('tK, 'tV) Dict_ffi.dict

val empty'0 : unit -> (_, _) dict

val get'2 : 'tK * ('tK, 'tV) dict -> 'tV Maybe.maybe

val member'2 : 'tK * ('tK, _) dict -> bool

val size'1 : (_, _) dict -> int

val is_empty'1 : (_, _) dict -> bool

val insert'3 : 'tK * 'tV * ('tK, 'tV) dict -> ('tK, 'tV) dict

val remove'2 : 'tK * ('tK, 'tV) dict -> ('tK, 'tV) dict

val update'3 :
  'tK * ('tV Maybe.maybe -> 'tV Maybe.maybe) * ('tK, 'tV) dict ->
  ('tK, 'tV) dict

val singleton'2 : 'tK * 'tV -> ('tK, 'tV) dict

val union'2 : ('tK, 'tV) dict * ('tK, 'tV) dict -> ('tK, 'tV) dict

val intersect'2 : ('tK, 'tV) dict * ('tK, 'tV) dict -> ('tK, 'tV) dict

val diff'2 : ('tK, 'tV) dict * ('tK, 'tV) dict -> ('tK, 'tV) dict

val fold'3 : ('tK * 'tV * 'tR -> 'tR) * 'tR * ('tK, 'tV) dict -> 'tR

val map'2 : ('tK * 'tA -> 'tB) * ('tK, 'tA) dict -> ('tK, 'tB) dict

val filter'2 : ('tK * 'tV -> bool) * ('tK, 'tV) dict -> ('tK, 'tV) dict

val partition'2 :
  ('tK * 'tV -> bool) * ('tK, 'tV) dict -> ('tK, 'tV) dict * ('tK, 'tV) dict

val keys'1 : ('tK, _) dict -> 'tK list

val values'1 : (_, 'tV) dict -> 'tV list

val to_list'1 : ('tK, 'tV) dict -> ('tK * 'tV) list

val from_list'1 : ('tK * 'tV) list -> ('tK, 'tV) dict
