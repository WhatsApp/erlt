val singleton'1 : 'tA -> 'tA list

val repeat'2 : int * 'tA -> 'tA list

val range'2 : int * int -> int list

val range_help'3 : int * int * int list -> int list

val cons'2 : 'tA * 'tA list -> 'tA list

val map'2 : ('tA -> 'tB) * 'tA list -> 'tB list

val indexed_map'2 : (int * 'tA -> 'tB) * 'tA list -> 'tB list

val foldl'3 : ('tA * 'tB -> 'tB) * 'tB * 'tA list -> 'tB

val foldr'3 : ('tA * 'tB -> 'tB) * 'tB * 'tA list -> 'tB

val filter'2 : ('tA -> bool) * 'tA list -> 'tA list

val filter_map'2 : ('tA -> 'tB Maybe.maybe'1) * 'tA list -> 'tB list

val maybe_cons'3 : ('tA -> 'tB Maybe.maybe'1) * 'tA * 'tB list -> 'tB list

val maybe_cons'1 : ('tA -> 'tB Maybe.maybe'1) -> 'tA * 'tB list -> 'tB list

val length'1 : _ list -> int

val reverse'1 : 'tA list -> 'tA list

val member'2 : 'tA * 'tA list -> bool

val all'2 : ('tA -> bool) * 'tA list -> bool

val any'2 : ('tA -> bool) * 'tA list -> bool

val maximum'1 : 'tA list -> 'tA Maybe.maybe'1

val minimum'1 : 'tA list -> 'tA Maybe.maybe'1

val sum'1 : int list -> int

val product'1 : int list -> int

val append'2 : 'tA list * 'tA list -> 'tA list

val concat'1 : 'tA list list -> 'tA list

val concat_map'2 : ('tA -> 'tB list) * 'tA list -> 'tB list

val intersperse'2 : 'tA * 'tA list -> 'tA list

val map2'3 : ('tA * 'tB -> 'tRes) * 'tA list * 'tB list -> 'tRes list

val is_empty'1 : _ list -> bool

val head'1 : 'tA list -> 'tA Maybe.maybe'1

val tail'1 : 'tA list -> 'tA list Maybe.maybe'1

val take'2 : int * 'tA list -> 'tA list

val drop'2 : int * 'tA list -> 'tA list

val partition'2 : ('tA -> bool) * 'tA list -> 'tA list * 'tA list

val unzip'1 : ('tA * 'tB) list -> 'tA list * 'tB list
