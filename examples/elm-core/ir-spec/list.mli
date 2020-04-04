val singleton'1 : 'A -> 'A list

val repeat'2 : int * 'A -> 'A list

val range'2 : int * int -> int list

val range_help'3 : int * int * int list -> int list

val cons'2 : 'A * 'A list -> 'A list

val map'2 : ('A -> 'B) * 'A list -> 'B list

val indexed_map'2 : (int * 'A -> 'B) * 'A list -> 'B list

val foldl'3 : ('A * 'B -> 'B) * 'B * 'A list -> 'B

val foldr'3 : ('A * 'B -> 'B) * 'B * 'A list -> 'B

val filter'2 : ('A -> bool) * 'A list -> 'A list

val filter_map'2 : ('A -> 'B Maybe.maybe) * 'A list -> 'B list

val maybe_cons'3 : ('A -> 'B Maybe.maybe) * 'A * 'B list -> 'B list

val maybe_cons'1 : ('A -> 'B Maybe.maybe) -> 'A * 'B list -> 'B list

val length'1 : _ list -> int

val reverse'1 : 'A list -> 'A list

val member'2 : 'A * 'A list -> bool

val all'2 : ('A -> bool) * 'A list -> bool

val any'2 : ('A -> bool) * 'A list -> bool

val maximum'1 : 'A list -> 'A Maybe.maybe

val minimum'1 : 'A list -> 'A Maybe.maybe

val sum'1 : int list -> int

val product'1 : int list -> int

val append'2 : 'A list * 'A list -> 'A list

val concat'1 : 'A list list -> 'A list

val concat_map'2 : ('A -> 'B list) * 'A list -> 'B list

val intersperse'2 : 'A * 'A list -> 'A list

val map2'3 : ('A * 'B -> 'Res) * 'A list * 'B list -> 'Res list

val is_empty'1 : _ list -> bool

val head'1 : 'A list -> 'A Maybe.maybe

val tail'1 : 'A list -> 'A list Maybe.maybe

val take'2 : int * 'A list -> 'A list

val drop'2 : int * 'A list -> 'A list

val partition'2 : ('A -> bool) * 'A list -> 'A list * 'A list

val unzip'1 : ('A * 'B) list -> 'A list * 'B list
