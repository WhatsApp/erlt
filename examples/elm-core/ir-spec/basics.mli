type order'0 = order

and order = Order'Lt | Order'Eq | Order'Gt

and never'0 = never

and never = Never'Just_one_more of never'0

val add'2 : int * int -> int

val add'1 : int -> int -> int

val sub'1 : int -> int -> int

val mul'2 : int * int -> int

val mul'1 : int -> int -> int

val idiv'1 : int -> int -> int

val eq'2 : 'tA * 'tA -> bool

val eq'1 : 'tA -> 'tA -> bool

val neq'2 : 'tA * 'tA -> bool

val neq'1 : 'tA -> 'tA -> bool

val lt'2 : 'tA * 'tA -> bool

val lt'1 : 'tA -> 'tA -> bool

val gt'2 : 'tA * 'tA -> bool

val gt'1 : 'tA -> 'tA -> bool

val le'2 : 'tA * 'tA -> bool

val le'1 : 'tA -> 'tA -> bool

val ge'2 : 'tA * 'tA -> bool

val ge'1 : 'tA -> 'tA -> bool

val min'2 : 'tA * 'tA -> 'tA

val min'1 : 'tA -> 'tA -> 'tA

val max'2 : 'tA * 'tA -> 'tA

val max'1 : 'tA -> 'tA -> 'tA

val compare'2 : 'tA * 'tA -> order'0

val compare'1 : 'tA -> 'tA -> order'0

val bool_not'1 : bool -> bool

val bool_and'2 : bool * bool -> bool

val bool_and'1 : bool -> bool -> bool

val bool_or'2 : bool * bool -> bool

val bool_or'1 : bool -> bool -> bool

val bool_xor'2 : bool * bool -> bool

val bool_xor'1 : bool -> bool -> bool

val mod_by'2 : int * int -> int

val mod_by'1 : int -> int -> int

val remainder_by'2 : int * int -> int

val remainder_by'1 : int -> int -> int

val negate'1 : int -> int

val abs'1 : int -> int

val clamp'3 : int * int * int -> int

val composeL'2 : ('tB -> 'tC) * ('tA -> 'tB) -> 'tA -> 'tC

val composeL'1 : ('tB -> 'tC) -> ('tA -> 'tB) -> 'tA -> 'tC

val composeR'2 : ('tA -> 'tB) * ('tB -> 'tC) -> 'tA -> 'tC

val composeR'1 : ('tA -> 'tB) -> ('tB -> 'tC) -> 'tA -> 'tC

val apR'2 : 'tA * ('tA -> 'tB) -> 'tB

val apR'1 : 'tA -> ('tA -> 'tB) -> 'tB

val apL'2 : ('tA -> 'tB) * 'tA -> 'tB

val apL'1 : ('tA -> 'tB) -> 'tA -> 'tB

val identity'1 : 'tA -> 'tA

val always'2 : 'tA * _ -> 'tA

val always'1 : 'tA -> _ -> 'tA

val never'1 : never'0 -> _
