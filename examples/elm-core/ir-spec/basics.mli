type order = Lt | Eq | Gt

and never = Just_one_more of never

val add'2 : int * int -> int

val add'1 : int -> int -> int

val sub'2 : int * int -> int

val sub'1 : int -> int -> int

val mul'2 : int * int -> int

val mul'1 : int -> int -> int

val idiv'2 : int * int -> int

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

val compare'2 : 'tA * 'tA -> order

val compare'1 : 'tA -> 'tA -> order

val not'1 : bool -> bool

val and'2 : bool * bool -> bool

val and'1 : bool -> bool -> bool

val or'2 : bool * bool -> bool

val or'1 : bool -> bool -> bool

val xor'2 : bool * bool -> bool

val xor'1 : bool -> bool -> bool

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

val apR'2 : 'tA * ('tA -> 'tB) -> 'tB

val apR'1 : 'tA -> ('tA -> 'tB) -> 'tB

val apL'2 : ('tA -> 'tB) * 'tA -> 'tB

val apL'1 : ('tA -> 'tB) -> 'tA -> 'tB

val identity'1 : 'tA -> 'tA

val always'2 : 'tA * _ -> 'tA

val always'1 : 'tA -> _ -> 'tA

val never'1 : never -> _
