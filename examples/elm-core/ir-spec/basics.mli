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

val eq'2 : 'A * 'A -> bool

val eq'1 : 'A -> 'A -> bool

val neq'2 : 'A * 'A -> bool

val neq'1 : 'A -> 'A -> bool

val lt'2 : 'A * 'A -> bool

val lt'1 : 'A -> 'A -> bool

val gt'2 : 'A * 'A -> bool

val gt'1 : 'A -> 'A -> bool

val le'2 : 'A * 'A -> bool

val le'1 : 'A -> 'A -> bool

val ge'2 : 'A * 'A -> bool

val ge'1 : 'A -> 'A -> bool

val min'2 : 'A * 'A -> 'A

val min'1 : 'A -> 'A -> 'A

val max'2 : 'A * 'A -> 'A

val max'1 : 'A -> 'A -> 'A

val compare'2 : 'A * 'A -> order

val compare'1 : 'A -> 'A -> order

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

val composeL'2 : ('B -> 'C) * ('A -> 'B) -> 'A -> 'C

val composeL'1 : ('B -> 'C) -> ('A -> 'B) -> 'A -> 'C

val composeR'2 : ('A -> 'B) * ('B -> 'C) -> 'A -> 'C

val apR'2 : 'A * ('A -> 'B) -> 'B

val apR'1 : 'A -> ('A -> 'B) -> 'B

val apL'2 : ('A -> 'B) * 'A -> 'B

val apL'1 : ('A -> 'B) -> 'A -> 'B

val identity'1 : 'A -> 'A

val always'2 : 'A * _ -> 'A

val always'1 : 'A -> _ -> 'A

val never'1 : never -> _
