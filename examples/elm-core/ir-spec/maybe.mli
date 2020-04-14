type 'tA maybe'1 = 'tA maybe

and 'tA maybe = Maybe'Just of 'tA | Maybe'Nothing

val with_default'2 : 'tA * 'tA maybe'1 -> 'tA

val with_default'1 : 'tA -> 'tA maybe'1 -> 'tA

val map'2 : ('tA -> 'tB) * 'tA maybe'1 -> 'tB maybe'1

val map'1 : ('tA -> 'tB) -> 'tA maybe'1 -> 'tB maybe'1

val map2'3 : ('tA * 'tB -> 'tC) * 'tA maybe'1 * 'tB maybe'1 -> 'tC maybe'1

val map2'1 : ('tA * 'tB -> 'tC) -> 'tA maybe'1 * 'tB maybe'1 -> 'tC maybe'1

val and_then'2 : ('tA -> 'tB maybe'1) * 'tA maybe'1 -> 'tB maybe'1

val and_then'1 : ('tA -> 'tB maybe'1) -> 'tA maybe'1 -> 'tB maybe'1

val is_just'1 : _ maybe'1 -> bool

val destruct'3 : 'tB * ('tA -> 'tB) * 'tA maybe'1 -> 'tB
