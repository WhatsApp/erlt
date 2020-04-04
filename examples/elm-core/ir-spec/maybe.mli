type 'tA maybe = Just of 'tA | Nothing

val with_default'2 : 'tA * 'tA maybe -> 'tA

val with_default'1 : 'tA -> 'tA maybe -> 'tA

val map'2 : ('tA -> 'tB) * 'tA maybe -> 'tB maybe

val map'1 : ('tA -> 'tB) -> 'tA maybe -> 'tB maybe

val map2'3 : ('tA * 'tB -> 'tC) * 'tA maybe * 'tB maybe -> 'tC maybe

val map2'1 : ('tA * 'tB -> 'tC) -> 'tA maybe * 'tB maybe -> 'tC maybe

val and_then'2 : ('tA -> 'tB maybe) * 'tA maybe -> 'tB maybe

val and_then'1 : ('tA -> 'tB maybe) -> 'tA maybe -> 'tB maybe

val is_just'1 : _ maybe -> bool

val destruct'3 : 'tB * ('tA -> 'tB) * 'tA maybe -> 'tB
