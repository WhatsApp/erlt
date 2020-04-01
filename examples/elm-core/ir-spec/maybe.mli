type 'A maybe = Just of 'A | Nothing

val with_default'2 : 'A * 'A maybe -> 'A

val with_default'1 : 'A -> 'A maybe -> 'A

val map'2 : ('A -> 'B) * 'A maybe -> 'B maybe

val map'1 : ('A -> 'B) -> 'A maybe -> 'B maybe

val map2'3 : ('A * 'B -> 'C) * 'A maybe * 'B maybe -> 'C maybe

val map2'1 : ('A * 'B -> 'C) -> 'A maybe * 'B maybe -> 'C maybe

val and_then'2 : ('A -> 'B maybe) * 'A maybe -> 'B maybe

val and_then'1 : ('A -> 'B maybe) -> 'A maybe -> 'B maybe

val is_just'1 : _ maybe -> bool

val destruct'3 : 'B * ('A -> 'B) * 'A maybe -> 'B
