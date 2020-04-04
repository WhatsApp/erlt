val pair'2 : 'tA * 'tB -> 'tA * 'tB

val pair'1 : 'tA -> 'tB -> 'tA * 'tB

val first'1 : 'tA * _ -> 'tA

val second'1 : _ * 'tB -> 'tB

val map_first'2 : ('tA -> 'tX) * ('tA * 'tB) -> 'tX * 'tB

val map_first'1 : ('tA -> 'tX) -> 'tA * 'tB -> 'tX * 'tB

val map_second'2 : ('tB -> 'tY) * ('tA * 'tB) -> 'tA * 'tY

val map_second'1 : ('tB -> 'tY) -> 'tA * 'tB -> 'tA * 'tY

val map_both'3 : ('tA -> 'tX) * ('tB -> 'tY) * ('tA * 'tB) -> 'tX * 'tY

val map_both'2 : ('tA -> 'tX) * ('tB -> 'tY) -> 'tA * 'tB -> 'tX * 'tY
