val pair'2 : 'A * 'B -> 'A * 'B

val pair'1 : 'A -> 'B -> 'A * 'B

val first'1 : 'A * _ -> 'A

val second'1 : _ * 'B -> 'B

val map_first'2 : ('A -> 'X) * ('A * 'B) -> 'X * 'B

val map_first'1 : ('A -> 'X) -> 'A * 'B -> 'X * 'B

val map_second'2 : ('B -> 'Y) * ('A * 'B) -> 'A * 'Y

val map_second'1 : ('B -> 'Y) -> 'A * 'B -> 'A * 'Y

val map_both'3 : ('A -> 'X) * ('B -> 'Y) * ('A * 'B) -> 'X * 'Y

val map_both'2 : ('A -> 'X) * ('B -> 'Y) -> 'A * 'B -> 'X * 'Y
