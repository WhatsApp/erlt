type ('Error, 'Value) result = Ok of 'Value | Err of 'Error

val with_default'2 : 'A * (_, 'A) result -> 'A

val with_default'1 : 'A -> (_, 'A) result -> 'A

val map'2 : ('A -> 'B) * ('X, 'A) result -> ('X, 'B) result

val map'1 : ('A -> 'B) -> ('X, 'A) result -> ('X, 'B) result

val map2'3 :
  ('A * 'B -> 'C) * ('X, 'A) result * ('X, 'B) result -> ('X, 'C) result

val map2'1 :
  ('A * 'B -> 'C) -> ('X, 'A) result * ('X, 'B) result -> ('X, 'C) result

val and_then'2 : ('A -> ('X, 'B) result) * ('X, 'A) result -> ('X, 'B) result

val and_then'1 : ('A -> ('X, 'B) result) -> ('X, 'A) result -> ('X, 'B) result

val map_error'2 : ('X -> 'Y) * ('X, 'A) result -> ('Y, 'A) result

val map_error'1 : ('X -> 'Y) -> ('X, 'A) result -> ('Y, 'A) result

val to_maybe'1 : (_, 'A) result -> 'A Maybe.maybe

val from_maybe'2 : 'X * 'A Maybe.maybe -> ('X, 'A) result

val from_maybe'1 : 'X -> 'A Maybe.maybe -> ('X, 'A) result

val is_ok'1 : (_, _) result -> bool
