type ('tError, 'tValue) result'2 = Ok of 'tValue | Err of 'tError

val with_default'2 : 'tA * (_, 'tA) result'2 -> 'tA

val with_default'1 : 'tA -> (_, 'tA) result'2 -> 'tA

val map'2 : ('tA -> 'tB) * ('tX, 'tA) result'2 -> ('tX, 'tB) result'2

val map'1 : ('tA -> 'tB) -> ('tX, 'tA) result'2 -> ('tX, 'tB) result'2

val map2'3 :
  ('tA * 'tB -> 'tC) * ('tX, 'tA) result'2 * ('tX, 'tB) result'2 ->
  ('tX, 'tC) result'2

val map2'1 :
  ('tA * 'tB -> 'tC) ->
  ('tX, 'tA) result'2 * ('tX, 'tB) result'2 ->
  ('tX, 'tC) result'2

val and_then'2 :
  ('tA -> ('tX, 'tB) result'2) * ('tX, 'tA) result'2 -> ('tX, 'tB) result'2

val and_then'1 :
  ('tA -> ('tX, 'tB) result'2) -> ('tX, 'tA) result'2 -> ('tX, 'tB) result'2

val map_error'2 : ('tX -> 'tY) * ('tX, 'tA) result'2 -> ('tY, 'tA) result'2

val map_error'1 : ('tX -> 'tY) -> ('tX, 'tA) result'2 -> ('tY, 'tA) result'2

val to_maybe'1 : (_, 'tA) result'2 -> 'tA Maybe.maybe'1

val from_maybe'2 : 'tX * 'tA Maybe.maybe'1 -> ('tX, 'tA) result'2

val from_maybe'1 : 'tX -> 'tA Maybe.maybe'1 -> ('tX, 'tA) result'2

val is_ok'1 : (_, _) result'2 -> bool
