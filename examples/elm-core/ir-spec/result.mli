type ('tError, 'tValue) result = Ok of 'tValue | Err of 'tError

val with_default'2 : 'tA * (_, 'tA) result -> 'tA

val with_default'1 : 'tA -> (_, 'tA) result -> 'tA

val map'2 : ('tA -> 'tB) * ('tX, 'tA) result -> ('tX, 'tB) result

val map'1 : ('tA -> 'tB) -> ('tX, 'tA) result -> ('tX, 'tB) result

val map2'3 :
  ('tA * 'tB -> 'tC) * ('tX, 'tA) result * ('tX, 'tB) result ->
  ('tX, 'tC) result

val map2'1 :
  ('tA * 'tB -> 'tC) ->
  ('tX, 'tA) result * ('tX, 'tB) result ->
  ('tX, 'tC) result

val and_then'2 :
  ('tA -> ('tX, 'tB) result) * ('tX, 'tA) result -> ('tX, 'tB) result

val and_then'1 :
  ('tA -> ('tX, 'tB) result) -> ('tX, 'tA) result -> ('tX, 'tB) result

val map_error'2 : ('tX -> 'tY) * ('tX, 'tA) result -> ('tY, 'tA) result

val map_error'1 : ('tX -> 'tY) -> ('tX, 'tA) result -> ('tY, 'tA) result

val to_maybe'1 : (_, 'tA) result -> 'tA Maybe.maybe

val from_maybe'2 : 'tX * 'tA Maybe.maybe -> ('tX, 'tA) result

val from_maybe'1 : 'tX -> 'tA Maybe.maybe -> ('tX, 'tA) result

val is_ok'1 : (_, _) result -> bool
