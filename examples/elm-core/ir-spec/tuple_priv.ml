module Tuple_priv : sig
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
end = struct
  let rec pair'2 : 'tA * 'tB -> 'tA * 'tB = function v_A, v_B -> (v_A, v_B)

  let rec pair'1 : 'tA -> 'tB -> 'tA * 'tB = function
    | v_A -> ( function v_B -> pair'2 (v_A, v_B) )

  let rec first'1 : 'tA * _ -> 'tA = function v_A, _ -> v_A

  let rec second'1 : _ * 'tB -> 'tB = function _, v_B -> v_B

  let rec map_first'2 : ('tA -> 'tX) * ('tA * 'tB) -> 'tX * 'tB = function
    | v_F, (v_A, v_B) -> (v_F v_A, v_B)

  let rec map_first'1 : ('tA -> 'tX) -> 'tA * 'tB -> 'tX * 'tB = function
    | v_F -> ( function v_P -> map_first'2 (v_F, v_P) )

  let rec map_second'2 : ('tB -> 'tY) * ('tA * 'tB) -> 'tA * 'tY = function
    | v_F, (v_A, v_B) -> (v_A, v_F v_B)

  let rec map_second'1 : ('tB -> 'tY) -> 'tA * 'tB -> 'tA * 'tY = function
    | v_F -> ( function v_P -> map_second'2 (v_F, v_P) )

  let rec map_both'3 : ('tA -> 'tX) * ('tB -> 'tY) * ('tA * 'tB) -> 'tX * 'tY =
    function
    | v_Fa, v_Fb, (v_A, v_B) -> (v_Fa v_A, v_Fb v_B)

  let rec map_both'2 : ('tA -> 'tX) * ('tB -> 'tY) -> 'tA * 'tB -> 'tX * 'tY =
    function
    | v_Fa, v_Fb -> ( function v_P -> map_both'3 (v_Fa, v_Fb, v_P) )
end
