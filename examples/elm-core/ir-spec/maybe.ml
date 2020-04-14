type 'tA maybe'1 = 'tA maybe

and 'tA maybe = Maybe'Just of 'tA | Maybe'Nothing

let rec with_default'2 : 'tA * 'tA maybe'1 -> 'tA = function
  | v__Default, Maybe'Just v_Value -> v_Value
  | v_Default, Maybe'Nothing -> v_Default

let rec with_default'1 : 'tA -> 'tA maybe'1 -> 'tA = function
  | v_Default -> ( function v_Maybe -> with_default'2 (v_Default, v_Maybe) )

let rec map'2 : ('tA -> 'tB) * 'tA maybe'1 -> 'tB maybe'1 = function
  | v_F, Maybe'Just v_Value -> Maybe'Just (v_F v_Value)
  | v__F, Maybe'Nothing -> Maybe'Nothing

let rec map'1 : ('tA -> 'tB) -> 'tA maybe'1 -> 'tB maybe'1 = function
  | v_F -> ( function v_Maybe -> map'2 (v_F, v_Maybe) )

let rec map2'3 : ('tA * 'tB -> 'tC) * 'tA maybe'1 * 'tB maybe'1 -> 'tC maybe'1 =
  function
  | v_F, Maybe'Just v_A, Maybe'Just v_B -> Maybe'Just (v_F (v_A, v_B))
  | v__F, v__Ma, v__Mb -> Maybe'Nothing

let rec map2'1 : ('tA * 'tB -> 'tC) -> 'tA maybe'1 * 'tB maybe'1 -> 'tC maybe'1
    = function
  | v_F -> ( function v_Ma, v_Mb -> map2'3 (v_F, v_Ma, v_Mb) )

let rec and_then'2 : ('tA -> 'tB maybe'1) * 'tA maybe'1 -> 'tB maybe'1 =
  function
  | v_Callback, Maybe'Just v_Value -> v_Callback v_Value
  | v__Callback, Maybe'Nothing -> Maybe'Nothing

let rec and_then'1 : ('tA -> 'tB maybe'1) -> 'tA maybe'1 -> 'tB maybe'1 =
  function
  | v_Callback -> ( function v_Ma -> and_then'2 (v_Callback, v_Ma) )

let rec is_just'1 : _ maybe'1 -> bool = function
  | Maybe'Just _ -> true
  | Maybe'Nothing -> false

let rec destruct'3 : 'tB * ('tA -> 'tB) * 'tA maybe'1 -> 'tB = function
  | v__Default, v_Func, Maybe'Just v_A -> v_Func v_A
  | v_Default, v__Func, Maybe'Nothing -> v_Default
