type 'A maybe = Just of 'A | Nothing

let rec with_default'2 : 'A * 'A maybe -> 'A = function
  | v__Default, Just v_Value -> v_Value
  | v_Default, Nothing -> v_Default

let rec with_default'1 : 'A -> 'A maybe -> 'A = function
  | v_Default -> ( function v_Maybe -> with_default'2 (v_Default, v_Maybe) )

let rec map'2 : ('A -> 'B) * 'A maybe -> 'B maybe = function
  | v_F, Just v_Value -> Just (v_F v_Value)
  | v__F, Nothing -> Nothing

let rec map'1 : ('A -> 'B) -> 'A maybe -> 'B maybe = function
  | v_F -> ( function v_Maybe -> map'2 (v_F, v_Maybe) )

let rec map2'3 : ('A * 'B -> 'C) * 'A maybe * 'B maybe -> 'C maybe = function
  | v_F, Just v_A, Just v_B -> Just (v_F (v_A, v_B))
  | v__F, v__Ma, v__Mb -> Nothing

let rec map2'1 : ('A * 'B -> 'C) -> 'A maybe * 'B maybe -> 'C maybe = function
  | v_F -> ( function v_Ma, v_Mb -> map2'3 (v_F, v_Ma, v_Mb) )

let rec and_then'2 : ('A -> 'B maybe) * 'A maybe -> 'B maybe = function
  | v_Callback, Just v_Value -> v_Callback v_Value
  | v__Callback, Nothing -> Nothing

let rec and_then'1 : ('A -> 'B maybe) -> 'A maybe -> 'B maybe = function
  | v_Callback -> ( function v_Ma -> and_then'2 (v_Callback, v_Ma) )

let rec is_just'1 : _ maybe -> bool = function
  | Just _ -> true
  | Nothing -> false

let rec destruct'3 : 'B * ('A -> 'B) * 'A maybe -> 'B = function
  | v__Default, v_Func, Just v_A -> v_Func v_A
  | v_Default, v__Func, Nothing -> v_Default
