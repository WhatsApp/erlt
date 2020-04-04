type ('tError, 'tValue) result = Ok of 'tValue | Err of 'tError

let rec with_default'2 : 'tA * (_, 'tA) result -> 'tA = function
  | v__Def, Ok v_A -> v_A
  | v_Def, Err v__E -> v_Def

let rec with_default'1 : 'tA -> (_, 'tA) result -> 'tA = function
  | v_Def -> ( function v_Res -> with_default'2 (v_Def, v_Res) )

let rec map'2 : ('tA -> 'tB) * ('tX, 'tA) result -> ('tX, 'tB) result = function
  | v_Func, Ok v_A -> Ok (v_Func v_A)
  | v__Func, Err v_E -> Err v_E

let rec map'1 : ('tA -> 'tB) -> ('tX, 'tA) result -> ('tX, 'tB) result =
  function
  | v_Func -> ( function v_Res -> map'2 (v_Func, v_Res) )

let rec map2'3 :
    ('tA * 'tB -> 'tC) * ('tX, 'tA) result * ('tX, 'tB) result ->
    ('tX, 'tC) result = function
  | v__Func, Err v_X, _ -> Err v_X
  | v__Func, Ok _, Err v_X -> Err v_X
  | v_Func, Ok v_A, Ok v_B -> Ok (v_Func (v_A, v_B))

let rec map2'1 :
    ('tA * 'tB -> 'tC) ->
    ('tX, 'tA) result * ('tX, 'tB) result ->
    ('tX, 'tC) result = function
  | v_Func -> ( function v_ResA, v_ResB -> map2'3 (v_Func, v_ResA, v_ResB) )

let rec and_then'2 :
    ('tA -> ('tX, 'tB) result) * ('tX, 'tA) result -> ('tX, 'tB) result =
  function
  | v_Callback, Ok v_Value -> v_Callback v_Value
  | v__Callback, Err v_Msg -> Err v_Msg

let rec and_then'1 :
    ('tA -> ('tX, 'tB) result) -> ('tX, 'tA) result -> ('tX, 'tB) result =
  function
  | v_Callback -> ( function v_Res -> and_then'2 (v_Callback, v_Res) )

let rec map_error'2 : ('tX -> 'tY) * ('tX, 'tA) result -> ('tY, 'tA) result =
  function
  | v__F, Ok v_V -> Ok v_V
  | v_F, Err v_E -> Err (v_F v_E)

let rec map_error'1 : ('tX -> 'tY) -> ('tX, 'tA) result -> ('tY, 'tA) result =
  function
  | v_F -> ( function v_Res -> map_error'2 (v_F, v_Res) )

let rec to_maybe'1 : (_, 'tA) result -> 'tA Maybe.maybe = function
  | Ok v_V -> Maybe.Just v_V
  | Err _ -> Maybe.Nothing

let rec from_maybe'2 : 'tX * 'tA Maybe.maybe -> ('tX, 'tA) result = function
  | v__Err, Maybe.Just v_V -> Ok v_V
  | v_Err, Maybe.Nothing -> Err v_Err

let rec from_maybe'1 : 'tX -> 'tA Maybe.maybe -> ('tX, 'tA) result = function
  | v_Err -> ( function v_Maybe -> from_maybe'2 (v_Err, v_Maybe) )

let rec is_ok'1 : (_, _) result -> bool = function
  | Ok _ -> true
  | Err _ -> false
