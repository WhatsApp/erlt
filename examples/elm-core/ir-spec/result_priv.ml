module Result_priv : sig
  type ('tError, 'tValue) result'2 = ('tError, 'tValue) result

  and ('tError, 'tValue) result = Result'Ok of 'tValue | Result'Err of 'tError

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
end = struct
  type ('tError, 'tValue) result'2 = ('tError, 'tValue) result

  and ('tError, 'tValue) result = Result'Ok of 'tValue | Result'Err of 'tError

  let rec with_default'2 : 'tA * (_, 'tA) result'2 -> 'tA = function
    | v__Def, Result'Ok v_A -> v_A
    | v_Def, Result'Err v__E -> v_Def

  let rec with_default'1 : 'tA -> (_, 'tA) result'2 -> 'tA = function
    | v_Def -> ( function v_Res -> with_default'2 (v_Def, v_Res) )

  let rec map'2 : ('tA -> 'tB) * ('tX, 'tA) result'2 -> ('tX, 'tB) result'2 =
    function
    | v_Func, Result'Ok v_A -> Result'Ok (v_Func v_A)
    | v__Func, Result'Err v_E -> Result'Err v_E

  let rec map'1 : ('tA -> 'tB) -> ('tX, 'tA) result'2 -> ('tX, 'tB) result'2 =
    function
    | v_Func -> ( function v_Res -> map'2 (v_Func, v_Res) )

  let rec map2'3 :
      ('tA * 'tB -> 'tC) * ('tX, 'tA) result'2 * ('tX, 'tB) result'2 ->
      ('tX, 'tC) result'2 = function
    | v__Func, Result'Err v_X, _ -> Result'Err v_X
    | v__Func, Result'Ok _, Result'Err v_X -> Result'Err v_X
    | v_Func, Result'Ok v_A, Result'Ok v_B -> Result'Ok (v_Func (v_A, v_B))

  let rec map2'1 :
      ('tA * 'tB -> 'tC) ->
      ('tX, 'tA) result'2 * ('tX, 'tB) result'2 ->
      ('tX, 'tC) result'2 = function
    | v_Func -> ( function v_ResA, v_ResB -> map2'3 (v_Func, v_ResA, v_ResB) )

  let rec and_then'2 :
      ('tA -> ('tX, 'tB) result'2) * ('tX, 'tA) result'2 -> ('tX, 'tB) result'2
      = function
    | v_Callback, Result'Ok v_Value -> v_Callback v_Value
    | v__Callback, Result'Err v_Msg -> Result'Err v_Msg

  let rec and_then'1 :
      ('tA -> ('tX, 'tB) result'2) -> ('tX, 'tA) result'2 -> ('tX, 'tB) result'2
      = function
    | v_Callback -> ( function v_Res -> and_then'2 (v_Callback, v_Res) )

  let rec map_error'2 :
      ('tX -> 'tY) * ('tX, 'tA) result'2 -> ('tY, 'tA) result'2 = function
    | v__F, Result'Ok v_V -> Result'Ok v_V
    | v_F, Result'Err v_E -> Result'Err (v_F v_E)

  let rec map_error'1 :
      ('tX -> 'tY) -> ('tX, 'tA) result'2 -> ('tY, 'tA) result'2 = function
    | v_F -> ( function v_Res -> map_error'2 (v_F, v_Res) )

  let rec to_maybe'1 : (_, 'tA) result'2 -> 'tA Maybe.maybe'1 = function
    | Result'Ok v_V -> Maybe.Maybe'Just v_V
    | Result'Err _ -> Maybe.Maybe'Nothing

  let rec from_maybe'2 : 'tX * 'tA Maybe.maybe'1 -> ('tX, 'tA) result'2 =
    function
    | v__Err, Maybe.Maybe'Just v_V -> Result'Ok v_V
    | v_Err, Maybe.Maybe'Nothing -> Result'Err v_Err

  let rec from_maybe'1 : 'tX -> 'tA Maybe.maybe'1 -> ('tX, 'tA) result'2 =
    function
    | v_Err -> ( function v_Maybe -> from_maybe'2 (v_Err, v_Maybe) )

  let rec is_ok'1 : (_, _) result'2 -> bool = function
    | Result'Ok _ -> true
    | Result'Err _ -> false
end
