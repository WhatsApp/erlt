type ('tK, 'tV) dict = ('tK, 'tV) Dict_ffi.dict

let rec empty'0 : unit -> ('t_K, 't_V) dict = function
  | () -> Dict_ffi.empty'0 ()

let rec get'2 : 'tK * ('tK, 'tV) dict -> 'tV Maybe.maybe = function
  | v_Key, v_Dict -> Dict_ffi.get'2 (v_Key, v_Dict)

let rec member'2 : 'tK * ('tK, 't_V) dict -> bool = function
  | v_Key, v_Dict -> (
      match get'2 (v_Key, v_Dict) with
      | Maybe.Just _ -> true
      | Maybe.Nothing -> false )

let rec size'1 : ('t_K, 't_V) dict -> int = function
  | v_Dict -> Dict_ffi.size'1 v_Dict

let rec is_empty'1 : ('t_K, 't_V) dict -> bool = function
  | v_Dict -> size'1 v_Dict = 0

let rec insert'3 : 'tK * 'tV * ('tK, 'tV) dict -> ('tK, 'tV) dict = function
  | v_Key, v_Value, v_Dict -> Dict_ffi.insert'3 (v_Key, v_Value, v_Dict)

let rec remove'2 : 'tK * ('tK, 'tV) dict -> ('tK, 'tV) dict = function
  | v_Key, v_Dict -> Dict_ffi.remove'2 (v_Key, v_Dict)

let rec update'3 :
    'tK * ('tV Maybe.maybe -> 'tV Maybe.maybe) * ('tK, 'tV) dict ->
    ('tK, 'tV) dict = function
  | v_Key, v_Alter, v_Dict -> (
      match v_Alter (get'2 (v_Key, v_Dict)) with
      | Maybe.Just v_Value -> insert'3 (v_Key, v_Value, v_Dict)
      | Maybe.Nothing -> remove'2 (v_Key, v_Dict) )

let rec singleton'2 : 'tK * 'tV -> ('tK, 'tV) dict = function
  | v_Key, v_Value -> insert'3 (v_Key, v_Value, empty'0 ())

let rec union'2 : ('tK, 'tV) dict * ('tK, 'tV) dict -> ('tK, 'tV) dict =
  function
  | v_Dict1, v_Dict2 -> Dict_ffi.union'2 (v_Dict1, v_Dict2)

let rec fold'3 : ('tK * 'tV * 'tR -> 'tR) * 'tR * ('tK, 'tV) dict -> 'tR =
  function
  | v_F, v_Acc, v_Dict -> Dict_ffi.fold'3 (v_F, v_Acc, v_Dict)

let rec map'2 : ('tK * 'tA -> 'tB) * ('tK, 'tA) dict -> ('tK, 'tB) dict =
  function
  | v_F, v_Dict -> Dict_ffi.map'2 (v_F, v_Dict)

let rec filter'2 : ('tK * 'tV -> bool) * ('tK, 'tV) dict -> ('tK, 'tV) dict =
  function
  | v_F, v_Dict -> Dict_ffi.filter'2 (v_F, v_Dict)

let rec intersect'2 : ('tK, 'tV) dict * ('tK, 'tV) dict -> ('tK, 'tV) dict =
  function
  | v_Dict1, v_Dict2 ->
      filter'2 ((function v_K, _ -> member'2 (v_K, v_Dict2)), v_Dict1)

let rec diff'2 : ('tK, 'tV) dict * ('tK, 'tV) dict -> ('tK, 'tV) dict = function
  | v_Dict1, v_Dict2 ->
      filter'2 ((function v_K, _ -> not (member'2 (v_K, v_Dict2))), v_Dict1)

let rec partition'2 :
    ('tK * 'tV -> bool) * ('tK, 'tV) dict -> ('tK, 'tV) dict * ('tK, 'tV) dict =
  function
  | v_F, v_Dict ->
      let v_Add = function
        | v_K, v_V, (v_D1, v_D2) -> (
            match v_F (v_K, v_V) with
            | true -> (insert'3 (v_K, v_V, v_D1), v_D2)
            | false -> (v_D1, insert'3 (v_K, v_V, v_D2)) )
      in
      fold'3 (v_Add, (empty'0 (), empty'0 ()), v_Dict)

let rec keys'1 : ('tK, 't_V) dict -> 'tK list = function
  | v_Dict -> Dict_ffi.keys'1 v_Dict

let rec values'1 : ('t_K, 'tV) dict -> 'tV list = function
  | v_Dict -> Dict_ffi.values'1 v_Dict

let rec to_list'1 : ('tK, 'tV) dict -> ('tK * 'tV) list = function
  | v_Dict -> Dict_ffi.to_list'1 v_Dict

let rec from_list'1 : ('tK * 'tV) list -> ('tK, 'tV) dict = function
  | v_List -> Dict_ffi.from_list'1 v_List
