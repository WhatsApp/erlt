type ('tK, 'tV) map'2 = ('tK, 'tV) Map_ffi.map'2

let rec empty'0 : unit -> ('t_K, 't_V) map'2 = function
  | () -> Map_ffi.empty'0 ()

let rec get'2 : 'tK * ('tK, 'tV) map'2 -> 'tV Maybe.maybe'1 = function
  | v_Key, v_Map -> Map_ffi.get'2 (v_Key, v_Map)

let rec member'2 : 'tK * ('tK, 't_V) map'2 -> bool = function
  | v_Key, v_Map -> (
      match get'2 (v_Key, v_Map) with
      | Maybe'Just _ -> true
      | Maybe'Nothing -> false )

let rec size'1 : ('t_K, 't_V) map'2 -> int = function
  | v_Map -> Map_ffi.size'1 v_Map

let rec is_empty'1 : ('t_K, 't_V) map'2 -> bool = function
  | v_Map -> size'1 v_Map = 0

let rec insert'3 : 'tK * 'tV * ('tK, 'tV) map'2 -> ('tK, 'tV) map'2 = function
  | v_Key, v_Value, v_Map -> Map_ffi.insert'3 (v_Key, v_Value, v_Map)

let rec remove'2 : 'tK * ('tK, 'tV) map'2 -> ('tK, 'tV) map'2 = function
  | v_Key, v_Map -> Map_ffi.remove'2 (v_Key, v_Map)

let rec update'3 :
    'tK * ('tV Maybe.maybe'1 -> 'tV Maybe.maybe'1) * ('tK, 'tV) map'2 ->
    ('tK, 'tV) map'2 = function
  | v_Key, v_Alter, v_Map -> (
      match v_Alter (get'2 (v_Key, v_Map)) with
      | Maybe'Just v_Value -> insert'3 (v_Key, v_Value, v_Map)
      | Maybe'Nothing -> remove'2 (v_Key, v_Map) )

let rec singleton'2 : 'tK * 'tV -> ('tK, 'tV) map'2 = function
  | v_Key, v_Value -> insert'3 (v_Key, v_Value, empty'0 ())

let rec union'2 : ('tK, 'tV) map'2 * ('tK, 'tV) map'2 -> ('tK, 'tV) map'2 =
  function
  | v_Map1, v_Map2 -> Map_ffi.union'2 (v_Map1, v_Map2)

let rec fold'3 : ('tK * 'tV * 'tR -> 'tR) * 'tR * ('tK, 'tV) map'2 -> 'tR =
  function
  | v_F, v_Acc, v_Map -> Map_ffi.fold'3 (v_F, v_Acc, v_Map)

let rec map'2 : ('tK * 'tA -> 'tB) * ('tK, 'tA) map'2 -> ('tK, 'tB) map'2 =
  function
  | v_F, v_Map -> Map_ffi.map'2 (v_F, v_Map)

let rec filter'2 : ('tK * 'tV -> bool) * ('tK, 'tV) map'2 -> ('tK, 'tV) map'2 =
  function
  | v_F, v_Map -> Map_ffi.filter'2 (v_F, v_Map)

let rec intersect'2 : ('tK, 'tV) map'2 * ('tK, 'tV) map'2 -> ('tK, 'tV) map'2 =
  function
  | v_Map1, v_Map2 ->
      filter'2 ((function v_K, _ -> member'2 (v_K, v_Map2)), v_Map1)

let rec diff'2 : ('tK, 'tV) map'2 * ('tK, 'tV) map'2 -> ('tK, 'tV) map'2 =
  function
  | v_Map1, v_Map2 ->
      filter'2 ((function v_K, _ -> not (member'2 (v_K, v_Map2))), v_Map1)

let rec partition'2 :
    ('tK * 'tV -> bool) * ('tK, 'tV) map'2 ->
    ('tK, 'tV) map'2 * ('tK, 'tV) map'2 = function
  | v_F, v_Map ->
      let v_Add = function
        | v_K, v_V, (v_D1, v_D2) -> (
            match v_F (v_K, v_V) with
            | true -> (insert'3 (v_K, v_V, v_D1), v_D2)
            | false -> (v_D1, insert'3 (v_K, v_V, v_D2)) )
      in
      fold'3 (v_Add, (empty'0 (), empty'0 ()), v_Map)

let rec keys'1 : ('tK, 't_V) map'2 -> 'tK list = function
  | v_Map -> Map_ffi.keys'1 v_Map

let rec values'1 : ('t_K, 'tV) map'2 -> 'tV list = function
  | v_Map -> Map_ffi.values'1 v_Map

let rec to_list'1 : ('tK, 'tV) map'2 -> ('tK * 'tV) list = function
  | v_Map -> Map_ffi.to_list'1 v_Map

let rec from_list'1 : ('tK * 'tV) list -> ('tK, 'tV) map'2 = function
  | v_List -> Map_ffi.from_list'1 v_List
