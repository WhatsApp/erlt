module Mod02_priv : sig
  val odd'1 : int -> bool

  val id'1 : 'tX -> 'tX

  val id_caller'1 : 'tX -> 'tX

  val id_rec'1 : 'tX -> 'tX

  val even'1 : int -> bool

  val mod01call'1 : 'tX -> 'tX

  val list_to_string'1 : _ list -> string

  val int_to_string'1 : int -> string
end = struct
  let rec odd'1 : int -> bool = function v_X -> even'1 (v_X - 1)

  and even'1 : int -> bool = function 0 -> true | v_X -> odd'1 (v_X - 1)

  let rec id'1 : 'tX -> 'tX = function v_X -> v_X

  let rec id_caller'1 : 'tX -> 'tX = function v_X -> id'1 v_X

  let rec id_rec'1 : 'tX -> 'tX = function v_X -> id_rec'1 v_X

  let rec mod01call'1 : 'tX -> 'tX = function v_X -> Mod01.mod01F'1 v_X

  let rec list_to_string'1 : _ list -> string = function
    | [] -> Ffi.to_string'1 []
    | v_X -> Ffi.to_string'1 v_X

  let rec int_to_string'1 : int -> string = function
    | 0 -> Ffi.to_string'1 0
    | v_N -> Ffi.to_string'1 v_N
end
