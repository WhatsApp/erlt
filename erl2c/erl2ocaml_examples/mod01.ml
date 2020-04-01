type 'A my_pair2 = ('A, 'A) my_pair

and ('A, 'B) my_pair = 'A * 'B

let rec id'1 : 'X -> 'X = function v_X -> v_X

let rec arg13'3 : 'X * _ * _ -> 'X = function v_X, v_Y, v_Z -> v_X

let rec arg23'3 : _ * 'Y * _ -> 'Y = function v_X, v_Y, v_Z -> v_Y

let rec arg33'3 : _ * _ * 'Z -> 'Z = function v_X, v_Y, v_Z -> v_Z

let rec mk_int'0 : unit -> int = function () -> 1

let rec mk_float'0 : unit -> int = function () -> 2

let rec mk_string'0 : unit -> string = function () -> "Erlang String"

let rec mk_char'0 : unit -> char = function () -> 'c'

let rec mk_nil'0 : unit -> _ list = function () -> []

let rec mk_cons'2 : 'A * 'A list -> 'A list = function v_H, v_T -> v_H :: v_T

let rec mk_tuple'3 : 'A * 'B * 'C -> 'A * 'B * 'C = function
  | v_A, v_B, v_C -> (v_A, v_B, v_C)

let rec mk_tuple2'1 : 'A -> int * int * int * 'A = function
  | v_A -> (1, 2, 3, v_A)

let rec mk_map'2 :
    'A * 'B ->
    (< get_a : 'A ; set_a : 'A -> 'map_1 ; get_b : 'B ; set_b : 'B -> 'map_1 >
     as
     'map_1) = function
  | v_A, v_B ->
      object
        val val_a = v_A

        method get_a = val_a

        method set_a new_val_a = {<val_a = new_val_a>}

        val val_b = v_B

        method get_b = val_b

        method set_b new_val_b = {<val_b = new_val_b>}
      end

let rec update_map1'2 :
    (< get_a : 'A ; set_a : 'A -> 'map_2 > as 'map_2) * 'A ->
    (< get_a : 'A ; set_a : 'A -> 'map_3 > as 'map_3) = function
  | v_M, v_A ->
      let _ = Ffi.same'2 (v_M, v_M#set_a v_A) in
      let _ = Ffi.same'2 (v_A, v_M#get_a) in
      v_M#set_a v_A

let rec update_map2'2 :
    (< get_a : 'A ; set_a : 'A -> 'map_M ; .. > as 'map_M) * 'A ->
    (< get_a : 'A ; set_a : 'A -> 'map_M ; .. > as 'map_M) = function
  | v_M, v_A ->
      let _ = Ffi.same'2 (v_M, v_M#set_a v_A) in
      let _ = Ffi.same'2 (v_A, v_M#get_a) in
      v_M#set_a v_A

let rec update_map3'1 :
    (< get_a : 'A ; set_a : 'A -> 'map_4 ; get_b : 'A ; set_b : 'A -> 'map_4 >
     as
     'map_4) ->
    (< get_a : 'A ; set_a : 'A -> 'map_5 ; get_b : 'A ; set_b : 'A -> 'map_5 >
     as
     'map_5) = function
  | v_M ->
      let v_A0 = v_M#get_a in
      let v_B0 = v_M#get_a in
      let _ = Ffi.same'2 (v_M, v_M#set_a v_B0) in
      let _ = Ffi.same'2 (v_M, v_M#set_b v_A0) in
      let _ = Ffi.same'2 (v_B0, v_M#get_a) in
      let _ = Ffi.same'2 (v_A0, v_M#get_b) in
      (v_M#set_a v_B0)#set_b v_A0

let rec update_map4'1 :
    (< get_a : 'A
     ; set_a : 'A -> 'map_M
     ; get_b : 'A
     ; set_b : 'A -> 'map_M
     ; .. >
     as
     'map_M) ->
    (< get_a : 'A
     ; set_a : 'A -> 'map_M
     ; get_b : 'A
     ; set_b : 'A -> 'map_M
     ; .. >
     as
     'map_M) = function
  | v_M ->
      let v_A0 = v_M#get_a in
      let v_B0 = v_M#get_a in
      let _ = Ffi.same'2 (v_M, v_M#set_a v_B0) in
      let _ = Ffi.same'2 (v_M, v_M#set_b v_A0) in
      let _ = Ffi.same'2 (v_B0, v_M#get_a) in
      let _ = Ffi.same'2 (v_A0, v_M#get_b) in
      (v_M#set_a v_B0)#set_b v_A0

let rec access_map'1 :
    (< get_id : 'Id
     ; set_id : 'Id -> 'map__
     ; get_location : 'Location
     ; set_location : 'Location -> 'map__
     ; .. >
     as
     'map__) ->
    'Id * 'Location = function
  | v_M -> (v_M#get_id, v_M#get_location)

let rec access_map2'1 :
    (< get_inner1 : (< get_inner2 : 'A ; set_inner2 : 'A -> 'map_7 > as 'map_7)
     ; set_inner1 :
         (< get_inner2 : 'A ; set_inner2 : 'A -> 'map_7 > as 'map_7) -> 'map_6 >
     as
     'map_6) ->
    'A = function
  | v_M -> v_M#get_inner1#get_inner2

let rec mk_seq'0 : unit -> int * int = function
  | () ->
      let v_X = 1 in
      let v_Y = 2 in
      let _ = 3 in
      let _ = 4 in
      (v_X, v_Y)

let rec is_empty'1 : _ list -> bool = function
  | [] -> true
  | v_H :: v_T -> false

let rec is_empty2'1 : _ list -> bool = function [] -> true | _ -> false

let rec with_as'1 : int -> int = function 1 as v_X -> v_X | _ as v_Y -> v_Y

let rec block'2 : 'A * 'A list -> 'A list * 'A list = function
  | v_X, v_Y ->
      let v_Z = v_X :: v_Y in
      (v_Z, v_X :: v_Y)

let rec is_empty_case'1 : _ list -> bool = function
  | v_L -> ( match v_L with [] -> true | _ -> false )

let rec both_empty'2 : _ list * _ list -> bool = function
  | v_L1, v_L2 -> (
      match v_L1 with
      | [] -> ( match v_L2 with [] -> true | _ -> false )
      | _ -> false )

let rec call'2 : _ list * _ list -> bool = function
  | v_L1, v_L2 -> both_empty'2 (v_L1, v_L2)

let rec remote_call'1 : 'A list -> 'A list = function v_L -> List.rev v_L

let rec fun_to_var'0 : unit -> 'A * 'B -> 'A * 'B = function
  | () ->
      let v_F = function v_X, v_Y -> (v_X, v_Y) in
      v_F

let rec local_fun_to_var'2 : _ * _ -> _ list * _ list -> bool = function
  | v_A, v_B ->
      let v_F = call'2 in
      v_F

let rec remote_fun_to_var'2 : _ * _ -> 'A list -> 'A list = function
  | v_A, v_B ->
      let v_F = List.rev in
      v_F

let rec local_n_fun'0 : unit -> _ list -> _ list = function
  | () ->
      let v_F =
        let rec v_Local = function _ :: v_T -> v_Local v_T | [] -> [] in
        v_Local
      in
      v_F

let rec mod01F'1 : 'X -> 'X = function v_X -> v_X

let rec unary_plus'1 : int -> int = function v_X -> + (+v_X)

let rec unary_minus'1 : int -> int = function v_X -> - (-v_X)

let rec unary_not'1 : bool -> bool = function v_X -> not (not v_X)

let rec unary_bnot'1 : int -> int = function v_X -> lnot (lnot v_X)

let rec binary_star'2 : int * int -> int = function v_X, v_Y -> v_X * v_Y

let rec binary_div'2 : int * int -> int = function v_X, v_Y -> v_X / v_Y

let rec binary_rem'2 : int * int -> int = function v_X, v_Y -> v_X mod v_Y

let rec binary_band'2 : int * int -> int = function v_X, v_Y -> v_X land v_Y

let rec binary_and'2 : bool * bool -> bool = function v_X, v_Y -> v_X && v_Y

let rec binary_plus'2 : int * int -> int = function v_X, v_Y -> v_X + v_Y

let rec binary_minus'2 : int * int -> int = function v_X, v_Y -> v_X - v_Y

let rec binary_bor'2 : int * int -> int = function v_X, v_Y -> v_X lor v_Y

let rec binary_bxor'2 : int * int -> int = function v_X, v_Y -> v_X lxor v_Y

let rec binary_bsl'2 : int * int -> int = function v_X, v_Y -> v_X lsl v_Y

let rec binary_bsr'2 : int * int -> int = function v_X, v_Y -> v_X lsr v_Y

let rec binary_or'2 : bool * bool -> bool = function v_X, v_Y -> v_X || v_Y

let rec binary_xor'2 : bool * bool -> bool = function
  | v_X, v_Y -> (not v_X) <> not v_Y

let rec binary_orelse'2 : bool * bool -> bool = function
  | v_X, v_Y -> v_X || v_Y

let rec binary_andalso'2 : bool * bool -> bool = function
  | v_X, v_Y -> v_X && v_Y

let rec list_plus'2 : 'A list * 'A list -> 'A list = function
  | v_X, v_Y -> v_X @ v_Y

let rec list_minus'2 : 'A list * 'A list -> 'A list = function
  | v_X, v_Y -> Ffi.list_diff'2 (v_X, v_Y)

let rec comp1'2 : 'A * 'A -> bool = function v_X, v_Y -> v_X = v_Y

let rec comp2'2 : 'A * 'A -> bool = function v_X, v_Y -> v_X <> v_Y

let rec comp3'2 : 'A * 'A -> bool = function v_X, v_Y -> v_X <= v_Y

let rec comp4'2 : 'A * 'A -> bool = function v_X, v_Y -> v_X < v_Y

let rec comp5'2 : 'A * 'A -> bool = function v_X, v_Y -> v_X >= v_Y

let rec comp6'2 : 'A * 'A -> bool = function v_X, v_Y -> v_X > v_Y

let rec comp7'2 : 'A * 'A -> bool = function v_X, v_Y -> v_X = v_Y

let rec comp8'2 : 'A * 'A -> bool = function v_X, v_Y -> v_X <> v_Y

let rec guard1'2 : int * int -> bool = function
  | v_X, v_Y when (v_X = 1 && v_Y = 1) || v_X <> v_Y -> true
  | v_X, v_Y when false -> false
  | v_X, v_Y -> false

let rec guard2'3 : 'A list * 'A * 'A list -> 'A list = function
  | v_X, v_Y, [] -> (
      match v_X with v_X1 :: v_Xs when v_X1 = v_Y -> v_Xs | v_Xs -> v_Xs )
  | v_X, _, v_Z when v_X = v_Z -> v_Z
  | _, _, v_Z -> v_Z

let rec p_match_tuple0'1 : unit -> unit Ffi.tuple1 = function
  | () -> Ffi.Tuple1 ()

let rec p_match_tuple1'1 : _ Ffi.tuple1 -> unit = function
  | Ffi.Tuple1 v_X -> ()

let rec p_match_invoke'0 : unit -> unit = function
  | () ->
      let _ = p_match_tuple0'1 () in
      p_match_tuple1'1 (Ffi.Tuple1 ())

let rec any_id'1 : Ffi.any -> Ffi.any = function v_A -> v_A

let rec atom_id'1 : Ffi.atom -> Ffi.atom = function v_A -> v_A

let rec binary_id'1 : Ffi.binary -> Ffi.binary = function v_A -> v_A

let rec bitstring_id'1 : Ffi.bitstring -> Ffi.bitstring = function v_A -> v_A

let rec byte_id'1 : Ffi.byte -> Ffi.byte = function v_A -> v_A

let rec float_id'1 : float -> float = function v_X -> v_X

let rec identifier_id'1 : Ffi.identifier -> Ffi.identifier = function
  | v_A -> v_A

let rec iodata_id'1 : Ffi.iodata -> Ffi.iodata = function v_A -> v_A

let rec iolist_id'1 : Ffi.iolist -> Ffi.iolist = function v_A -> v_A

let rec map_id'1 : ('A, 'B) Ffi.map -> ('A, 'B) Ffi.map = function v_M -> v_M

let rec none_id'1 : Ffi.none -> Ffi.none = function v_A -> v_A

let rec noreturn_id'1 : Ffi.no_return -> Ffi.no_return = function
  | v_A -> none_id'1 v_A

let rec number_id'1 : Ffi.none -> Ffi.none = function v_A -> v_A

let rec pid_id'1 : Ffi.pid -> Ffi.pid = function v_A -> v_A

let rec port_id'1 : Ffi.port -> Ffi.port = function v_A -> v_A

let rec reference_id'1 : Ffi.reference -> Ffi.reference = function v_A -> v_A

let rec term_id'1 : Ffi.term -> Ffi.term = function v_A -> v_A

let rec timeout_id'1 : Ffi.timeout -> Ffi.timeout = function v_A -> v_A

let rec ints_id'1 :
    Ffi.neg_integer * Ffi.non_neg_integer * Ffi.pos_integer ->
    Ffi.neg_integer * Ffi.non_neg_integer * Ffi.pos_integer = function
  | v_X -> v_X

let rec mk_my_pair'2 = function v_A, v_B -> (v_A, v_B)
