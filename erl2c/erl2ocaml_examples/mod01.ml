let same''2 (x, y) = x == y

let rec id'1 = function v_X -> v_X

let rec arg13'3 = function v_X, v_Y, v_Z -> v_X

let rec arg23'3 = function v_X, v_Y, v_Z -> v_Y

let rec arg33'3 = function v_X, v_Y, v_Z -> v_Z

let rec mk_int'0 = function () -> 1

let rec mk_float'0 = function () -> 2

let rec mk_string'0 = function () -> "Erlang String"

let rec mk_char'0 = function () -> 'c'

let rec mk_nil'0 = function () -> []

let rec mk_cons'2 = function v_H, v_T -> v_H :: v_T

let rec mk_tuple'3 = function v_A, v_B, v_C -> (v_A, v_B, v_C)

let rec mk_tuple2'1 = function v_A -> (1, 2, 3, v_A)

let rec mk_map'2 = function
  | v_A, v_B ->
      object
        val val_a = v_A

        method get_a = val_a

        method set_a new_val_a = {<val_a = new_val_a>}

        val val_b = v_B

        method get_b = val_b

        method set_b new_val_b = {<val_b = new_val_b>}
      end

let rec update_map'3 = function
  | v_M, v_A, v_B ->
      let _ = same''2 (v_M, v_M#set_a (v_A, v_B)) in
      let _ = same''2 (v_M, v_M#set_b (v_B, v_A)) in
      (v_M#set_a (v_A, v_B))#set_b (v_B, v_A)

let rec access_map'1 = function v_M -> (v_M#get_id, v_M#get_location)

let rec mk_seq'0 = function
  | () ->
      let v_X = 1 in
      let v_Y = 2 in
      let _ = 3 in
      let _ = 4 in
      (v_X, v_Y)

let rec mk_seq'2 = function
  | v_X, v_Y ->
      let _ = 3 in
      let _ = 4 in
      (v_X, v_Y)

let rec is_empty'1 = function [] -> true | v_H :: v_T -> false

let rec is_empty2'1 = function [] -> true | _ -> false

let rec with_as'1 = function 1 as v_X -> v_X | _ as v_Y -> v_Y

let rec block'2 = function
  | v_X, v_Y ->
      let v_Z = v_X :: v_Y in
      (v_Z, v_X :: v_Y)

let rec is_empty_case'1 = function
  | v_L -> ( match v_L with [] -> true | _ -> false )

let rec both_empty'2 = function
  | v_L1, v_L2 -> (
      match v_L1 with
      | [] -> ( match v_L2 with [] -> true | _ -> false )
      | _ -> false )

let rec call'2 = function v_L1, v_L2 -> both_empty'2 (v_L1, v_L2)

let rec remote_call'1 = function v_L -> List.rev v_L

let rec fun_to_var'0 = function
  | () ->
      let v_F = function v_X, v_Y -> (v_X, v_Y) in
      v_F

let rec local_fun_to_var'2 = function
  | v_A, v_B ->
      let v_F = call'2 in
      v_F

let rec remote_fun_to_var'2 = function
  | v_A, v_B ->
      let v_F = List.rev in
      v_F

let rec local_n_fun'0 = function
  | () ->
      let v_F =
        let rec v_Local = function _ :: v_T -> v_Local v_T | [] -> [] in
        v_Local
      in
      v_F

let rec mod01F'1 = function v_X -> v_X

let rec unary_plus'1 = function v_X -> + (+v_X)

let rec unary_minus'1 = function v_X -> - (-v_X)

let rec unary_not'1 = function v_X -> not (not v_X)

let rec unary_bnot'1 = function v_X -> lnot (lnot v_X)

let rec binary_star'2 = function v_X, v_Y -> v_X * v_Y

let rec binary_div'2 = function v_X, v_Y -> v_X / v_Y

let rec binary_rem'2 = function v_X, v_Y -> v_X mod v_Y

let rec binary_band'2 = function v_X, v_Y -> v_X land v_Y

let rec binary_and'2 = function v_X, v_Y -> v_X && v_Y

let rec binary_plus'2 = function v_X, v_Y -> v_X + v_Y

let rec binary_minus'2 = function v_X, v_Y -> v_X - v_Y

let rec binary_bor'2 = function v_X, v_Y -> v_X lor v_Y

let rec binary_bxor'2 = function v_X, v_Y -> v_X lxor v_Y

let rec binary_bsl'2 = function v_X, v_Y -> v_X lsl v_Y

let rec binary_bsr'2 = function v_X, v_Y -> v_X lsr v_Y

let rec binary_or'2 = function v_X, v_Y -> v_X || v_Y

let rec binary_xor'2 = function v_X, v_Y -> (not v_X) <> not v_Y

let rec binary_orelse'2 = function v_X, v_Y -> v_X || v_Y

let rec binary_andalso'2 = function v_X, v_Y -> v_X && v_Y

let rec list_plus'2 = function v_X, v_Y -> v_X @ v_Y

let rec list_minus'2 = function v_X, v_Y -> Ffi.list_diff'2 (v_X, v_Y)

let rec comp1'2 = function v_X, v_Y -> v_X = v_Y

let rec comp2'2 = function v_X, v_Y -> v_X <> v_Y

let rec comp3'2 = function v_X, v_Y -> v_X <= v_Y

let rec comp4'2 = function v_X, v_Y -> v_X < v_Y

let rec comp5'2 = function v_X, v_Y -> v_X >= v_Y

let rec comp6'2 = function v_X, v_Y -> v_X > v_Y

let rec comp7'2 = function v_X, v_Y -> v_X = v_Y

let rec comp8'2 = function v_X, v_Y -> v_X <> v_Y

let rec guard1'2 = function
  | v_X, v_Y when (v_X = 1 && v_Y = 1) || v_X <> v_Y -> true
  | v_X, v_Y when false -> false
  | v_X, v_Y -> false

let rec guard2'3 = function
  | v_X, v_Y, [] -> (
      match v_X with v_X1 :: v_Xs when v_X1 = v_Y -> v_Xs | v_Xs -> v_Xs )
  | v_X, _, v_Z when v_X = v_Z -> v_Z

let rec p_match_tuple0'1 = function () -> Ffi.Tuple1 ()

let rec p_match_tuple1'1 = function Ffi.Tuple1 v_X -> ()

let rec p_match_invoke'0 = function
  | () ->
      let _ = p_match_tuple0'1 () in
      p_match_tuple1'1 (Ffi.Tuple1 ())
