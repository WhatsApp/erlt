module Mod01_priv : sig
  type 'tA my_pair2'1 = ('tA, 'tA) my_pair'2

  and ('tA, 'tB) my_pair'2 = 'tA * 'tB

  val id'1 : 'tX -> 'tX

  val arg13'3 : 'tX * _ * _ -> 'tX

  val arg23'3 : _ * 'tY * _ -> 'tY

  val arg33'3 : _ * _ * 'tZ -> 'tZ

  val mk_int'0 : unit -> int

  val mk_float'0 : unit -> int

  val mk_string'0 : unit -> string

  val mk_char'0 : unit -> char

  val mk_nil'0 : unit -> _ list

  val mk_cons'2 : 'tA * 'tA list -> 'tA list

  val mk_tuple'3 : 'tA * 'tB * 'tC -> 'tA * 'tB * 'tC

  val mk_tuple2'1 : 'tA -> int * int * int * 'tA

  val mk_map'2 :
    'tA * 'tB ->
    (< get_a : 'tA
     ; set_a : 'tA -> 'rec_tv__1
     ; get_b : 'tB
     ; set_b : 'tB -> 'rec_tv__1 >
     as
     'rec_tv__1)

  val update_map1'2 :
    (< get_a : 'tA ; set_a : 'tA -> 'rec_tv__2 > as 'rec_tv__2) * 'tA ->
    (< get_a : 'tA ; set_a : 'tA -> 'rec_tv__3 > as 'rec_tv__3)

  val update_map3'1 :
    (< get_a : 'tA
     ; set_a : 'tA -> 'rec_tv__4
     ; get_b : 'tA
     ; set_b : 'tA -> 'rec_tv__4 >
     as
     'rec_tv__4) ->
    (< get_a : 'tA
     ; set_a : 'tA -> 'rec_tv__5
     ; get_b : 'tA
     ; set_b : 'tA -> 'rec_tv__5 >
     as
     'rec_tv__5)

  val access_map'1 :
    (< get_id : 'tId
     ; set_id : 'tId -> 'rec_tv__6
     ; get_location : 'tLocation
     ; set_location : 'tLocation -> 'rec_tv__6
     ; .. >
     as
     'rec_tv__6) ->
    'tId * 'tLocation

  val access_map2'1 :
    (< get_inner1 :
         (< get_inner2 : 'tA ; set_inner2 : 'tA -> 'rec_tv__8 > as 'rec_tv__8)
     ; set_inner1 :
         (< get_inner2 : 'tA ; set_inner2 : 'tA -> 'rec_tv__8 > as 'rec_tv__8) ->
         'rec_tv__7 >
     as
     'rec_tv__7) ->
    'tA

  val mk_seq'0 : unit -> int * int

  val is_empty'1 : _ list -> bool

  val is_empty2'1 : _ list -> bool

  val with_as'1 : int -> int

  val block'2 : 'tA * 'tA list -> 'tA list * 'tA list

  val is_empty_case'1 : _ list -> bool

  val both_empty'2 : _ list * _ list -> bool

  val call'2 : _ list * _ list -> bool

  val fun_to_var'0 : unit -> 'tA * 'tB -> 'tA * 'tB

  val local_fun_to_var'2 : _ * _ -> _ list * _ list -> bool

  val remote_fun_to_var'2 : _ * _ -> 'tA list -> 'tA list

  val local_n_fun'0 : unit -> _ list -> _ list

  val mod01F'1 : 'tX -> 'tX

  val unary_plus'1 : int -> int

  val unary_minus'1 : int -> int

  val unary_not'1 : bool -> bool

  val unary_bnot'1 : int -> int

  val binary_star'2 : int * int -> int

  val binary_div'2 : int * int -> int

  val binary_rem'2 : int * int -> int

  val binary_band'2 : int * int -> int

  val binary_and'2 : bool * bool -> bool

  val binary_plus'2 : int * int -> int

  val binary_minus'2 : int * int -> int

  val binary_bor'2 : int * int -> int

  val binary_bxor'2 : int * int -> int

  val binary_bsl'2 : int * int -> int

  val binary_bsr'2 : int * int -> int

  val binary_or'2 : bool * bool -> bool

  val binary_xor'2 : bool * bool -> bool

  val binary_orelse'2 : bool * bool -> bool

  val binary_andalso'2 : bool * bool -> bool

  val list_plus'2 : 'tA list * 'tA list -> 'tA list

  val list_minus'2 : 'tA list * 'tA list -> 'tA list

  val comp1'2 : 'tA * 'tA -> bool

  val comp2'2 : 'tA * 'tA -> bool

  val comp3'2 : 'tA * 'tA -> bool

  val comp4'2 : 'tA * 'tA -> bool

  val comp5'2 : 'tA * 'tA -> bool

  val comp6'2 : 'tA * 'tA -> bool

  val comp7'2 : 'tA * 'tA -> bool

  val comp8'2 : 'tA * 'tA -> bool

  val guard1'2 : int * int -> bool

  val guard2'3 : 'tA list * 'tA * 'tA list -> 'tA list

  val p_match_tuple0'1 : unit -> unit Ffi.tuple1'1

  val p_match_tuple1'1 : _ Ffi.tuple1'1 -> unit

  val p_match_invoke'0 : unit -> unit

  val any_id'1 : Ffi.any'0 -> Ffi.any'0

  val atom_id'1 : Ffi.atom'0 -> Ffi.atom'0

  val binary_id'1 : Ffi.binary'0 -> Ffi.binary'0

  val bitstring_id'1 : Ffi.bitstring'0 -> Ffi.bitstring'0

  val byte_id'1 : Ffi.byte'0 -> Ffi.byte'0

  val float_id'1 : float -> float

  val identifier_id'1 : Ffi.identifier'0 -> Ffi.identifier'0

  val iodata_id'1 : Ffi.iodata'0 -> Ffi.iodata'0

  val iolist_id'1 : Ffi.iolist'0 -> Ffi.iolist'0

  val map_id'1 : ('tA, 'tB) Ffi.map'2 -> ('tA, 'tB) Ffi.map'2

  val none_id'1 : Ffi.none'0 -> Ffi.none'0

  val noreturn_id'1 : Ffi.no_return'0 -> Ffi.no_return'0

  val number_id'1 : Ffi.none'0 -> Ffi.none'0

  val pid_id'1 : Ffi.pid'0 -> Ffi.pid'0

  val port_id'1 : Ffi.port'0 -> Ffi.port'0

  val reference_id'1 : Ffi.reference'0 -> Ffi.reference'0

  val term_id'1 : Ffi.term'0 -> Ffi.term'0

  val timeout_id'1 : Ffi.timeout'0 -> Ffi.timeout'0

  val ints_id'1 :
    Ffi.neg_integer'0 * Ffi.non_neg_integer'0 * Ffi.pos_integer'0 ->
    Ffi.neg_integer'0 * Ffi.non_neg_integer'0 * Ffi.pos_integer'0

  val mk_my_pair'2 : 'tA * 'tA -> 'tA my_pair2'1
end = struct
  type 'tA my_pair2'1 = ('tA, 'tA) my_pair'2

  and ('tA, 'tB) my_pair'2 = 'tA * 'tB

  let rec id'1 : 'tX -> 'tX = function v_X -> v_X

  let rec arg13'3 : 'tX * _ * _ -> 'tX = function v_X, v__Y, v__Z -> v_X

  let rec arg23'3 : _ * 'tY * _ -> 'tY = function v__X, v_Y, v__Z -> v_Y

  let rec arg33'3 : _ * _ * 'tZ -> 'tZ = function v__X, v__Y, v_Z -> v_Z

  let rec mk_int'0 : unit -> int = function () -> 1

  let rec mk_float'0 : unit -> int = function () -> 2

  let rec mk_string'0 : unit -> string = function () -> "Erlang String"

  let rec mk_char'0 : unit -> char = function () -> 'c'

  let rec mk_nil'0 : unit -> _ list = function () -> []

  let rec mk_cons'2 : 'tA * 'tA list -> 'tA list = function
    | v_H, v_T -> v_H :: v_T

  let rec mk_tuple'3 : 'tA * 'tB * 'tC -> 'tA * 'tB * 'tC = function
    | v_A, v_B, v_C -> (v_A, v_B, v_C)

  let rec mk_tuple2'1 : 'tA -> int * int * int * 'tA = function
    | v_A -> (1, 2, 3, v_A)

  let rec mk_map'2 :
      'tA * 'tB ->
      (< get_a : 'tA
       ; set_a : 'tA -> 'rec_tv__1
       ; get_b : 'tB
       ; set_b : 'tB -> 'rec_tv__1 >
       as
       'rec_tv__1) = function
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
      (< get_a : 'tA ; set_a : 'tA -> 'rec_tv__2 > as 'rec_tv__2) * 'tA ->
      (< get_a : 'tA ; set_a : 'tA -> 'rec_tv__3 > as 'rec_tv__3) = function
    | v_M, v_A ->
        let _ = Ffi.same'2 (v_M, v_M#set_a v_A) in
        let _ = Ffi.same'2 (v_A, v_M#get_a) in
        v_M#set_a v_A

  let rec update_map3'1 :
      (< get_a : 'tA
       ; set_a : 'tA -> 'rec_tv__4
       ; get_b : 'tA
       ; set_b : 'tA -> 'rec_tv__4 >
       as
       'rec_tv__4) ->
      (< get_a : 'tA
       ; set_a : 'tA -> 'rec_tv__5
       ; get_b : 'tA
       ; set_b : 'tA -> 'rec_tv__5 >
       as
       'rec_tv__5) = function
    | v_M ->
        let v_A0 = v_M#get_a in
        let v_B0 = v_M#get_a in
        let _ = Ffi.same'2 (v_M, v_M#set_a v_B0) in
        let _ = Ffi.same'2 (v_M, v_M#set_b v_A0) in
        let _ = Ffi.same'2 (v_B0, v_M#get_a) in
        let _ = Ffi.same'2 (v_A0, v_M#get_b) in
        (v_M#set_a v_B0)#set_b v_A0

  let rec access_map'1 :
      (< get_id : 'tId
       ; set_id : 'tId -> 'rec_tv__6
       ; get_location : 'tLocation
       ; set_location : 'tLocation -> 'rec_tv__6
       ; .. >
       as
       'rec_tv__6) ->
      'tId * 'tLocation = function
    | v_M -> (v_M#get_id, v_M#get_location)

  let rec access_map2'1 :
      (< get_inner1 :
           (< get_inner2 : 'tA ; set_inner2 : 'tA -> 'rec_tv__8 > as 'rec_tv__8)
       ; set_inner1 :
           (< get_inner2 : 'tA ; set_inner2 : 'tA -> 'rec_tv__8 > as 'rec_tv__8) ->
           'rec_tv__7 >
       as
       'rec_tv__7) ->
      'tA = function
    | v_M -> v_M#get_inner1#get_inner2

  let rec mk_seq'0 : unit -> int * int = function
    | () ->
        let v_X = 1 in
        let v_Y = 2 in
        (v_X, v_Y)

  let rec is_empty'1 : _ list -> bool = function
    | [] -> true
    | v__H :: v__T -> false

  let rec is_empty2'1 : _ list -> bool = function [] -> true | _ -> false

  let rec with_as'1 : int -> int = function 1 as v_X -> v_X | _ as v_Y -> v_Y

  let rec block'2 : 'tA * 'tA list -> 'tA list * 'tA list = function
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

  let rec fun_to_var'0 : unit -> 'tA * 'tB -> 'tA * 'tB = function
    | () ->
        let v_F = function v_X, v_Y -> (v_X, v_Y) in
        v_F

  let rec local_fun_to_var'2 : _ * _ -> _ list * _ list -> bool = function
    | v__A, v__B ->
        let v_F = call'2 in
        v_F

  let rec remote_fun_to_var'2 : _ * _ -> 'tA list -> 'tA list = function
    | v__A, v__B ->
        let v_F = List.rev in
        v_F

  let rec local_n_fun'0 : unit -> _ list -> _ list = function
    | () ->
        let v_F =
          let rec v_Local = function _ :: v_T -> v_Local v_T | [] -> [] in
          v_Local
        in
        v_F

  let rec mod01F'1 : 'tX -> 'tX = function v_X -> v_X

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

  let rec list_plus'2 : 'tA list * 'tA list -> 'tA list = function
    | v_X, v_Y -> v_X @ v_Y

  let rec list_minus'2 : 'tA list * 'tA list -> 'tA list = function
    | v_X, v_Y -> Ffi.list_diff'2 (v_X, v_Y)

  let rec comp1'2 : 'tA * 'tA -> bool = function v_X, v_Y -> v_X = v_Y

  let rec comp2'2 : 'tA * 'tA -> bool = function v_X, v_Y -> v_X <> v_Y

  let rec comp3'2 : 'tA * 'tA -> bool = function v_X, v_Y -> v_X <= v_Y

  let rec comp4'2 : 'tA * 'tA -> bool = function v_X, v_Y -> v_X < v_Y

  let rec comp5'2 : 'tA * 'tA -> bool = function v_X, v_Y -> v_X >= v_Y

  let rec comp6'2 : 'tA * 'tA -> bool = function v_X, v_Y -> v_X > v_Y

  let rec comp7'2 : 'tA * 'tA -> bool = function v_X, v_Y -> v_X = v_Y

  let rec comp8'2 : 'tA * 'tA -> bool = function v_X, v_Y -> v_X <> v_Y

  let rec guard1'2 : int * int -> bool = function
    | v_X, v_Y when (v_X = 1 && v_Y = 1) || v_X <> v_Y -> true
    | v_X, v_Y when v_X > v_Y -> false
    | v__X, v__Y -> false

  let rec guard2'3 : 'tA list * 'tA * 'tA list -> 'tA list = function
    | v_X, v_Y, [] -> (
        match v_X with v_X1 :: v_Xs when v_X1 = v_Y -> v_Xs | v_Xs -> v_Xs )
    | v_X, _, v_Z when v_X = v_Z -> v_Z
    | _, _, v_Z -> v_Z

  let rec p_match_tuple0'1 : unit -> unit Ffi.tuple1'1 = function
    | () -> Ffi.Tuple1 ()

  let rec p_match_tuple1'1 : _ Ffi.tuple1'1 -> unit = function
    | Ffi.Tuple1 v__X -> ()

  let rec p_match_invoke'0 : unit -> unit = function
    | () ->
        let _ = p_match_tuple0'1 () in
        p_match_tuple1'1 (Ffi.Tuple1 ())

  let rec any_id'1 : Ffi.any'0 -> Ffi.any'0 = function v_A -> v_A

  let rec atom_id'1 : Ffi.atom'0 -> Ffi.atom'0 = function v_A -> v_A

  let rec binary_id'1 : Ffi.binary'0 -> Ffi.binary'0 = function v_A -> v_A

  let rec bitstring_id'1 : Ffi.bitstring'0 -> Ffi.bitstring'0 = function
    | v_A -> v_A

  let rec byte_id'1 : Ffi.byte'0 -> Ffi.byte'0 = function v_A -> v_A

  let rec float_id'1 : float -> float = function v_X -> v_X

  let rec identifier_id'1 : Ffi.identifier'0 -> Ffi.identifier'0 = function
    | v_A -> v_A

  let rec iodata_id'1 : Ffi.iodata'0 -> Ffi.iodata'0 = function v_A -> v_A

  let rec iolist_id'1 : Ffi.iolist'0 -> Ffi.iolist'0 = function v_A -> v_A

  let rec map_id'1 : ('tA, 'tB) Ffi.map'2 -> ('tA, 'tB) Ffi.map'2 = function
    | v_M -> v_M

  let rec none_id'1 : Ffi.none'0 -> Ffi.none'0 = function v_A -> v_A

  let rec noreturn_id'1 : Ffi.no_return'0 -> Ffi.no_return'0 = function
    | v_A -> none_id'1 v_A

  let rec number_id'1 : Ffi.none'0 -> Ffi.none'0 = function v_A -> v_A

  let rec pid_id'1 : Ffi.pid'0 -> Ffi.pid'0 = function v_A -> v_A

  let rec port_id'1 : Ffi.port'0 -> Ffi.port'0 = function v_A -> v_A

  let rec reference_id'1 : Ffi.reference'0 -> Ffi.reference'0 = function
    | v_A -> v_A

  let rec term_id'1 : Ffi.term'0 -> Ffi.term'0 = function v_A -> v_A

  let rec timeout_id'1 : Ffi.timeout'0 -> Ffi.timeout'0 = function v_A -> v_A

  let rec ints_id'1 :
      Ffi.neg_integer'0 * Ffi.non_neg_integer'0 * Ffi.pos_integer'0 ->
      Ffi.neg_integer'0 * Ffi.non_neg_integer'0 * Ffi.pos_integer'0 = function
    | v_X -> v_X

  let rec mk_my_pair'2 : 'tA * 'tA -> 'tA my_pair2'1 = function
    | v_A, v_B -> (v_A, v_B)
end
