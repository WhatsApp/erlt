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

val remote_call'1 : 'tA list -> 'tA list

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
