type ('A, 'B) my_pair = 'A * 'B

type 'A my_pair2 = ('A, 'A) my_pair

val id'1 : 'X -> 'X

val arg13'3 : 'X * _ * _ -> 'X

val arg23'3 : _ * 'Y * _ -> 'Y

val arg33'3 : _ * _ * 'Z -> 'Z

val mk_int'0 : unit -> int

val mk_float'0 : unit -> int

val mk_string'0 : unit -> string

val mk_char'0 : unit -> char

val mk_nil'0 : unit -> _ list

val mk_cons'2 : 'A * 'A list -> 'A list

val mk_tuple'3 : 'A * 'B * 'C -> 'A * 'B * 'C

val mk_tuple2'1 : 'A -> int * int * int * 'A

val mk_map'2 :
  'A * 'B ->
  (< get_a : 'A ; set_a : 'A -> 'map_1 ; get_b : 'B ; set_b : 'B -> 'map_1 >
   as
   'map_1)

val update_map1'2 :
  (< get_a : 'A ; set_a : 'A -> 'map_2 > as 'map_2) * 'A ->
  (< get_a : 'A ; set_a : 'A -> 'map_3 > as 'map_3)

val update_map2'2 :
  (< get_a : 'A ; set_a : 'A -> 'map_M ; .. > as 'map_M) * 'A ->
  (< get_a : 'A ; set_a : 'A -> 'map_M ; .. > as 'map_M)

val update_map3'1 :
  (< get_a : 'A ; set_a : 'A -> 'map_4 ; get_b : 'A ; set_b : 'A -> 'map_4 >
   as
   'map_4) ->
  (< get_a : 'A ; set_a : 'A -> 'map_5 ; get_b : 'A ; set_b : 'A -> 'map_5 >
   as
   'map_5)

val update_map4'1 :
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
   'map_M)

val access_map'1 :
  (< get_id : 'Id
   ; set_id : 'Id -> 'map__
   ; get_location : 'Location
   ; set_location : 'Location -> 'map__
   ; .. >
   as
   'map__) ->
  'Id * 'Location

val mk_seq'0 : unit -> int * int

val is_empty'1 : _ list -> bool

val is_empty2'1 : _ list -> bool

val with_as'1 : int -> int

val block'2 : 'A * 'A list -> 'A list * 'A list

val is_empty_case'1 : _ list -> bool

val both_empty'2 : _ list * _ list -> bool

val call'2 : _ list * _ list -> bool

val remote_call'1 : 'A list -> 'A list

val fun_to_var'0 : unit -> 'A * 'B -> 'A * 'B

val local_fun_to_var'2 : _ * _ -> _ list * _ list -> bool

val remote_fun_to_var'2 : _ * _ -> 'A list -> 'A list

val local_n_fun'0 : unit -> _ list -> _ list

val mod01F'1 : 'X -> 'X

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

val list_plus'2 : 'A list * 'A list -> 'A list

val list_minus'2 : 'A list * 'A list -> 'A list

val comp1'2 : 'A * 'A -> bool

val comp2'2 : 'A * 'A -> bool

val comp3'2 : 'A * 'A -> bool

val comp4'2 : 'A * 'A -> bool

val comp5'2 : 'A * 'A -> bool

val comp6'2 : 'A * 'A -> bool

val comp7'2 : 'A * 'A -> bool

val comp8'2 : 'A * 'A -> bool

val guard1'2 : int * int -> bool

val guard2'3 : 'A list * 'A * 'A list -> 'A list

val p_match_tuple0'1 : unit -> unit Ffi.tuple1

val p_match_tuple1'1 : _ Ffi.tuple1 -> unit

val p_match_invoke'0 : unit -> unit

val any_id'1 : Ffi.any -> Ffi.any

val atom_id'1 : Ffi.atom -> Ffi.atom

val binary_id'1 : Ffi.binary -> Ffi.binary

val bitstring_id'1 : Ffi.bitstring -> Ffi.bitstring

val byte_id'1 : Ffi.byte -> Ffi.byte

val float_id'1 : float -> float

val identifier_id'1 : Ffi.identifier -> Ffi.identifier

val iodata_id'1 : Ffi.iodata -> Ffi.iodata

val iolist_id'1 : Ffi.iolist -> Ffi.iolist

val map_id'1 : ('A, 'B) Ffi.map -> ('A, 'B) Ffi.map

val none_id'1 : Ffi.none -> Ffi.none

val noreturn_id'1 : Ffi.no_return -> Ffi.no_return

val number_id'1 : Ffi.none -> Ffi.none

val pid_id'1 : Ffi.pid -> Ffi.pid

val port_id'1 : Ffi.port -> Ffi.port

val reference_id'1 : Ffi.reference -> Ffi.reference

val term_id'1 : Ffi.term -> Ffi.term

val timeout_id'1 : Ffi.timeout -> Ffi.timeout

val ints_id'1 :
  Ffi.neg_integer * Ffi.non_neg_integer * Ffi.pos_integer ->
  Ffi.neg_integer * Ffi.non_neg_integer * Ffi.pos_integer

val mk_my_pair'2 : 'A * 'B -> ('A, 'B) my_pair
