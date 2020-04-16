module St_mapping_priv : sig
  type float_alias'0 = int

  and ('tA, 'tB) some_fun'2 = 'tA -> 'tB

  and 'tA list_alias'1 = 'tA list

  and ('tA, 'tB) tuple2_alias'2 = 'tA * 'tB

  and ('tA, 'tB, 'tC) tuple3_alias'3 = 'tA * 'tB * 'tC

  and string_alias'0 = string

  and integer_alias'0 = int

  and char_alias'0 = char

  and boolean_alias'0 = bool

  and tuple0_alias'0 = unit

  and 'tA tuple1_alias'1 = 'tA Ffi.tuple1'1

  and atom_alias'0 = Ffi.atom'0

  and pid_alias'0 = Ffi.pid'0

  and port_alias'0 = Ffi.port'0

  and reference_alias'0 = Ffi.reference'0

  and neg_integer_alias'0 = Ffi.neg_integer'0

  and non_neg_integer_alias'0 = Ffi.non_neg_integer'0

  and pos_integer_alias'0 = Ffi.pos_integer'0

  and any_alias'0 = Ffi.any'0

  and none_alias'0 = Ffi.none'0

  and term_alias'0 = Ffi.term'0

  and binary_alias'0 = Ffi.binary'0

  and bitstring_alias'0 = Ffi.bitstring'0

  and byte_alias'0 = Ffi.byte'0

  and number_alias'0 = Ffi.number'0

  and iodata_alias'0 = Ffi.iodata'0

  and iolist_alias'0 = Ffi.iolist'0

  and identifier_alias'0 = Ffi.identifier'0

  and node_alias'0 = Ffi.node'0

  and timeout_alias'0 = Ffi.timeout'0

  and no_return_alias'0 = Ffi.no_return'0

  and ('tA, 'tB) map_alias'2 = ('tA, 'tB) Ffi.map'2

  and map_string_int'0 = (string, int) Ffi.map'2

  and rec_with_int_id'0 =
    < get_id : int ; set_id : int -> 'rec_tv__5 > as 'rec_tv__5

  and 'tA rec_with_generic_id'1 =
    < get_id : 'tA ; set_id : 'tA -> 'rec_tv__6 > as 'rec_tv__6

  and date'0 =
    < get_year : int
    ; set_year : int -> 'rec_tv__7
    ; get_month : string
    ; set_month : string -> 'rec_tv__7
    ; get_day : int
    ; set_day : int -> 'rec_tv__7 >
    as
    'rec_tv__7

  and ('tA, 'tB) either'2 = ('tA, 'tB) either

  and ('tA, 'tB) either = Either'Left of 'tA | Either'Right of 'tB

  and 'tA option'1 = 'tA option

  and 'tA option = Option'None | Option'Some of 'tA

  val option_to_list'1 : 'tA option'1 -> 'tA list

  val rec5'1 :
    (< get_id : 'tA ; set_id : 'tA -> 'rec_tv__1 > as 'rec_tv__1) -> 'tA

  val rec6'1 :
    (< get_year : int ; set_year : int -> 'rec_tv__2 > as 'rec_tv__2) ->
    (< get_year : int ; set_year : int -> 'rec_tv__3 > as 'rec_tv__3)

  val rec7'1 :
    (< get_id : 'tA ; set_id : 'tA -> 'rec_tv__4 ; .. > as 'rec_tv__4) -> 'tA
end = struct
  type float_alias'0 = int

  and ('tA, 'tB) some_fun'2 = 'tA -> 'tB

  and 'tA list_alias'1 = 'tA list

  and ('tA, 'tB) tuple2_alias'2 = 'tA * 'tB

  and ('tA, 'tB, 'tC) tuple3_alias'3 = 'tA * 'tB * 'tC

  and string_alias'0 = string

  and integer_alias'0 = int

  and char_alias'0 = char

  and boolean_alias'0 = bool

  and tuple0_alias'0 = unit

  and 'tA tuple1_alias'1 = 'tA Ffi.tuple1'1

  and atom_alias'0 = Ffi.atom'0

  and pid_alias'0 = Ffi.pid'0

  and port_alias'0 = Ffi.port'0

  and reference_alias'0 = Ffi.reference'0

  and neg_integer_alias'0 = Ffi.neg_integer'0

  and non_neg_integer_alias'0 = Ffi.non_neg_integer'0

  and pos_integer_alias'0 = Ffi.pos_integer'0

  and any_alias'0 = Ffi.any'0

  and none_alias'0 = Ffi.none'0

  and term_alias'0 = Ffi.term'0

  and binary_alias'0 = Ffi.binary'0

  and bitstring_alias'0 = Ffi.bitstring'0

  and byte_alias'0 = Ffi.byte'0

  and number_alias'0 = Ffi.number'0

  and iodata_alias'0 = Ffi.iodata'0

  and iolist_alias'0 = Ffi.iolist'0

  and identifier_alias'0 = Ffi.identifier'0

  and node_alias'0 = Ffi.node'0

  and timeout_alias'0 = Ffi.timeout'0

  and no_return_alias'0 = Ffi.no_return'0

  and ('tA, 'tB) map_alias'2 = ('tA, 'tB) Ffi.map'2

  and map_string_int'0 = (string, int) Ffi.map'2

  and rec_with_int_id'0 =
    < get_id : int ; set_id : int -> 'rec_tv__5 > as 'rec_tv__5

  and 'tA rec_with_generic_id'1 =
    < get_id : 'tA ; set_id : 'tA -> 'rec_tv__6 > as 'rec_tv__6

  and date'0 =
    < get_year : int
    ; set_year : int -> 'rec_tv__7
    ; get_month : string
    ; set_month : string -> 'rec_tv__7
    ; get_day : int
    ; set_day : int -> 'rec_tv__7 >
    as
    'rec_tv__7

  and ('tA, 'tB) either'2 = ('tA, 'tB) either

  and ('tA, 'tB) either = Either'Left of 'tA | Either'Right of 'tB

  and 'tA option'1 = 'tA option

  and 'tA option = Option'None | Option'Some of 'tA

  let rec option_to_list'1 : 'tA option'1 -> 'tA list = function
    | Option'None -> []
    | Option'Some v_A -> [ v_A ]

  let rec rec1'0 = function () -> object end

  let rec rec2'0 = function
    | () ->
        object
          val val_year = 2020

          method get_year = val_year

          method set_year new_val_year = {<val_year = new_val_year>}

          val val_month = "April"

          method get_month = val_month

          method set_month new_val_month = {<val_month = new_val_month>}

          val val_day = 17

          method get_day = val_day

          method set_day new_val_day = {<val_day = new_val_day>}
        end

  let rec rec3'1 = function v_Rec -> v_Rec#get_id

  let rec rec4'1 = function
    | v_Rec ->
        let _ = v_Rec = v_Rec#set_year 2046 in
        let _ = 2046 = v_Rec#get_year in
        v_Rec#set_year 2046

  let rec rec5'1 :
      (< get_id : 'tA ; set_id : 'tA -> 'rec_tv__1 > as 'rec_tv__1) -> 'tA =
    function
    | v_Rec -> v_Rec#get_id

  let rec rec6'1 :
      (< get_year : int ; set_year : int -> 'rec_tv__2 > as 'rec_tv__2) ->
      (< get_year : int ; set_year : int -> 'rec_tv__3 > as 'rec_tv__3) =
    function
    | v_Rec ->
        let _ = v_Rec = v_Rec#set_year 2046 in
        let _ = 2046 = v_Rec#get_year in
        v_Rec#set_year 2046

  let rec rec7'1 :
      (< get_id : 'tA ; set_id : 'tA -> 'rec_tv__4 ; .. > as 'rec_tv__4) -> 'tA
      = function
    | v_Rec -> v_Rec#get_id

  let rec update_x'2 = function
    | v_R, v_X ->
        let _ = v_R = v_R#set_x v_X in
        let _ = v_X = v_R#get_x in
        v_R#set_x v_X
end
