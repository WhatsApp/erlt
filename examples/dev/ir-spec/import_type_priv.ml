module Import_type_priv : sig
  val f'1 : (bool, int) Mod01.my_pair'2 -> int
end = struct
  let rec f'1 : (bool, int) Mod01.my_pair'2 -> int = function
    | v_X -> ( match v_X with true, v_N -> v_N | false, _ -> 0 )
end
