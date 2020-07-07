type maybe_pair'0 = maybe_pair

and maybe_pair =
  | Maybe_pair'Really_pair of (int, int) Mod03.pair'2
  | Maybe_pair'Empty

let rec mk_pair'2 : int * int -> (int, int) Mod03.pair'2 = function
  | v_A, v_B -> Mod03.Pair'Pair (v_A, v_B)

let rec mk_ordered_pair'1 : (int, int) Mod03.pair'2 -> maybe_pair'0 = function
  | Mod03.Pair'Pair (v_A, v_B) as v_P when v_A >= v_B ->
      Maybe_pair'Really_pair v_P
  | Mod03.Pair'Pair (v_A, v_B) when v_A < v_B -> Maybe_pair'Empty
