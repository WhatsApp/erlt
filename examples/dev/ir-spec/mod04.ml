let rec mk_unit_remote_dot'0 : unit -> Mod03.unit0 = function
  | () -> Mod03.Unit0

let rec mk_left_remote_dot'1 : 'tA -> ('tA, _) Mod03.either = function
  | v_A -> Mod03.Left v_A

let rec mk_right_remote_dot'1 : 'tB -> (_, 'tB) Mod03.either = function
  | v_B -> Mod03.Right v_B

let rec zero_remote_dot'2 : Mod03.unit0 * 'tV -> 'tV = function
  | v_U, v_Val -> ( match v_U with Mod03.Unit0 -> v_Val )

let rec zero_remote1_dot'2 : Mod03.unit0 * 'tV -> 'tV = function
  | Mod03.Unit0, v_Val -> v_Val

let rec un_pair_remote_dot'1 : ('tA, 'tB) Mod03.pair -> 'tA * 'tB = function
  | v_Pair -> ( match v_Pair with Mod03.Pair (v_A, v_B) -> (v_A, v_B) )
