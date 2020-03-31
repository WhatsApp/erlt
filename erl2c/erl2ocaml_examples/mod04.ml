let rec mk_unit_remote'0 = function () -> Mod03.Unit0

let rec mk_left_remote'1 = function v_A -> Mod03.Left v_A

let rec mk_right_remote'1 = function v_B -> Mod03.Right v_B

let rec zero_remote'2 = function
  | v_U, v_Val -> ( match v_U with Mod03.Unit0 -> v_Val )

let rec un_pair_remote'1 = function
  | v_Pair -> ( match v_Pair with Mod03.Pair (v_A, v_B) -> (v_A, v_B) )
