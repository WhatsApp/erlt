let same''2 (x, y) = x == y

let rec odd'1 = function v_X -> even'1 v_X

and even'1 = function v_X -> odd'1 v_X

let rec id'1 = function v_X -> v_X

let rec id_caller'1 = function v_X -> id'1 v_X

let rec id_rec'1 = function v_X -> id_rec'1 v_X
