let rec pair'2 : 'A * 'B -> 'A * 'B = function v_A, v_B -> (v_A, v_B)

let rec pair'1 : 'A -> 'B -> 'A * 'B = function
  | v_A -> ( function v_B -> pair'2 (v_A, v_B) )

let rec first'1 : 'A * _ -> 'A = function v_A, _ -> v_A

let rec second'1 : _ * 'B -> 'B = function _, v_B -> v_B

let rec map_first'2 : ('A -> 'X) * ('A * 'B) -> 'X * 'B = function
  | v_F, (v_A, v_B) -> (v_F v_A, v_B)

let rec map_first'1 : ('A -> 'X) -> 'A * 'B -> 'X * 'B = function
  | v_F -> ( function v_P -> map_first'2 (v_F, v_P) )

let rec map_second'2 : ('B -> 'Y) * ('A * 'B) -> 'A * 'Y = function
  | v_F, (v_A, v_B) -> (v_A, v_F v_B)

let rec map_second'1 : ('B -> 'Y) -> 'A * 'B -> 'A * 'Y = function
  | v_F -> ( function v_P -> map_second'2 (v_F, v_P) )

let rec map_both'3 : ('A -> 'X) * ('B -> 'Y) * ('A * 'B) -> 'X * 'Y = function
  | v_Fa, v_Fb, (v_A, v_B) -> (v_Fa v_A, v_Fb v_B)

let rec map_both'2 : ('A -> 'X) * ('B -> 'Y) -> 'A * 'B -> 'X * 'Y = function
  | v_Fa, v_Fb -> ( function v_P -> map_both'3 (v_Fa, v_Fb, v_P) )
