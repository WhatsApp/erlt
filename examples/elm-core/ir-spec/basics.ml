type order'0 = order

and order = Order'Lt | Order'Eq | Order'Gt

and never'0 = never

and never = Never'Just_one_more of never'0

let rec add'2 : int * int -> int = function v_X1, v_X2 -> v_X1 + v_X2

let rec add'1 : int -> int -> int = function
  | v_X1 -> ( function v_X2 -> add'2 (v_X1, v_X2) )

let rec sub'2 : int * int -> int = function v_X1, v_X2 -> v_X1 - v_X2

let rec sub'1 : int -> int -> int = function
  | v_X1 -> ( function v_X2 -> sub'2 (v_X1, v_X2) )

let rec mul'2 : int * int -> int = function v_X1, v_X2 -> v_X1 * v_X2

let rec mul'1 : int -> int -> int = function
  | v_X1 -> ( function v_X2 -> mul'2 (v_X1, v_X2) )

let rec idiv'2 : int * int -> int = function v_X1, v_X2 -> v_X1 / v_X2

let rec idiv'1 : int -> int -> int = function
  | v_X1 -> ( function v_X2 -> idiv'2 (v_X1, v_X2) )

let rec eq'2 : 'tA * 'tA -> bool = function v_X1, v_X2 -> v_X1 = v_X2

let rec eq'1 : 'tA -> 'tA -> bool = function
  | v_X1 -> ( function v_X2 -> eq'2 (v_X1, v_X2) )

let rec neq'2 : 'tA * 'tA -> bool = function v_X1, v_X2 -> v_X1 <> v_X2

let rec neq'1 : 'tA -> 'tA -> bool = function
  | v_X1 -> ( function v_X2 -> neq'2 (v_X1, v_X2) )

let rec lt'2 : 'tA * 'tA -> bool = function v_X1, v_X2 -> v_X1 < v_X2

let rec lt'1 : 'tA -> 'tA -> bool = function
  | v_X1 -> ( function v_X2 -> lt'2 (v_X1, v_X2) )

let rec gt'2 : 'tA * 'tA -> bool = function v_X1, v_X2 -> v_X1 > v_X2

let rec gt'1 : 'tA -> 'tA -> bool = function
  | v_X1 -> ( function v_X2 -> gt'2 (v_X1, v_X2) )

let rec le'2 : 'tA * 'tA -> bool = function v_X1, v_X2 -> v_X1 <= v_X2

let rec le'1 : 'tA -> 'tA -> bool = function
  | v_X1 -> ( function v_X2 -> le'2 (v_X1, v_X2) )

let rec ge'2 : 'tA * 'tA -> bool = function v_X1, v_X2 -> v_X1 >= v_X2

let rec ge'1 : 'tA -> 'tA -> bool = function
  | v_X1 -> ( function v_X2 -> ge'2 (v_X1, v_X2) )

let rec min'2 : 'tA * 'tA -> 'tA = function
  | v_X1, v_X2 -> (
      match lt'2 (v_X1, v_X2) with true -> v_X1 | false -> v_X2 )

let rec min'1 : 'tA -> 'tA -> 'tA = function
  | v_X1 -> ( function v_X2 -> min'2 (v_X1, v_X2) )

let rec max'2 : 'tA * 'tA -> 'tA = function
  | v_X1, v_X2 -> (
      match gt'2 (v_X1, v_X2) with true -> v_X1 | false -> v_X2 )

let rec max'1 : 'tA -> 'tA -> 'tA = function
  | v_X1 -> ( function v_X2 -> max'2 (v_X1, v_X2) )

let rec compare'2 : 'tA * 'tA -> order'0 = function
  | v_X1, v_X2 -> (
      match lt'2 (v_X1, v_X2) with
      | true -> Order'Lt
      | false -> (
          match eq'2 (v_X1, v_X2) with true -> Order'Eq | false -> Order'Gt ) )

let rec compare'1 : 'tA -> 'tA -> order'0 = function
  | v_X1 -> ( function v_X2 -> compare'2 (v_X1, v_X2) )

let rec not'1 : bool -> bool = function v_B -> not v_B

let rec and'2 : bool * bool -> bool = function v_B1, v_B2 -> v_B1 && v_B2

let rec and'1 : bool -> bool -> bool = function
  | v_B1 -> ( function v_B2 -> and'2 (v_B1, v_B2) )

let rec or'2 : bool * bool -> bool = function v_B1, v_B2 -> v_B1 || v_B2

let rec or'1 : bool -> bool -> bool = function
  | v_B1 -> ( function v_B2 -> or'2 (v_B1, v_B2) )

let rec xor'2 : bool * bool -> bool = function
  | v_B1, v_B2 -> (not v_B1) <> not v_B2

let rec xor'1 : bool -> bool -> bool = function
  | v_B1 -> ( function v_B2 -> xor'2 (v_B1, v_B2) )

let rec mod_by'2 : int * int -> int = function v_X1, v_X2 -> v_X2 / v_X1

let rec mod_by'1 : int -> int -> int = function
  | v_X1 -> ( function v_X2 -> mod_by'2 (v_X1, v_X2) )

let rec remainder_by'2 : int * int -> int = function
  | v_X1, v_X2 -> v_X2 mod v_X1

let rec remainder_by'1 : int -> int -> int = function
  | v_X1 -> ( function v_X2 -> remainder_by'2 (v_X1, v_X2) )

let rec negate'1 : int -> int = function v_X -> -v_X

let rec abs'1 : int -> int = function
  | v_X -> ( match lt'2 (v_X, 0) with true -> -v_X | false -> v_X )

let rec clamp'3 : int * int * int -> int = function
  | v_Low, v_High, v_Num -> (
      match lt'2 (v_Num, v_Low) with
      | true -> v_Low
      | false -> (
          match gt'2 (v_Num, v_High) with true -> v_High | false -> v_Num ) )

let rec composeL'2 : ('tB -> 'tC) * ('tA -> 'tB) -> 'tA -> 'tC = function
  | v_G, v_F -> ( function v_X -> v_G (v_F v_X) )

let rec composeL'1 : ('tB -> 'tC) -> ('tA -> 'tB) -> 'tA -> 'tC = function
  | v_G -> ( function v_F -> composeL'2 (v_G, v_F) )

let rec composeR'2 : ('tA -> 'tB) * ('tB -> 'tC) -> 'tA -> 'tC = function
  | v_F, v_G -> ( function v_X -> v_G (v_F v_X) )

let rec composeR'1 : ('tA -> 'tB) -> ('tB -> 'tC) -> 'tA -> 'tC = function
  | v_F -> ( function v_G -> composeR'2 (v_F, v_G) )

let rec apR'2 : 'tA * ('tA -> 'tB) -> 'tB = function v_X, v_F -> v_F v_X

let rec apR'1 : 'tA -> ('tA -> 'tB) -> 'tB = function
  | v_X -> ( function v_F -> apR'2 (v_X, v_F) )

let rec apL'2 : ('tA -> 'tB) * 'tA -> 'tB = function v_F, v_X -> v_F v_X

let rec apL'1 : ('tA -> 'tB) -> 'tA -> 'tB = function
  | v_F -> ( function v_X -> apL'2 (v_F, v_X) )

let rec identity'1 : 'tA -> 'tA = function v_X -> v_X

let rec always'2 : 'tA * _ -> 'tA = function v_A, _ -> v_A

let rec always'1 : 'tA -> _ -> 'tA = function
  | v_A -> ( function v_X -> always'2 (v_A, v_X) )

let rec never'1 : never'0 -> _ = function
  | Never'Just_one_more v_Nvr -> never'1 v_Nvr
