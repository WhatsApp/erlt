type expr'0 = expr

and expr =
  | Expr'Number of int
  | Expr'Add of expr'0 * expr'0
  | Expr'Subtr of expr'0 * expr'0
  | Expr'Mult of expr'0 * expr'0
  | Expr'Divd of expr'0 * expr'0
  | Expr'Var of Ffi.atom'0

let rec expr_number'1 : int -> expr'0 = function v_N -> Expr'Number v_N

let rec expr_add'2 : expr'0 * expr'0 -> expr'0 = function
  | v_E1, v_E2 -> Expr'Add (v_E1, v_E2)

let rec expr_subtr'2 : expr'0 * expr'0 -> expr'0 = function
  | v_E1, v_E2 -> Expr'Subtr (v_E1, v_E2)

let rec expr_mult'2 : expr'0 * expr'0 -> expr'0 = function
  | v_E1, v_E2 -> Expr'Mult (v_E1, v_E2)

let rec expr_divd'2 : expr'0 * expr'0 -> expr'0 = function
  | v_E1, v_E2 -> Expr'Divd (v_E1, v_E2)

let rec expr_var'1 : Ffi.atom'0 -> expr'0 = function v_A -> Expr'Var v_A
