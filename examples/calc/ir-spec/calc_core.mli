type expr'0 = expr

and expr =
  | Expr'Number of int
  | Expr'Add of expr'0 * expr'0
  | Expr'Subtr of expr'0 * expr'0
  | Expr'Mult of expr'0 * expr'0
  | Expr'Divd of expr'0 * expr'0
  | Expr'Var of Ffi.atom'0

val expr_number'1 : int -> expr'0

val expr_add'2 : expr'0 * expr'0 -> expr'0

val expr_subtr'2 : expr'0 * expr'0 -> expr'0

val expr_mult'2 : expr'0 * expr'0 -> expr'0

val expr_divd'2 : expr'0 * expr'0 -> expr'0

val expr_var'1 : Ffi.atom'0 -> expr'0
