module Calc_priv : sig
  type env'0 = (Ffi.atom'0 * int) list

  val calculate'2 : env'0 * string -> int
end = struct
  type env'0 = (Ffi.atom'0 * int) list

  let rec lookup'2 = function
    | v_A, (v__p'A1, v_V) :: _ when v__p'A1 = v_A -> v_V
    | v_A, _ :: v_Rest -> lookup'2 (v_A, v_Rest)

  let rec eval'2 = function
    | v_Env, v_Expr -> (
        match v_Expr with
        | Calc_core.Expr'Number v_N -> v_N
        | Calc_core.Expr'Add (v_E1, v_E2) ->
            eval'2 (v_Env, v_E1) + eval'2 (v_Env, v_E2)
        | Calc_core.Expr'Subtr (v_E1, v_E2) ->
            eval'2 (v_Env, v_E1) - eval'2 (v_Env, v_E2)
        | Calc_core.Expr'Mult (v_E1, v_E2) ->
            eval'2 (v_Env, v_E1) * eval'2 (v_Env, v_E2)
        | Calc_core.Expr'Divd (v_E1, v_E2) ->
            eval'2 (v_Env, v_E1) / eval'2 (v_Env, v_E2)
        | Calc_core.Expr'Var v_Name -> lookup'2 (v_Name, v_Env) )

  let rec calculate'2 : env'0 * string -> int = function
    | v_Env, v_Input -> eval'2 (v_Env, Calc_parser_ffi.parse'1 v_Input)
end
