let rec singleton'1 : 'tA -> 'tA list = function v_Val -> [ v_Val ]

let rec repeat_help'3 = function
  | v_Res, v_N, v__Val when v_N <= 0 -> v_Res
  | v_Res, v_N, v_Val -> repeat_help'3 (v_Val :: v_Res, v_N, v_Val)

let rec repeat'2 : int * 'tA -> 'tA list = function
  | v_N, v_Val -> repeat_help'3 ([], v_N, v_Val)

let rec range_help'3 : int * int * int list -> int list = function
  | v_Lo, v_Hi, v_L when v_Lo <= v_Hi ->
      range_help'3 (v_Lo, v_Hi - 1, v_Hi :: v_L)
  | v__Lo, v__Hi, v_L -> v_L

let rec range'2 : int * int -> int list = function
  | v_Lo, v_Hi -> range_help'3 (v_Lo, v_Hi, [])

let rec cons'2 : 'tA * 'tA list -> 'tA list = function v_H, v_T -> v_H :: v_T

let rec map'2 : ('tA -> 'tB) * 'tA list -> 'tB list = function
  | v_F, v_H :: v_T -> v_F v_H :: map'2 (v_F, v_T)
  | v__F, [] -> []

let rec foldl'3 : ('tA * 'tB -> 'tB) * 'tB * 'tA list -> 'tB = function
  | v_F, v_Acc, v_H :: v_T -> foldl'3 (v_F, v_F (v_H, v_Acc), v_T)
  | v__F, v_Acc, [] -> v_Acc

let rec foldr'3 : ('tA * 'tB -> 'tB) * 'tB * 'tA list -> 'tB = function
  | v_F, v_Acc, v_H :: v_T -> v_F (v_H, foldr'3 (v_F, v_Acc, v_T))
  | v__F, v_Acc, [] -> v_Acc

let rec filter'2 : ('tA -> bool) * 'tA list -> 'tA list = function
  | v_F, v_List ->
      foldr'3
        ( (function
          | v_X, v_Xs -> (
              match v_F v_X with true -> v_X :: v_Xs | false -> v_Xs )),
          [],
          v_List )

let rec maybe_cons'3 : ('tA -> 'tB Maybe.maybe) * 'tA * 'tB list -> 'tB list =
  function
  | v_F, v_Mx, v_Xs -> (
      match v_F v_Mx with
      | Maybe.Just v_X -> cons'2 (v_X, v_Xs)
      | Maybe.Nothing -> v_Xs )

let rec maybe_cons'1 : ('tA -> 'tB Maybe.maybe) -> 'tA * 'tB list -> 'tB list =
  function
  | v_F -> ( function v_Mx, v_Xs -> maybe_cons'3 (v_F, v_Mx, v_Xs) )

let rec filter_map'2 : ('tA -> 'tB Maybe.maybe) * 'tA list -> 'tB list =
  function
  | v_F, v_Xs -> foldr'3 (maybe_cons'1 v_F, [], v_Xs)

let rec length'1 : _ list -> int = function
  | v_Xs -> foldl'3 ((function _, v_I -> v_I + 1), 0, v_Xs)

let rec reverse'1 : 'tA list -> 'tA list = function
  | v_Xs -> foldl'3 (cons'2, [], v_Xs)

let rec all'2 : ('tA -> bool) * 'tA list -> bool = function
  | v_Pred, v_H :: v_T -> (
      match v_Pred v_H with true -> all'2 (v_Pred, v_T) | false -> false )
  | v__Pred, [] -> true

let rec any'2 : ('tA -> bool) * 'tA list -> bool = function
  | v_Pred, v_H :: v_T -> (
      match v_Pred v_H with true -> true | false -> any'2 (v_Pred, v_T) )
  | v__Pred, [] -> false

let rec member'2 : 'tA * 'tA list -> bool = function
  | v_X, v_Xs -> any'2 (Basics.eq'1 v_X, v_Xs)

let rec maximum'1 : 'tA list -> 'tA Maybe.maybe = function
  | [] -> Maybe.Nothing
  | v_H :: v_T -> Maybe.Just (foldl'3 (Basics.max'2, v_H, v_T))

let rec minimum'1 : 'tA list -> 'tA Maybe.maybe = function
  | [] -> Maybe.Nothing
  | v_H :: v_T -> Maybe.Just (foldl'3 (Basics.min'2, v_H, v_T))

let rec sum'1 : int list -> int = function
  | v_Ns -> foldl'3 (Basics.add'2, 0, v_Ns)

let rec product'1 : int list -> int = function
  | v_Ns -> foldl'3 (Basics.mul'2, 0, v_Ns)

let rec append'2 : 'tA list * 'tA list -> 'tA list = function
  | v_Xs, v_Ys -> v_Xs @ v_Ys

let rec concat'1 : 'tA list list -> 'tA list = function
  | v_Lists -> foldr'3 (append'2, [], v_Lists)

let rec concat_map'2 : ('tA -> 'tB list) * 'tA list -> 'tB list = function
  | v_F, v_Xs -> concat'1 (map'2 (v_F, v_Xs))

let rec intersperse'2 : 'tA * 'tA list -> 'tA list = function
  | v_Sep, v_H :: v_T ->
      let v_Step = function v_X, v_Rest -> v_Sep :: v_X :: v_Rest in
      let v_Spersed = foldr'3 (v_Step, [], v_T) in
      v_H :: v_Spersed
  | v__Sep, [] -> []

let rec map2'3 : ('tA * 'tB -> 'tRes) * 'tA list * 'tB list -> 'tRes list =
  function
  | v_F, v_H1 :: v_T1, v_H2 :: v_T2 ->
      v_F (v_H1, v_H2) :: map2'3 (v_F, v_T1, v_T2)
  | _, _, _ -> []

let rec indexed_map'2 : (int * 'tA -> 'tB) * 'tA list -> 'tB list = function
  | v_F, v_Xs -> map2'3 (v_F, range'2 (0, length'1 v_Xs - 1), v_Xs)

let rec is_empty'1 : _ list -> bool = function _ :: _ -> false | [] -> true

let rec head'1 : 'tA list -> 'tA Maybe.maybe = function
  | v_H :: _ -> Maybe.Just v_H
  | [] -> Maybe.Nothing

let rec tail'1 : 'tA list -> 'tA list Maybe.maybe = function
  | _ :: v_T -> Maybe.Just v_T
  | [] -> Maybe.Nothing

let rec take'2 : int * 'tA list -> 'tA list = function
  | v_N, v__L when v_N <= 0 -> []
  | v__N, [] -> []
  | v_N, v_H :: v_T -> v_H :: take'2 (v_N - 1, v_T)

let rec drop'2 : int * 'tA list -> 'tA list = function
  | v_N, v_L when v_N <= 0 -> v_L
  | v__N, [] -> []
  | v_N, v__H :: v_T -> drop'2 (v_N - 1, v_T)

let rec partition'2 : ('tA -> bool) * 'tA list -> 'tA list * 'tA list = function
  | v_Pred, v_List ->
      let v_Step = function
        | v_X, (v_Trues, v_Falses) -> (
            match v_Pred v_X with
            | true -> (v_X :: v_Trues, v_Falses)
            | false -> (v_Trues, v_X :: v_Falses) )
      in
      foldr'3 (v_Step, ([], []), v_List)

let rec unzip'1 : ('tA * 'tB) list -> 'tA list * 'tB list = function
  | v_Pairs ->
      let v_Step = function
        | (v_X, v_Y), (v_Xs, v_Ys) -> (v_X :: v_Xs, v_Y :: v_Ys)
      in
      foldr'3 (v_Step, ([], []), v_Pairs)
