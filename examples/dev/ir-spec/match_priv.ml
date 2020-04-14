module Match_priv : sig
  val f1'0 : unit -> int * int

  val f2'0 : unit -> int

  val f4'0 : unit -> int

  val f5'2 : ('tA * 'tB) * ('tA * 'tB) -> bool

  val f6'2 : 'tA * 'tA -> 'tA * 'tA

  val f7'2 : 'tA * 'tA -> 'tA * 'tA

  val f8'2 : ('tA * 'tB) * ('tA * 'tB) -> 'tA * 'tB

  val f9'2 : ('tA * 'tB) * ('tA * 'tB) -> 'tA * 'tB

  val f10'0 : unit -> int * int

  val f11'1 : ('tE * 'tE) list -> 'tE

  val f12'1 : ('tA * 'tA) * ('tA * 'tA) -> 'tA * 'tA

  val f13'2 : ('tA * 'tA) * 'tA -> 'tA

  val f14'1 : ('tA * 'tA) * ('tA * 'tA) -> 'tA * 'tA
end = struct
  let rec id'1 = function v_X -> v_X

  let rec f1'0 : unit -> int * int = function
    | () ->
        let v__X, v__Y = (1, 2) in
        (v__X, v__Y)

  let rec f2'0 : unit -> int = function
    | () ->
        let v_Y = 1 in
        let v__X = v_Y in
        v__X

  let rec f3'2 = function
    | v_X, v_T -> (
        match v_X with
        | 1 ->
            let v__Y = 2 in
            v__Y
        | 2 ->
            let v__Y = 3 in
            v__Y
        | _ ->
            let v_Z = v_T in
            let v__Y = v_Z in
            v__Y )

  let rec f4'0 : unit -> int = function
    | () ->
        let _ =
          f3'2
            (let v__Y = 1 in
             (v__Y, 4))
        in
        let _ =
          f3'2
            (let 2 = 1 in
             ( 2,
               let 3 = 4 in
               3 ))
        in
        let _ =
          id'1
            (let [ 1; 2 ] = [ 3; 4 ] in
             [ 1; 2 ])
        in
        0

  let rec f5'2 : ('tA * 'tB) * ('tA * 'tB) -> bool = function
    | v_P1, v_P2 ->
        (let v__X1, v__Y1 = v_P1 in
         (v__X1, v__Y1))
        =
        let v__X2, v__Y2 = v_P2 in
        (v__X2, v__Y2)

  let rec f6'2 : 'tA * 'tA -> 'tA * 'tA = function
    | v_X, v__p'X1 when v__p'X1 = v_X -> (v_X, v_X)

  let rec f7'2 : 'tA * 'tA -> 'tA * 'tA = function
    | v_X, (v_Y as v__p'X2) when v__p'X2 = v_X -> (v_X, v_Y)

  let rec f8'2 : ('tA * 'tB) * ('tA * 'tB) -> 'tA * 'tB = function
    | ((v_X, v_Y) as v_Z), ((v__p'X3, v__p'Y4) as v__p'Z5)
      when (v__p'Z5 = v_Z && v__p'Y4 = v_Y) && v__p'X3 = v_X ->
        v_Z

  let rec f9'2 : ('tA * 'tB) * ('tA * 'tB) -> 'tA * 'tB = function
    | v_P1, v_P2 -> (
        match (v_P1, v_P2) with
        | ((v_X, v_Y) as v_Z), ((v__p'X3, v__p'Y4) as v__p'Z5)
          when (v__p'Z5 = v_Z && v__p'Y4 = v_Y) && v__p'X3 = v_X ->
            v_Z )

  let rec f10'0 : unit -> int * int = function
    | () ->
        let v_X, v__p'X1 = (1, 1) in
        let true = v__p'X1 = v_X in
        (v_X, v_X)

  let rec f11'1 : ('tE * 'tE) list -> 'tE = function
    | v_L ->
        let [ (v_E, v__p'E1); (v__p'E2, v__p'E3) ] = v_L in
        let true = v__p'E3 = v_E in
        let true = v__p'E2 = v_E in
        let true = v__p'E1 = v_E in
        v_E

  let rec f12'1 : ('tA * 'tA) * ('tA * 'tA) -> 'tA * 'tA = function
    | ((v_E1, v_E2), (v__p'E22, v__p'E13)) as v_d_pat'1 ->
        let v_E6, v__p'E65 = v_d_pat'1 in
        let true = (v__p'E65 = v_E6 && v__p'E13 = v_E1) && v__p'E22 = v_E2 in
        v_E6

  let rec f13'2 : ('tA * 'tA) * 'tA -> 'tA = function
    | (v__P as v_d_pat'2), v__p'E33 ->
        let v_E3, v__p'E32 = v_d_pat'2 in
        let true = v__p'E33 = v_E3 && v__p'E32 = v_E3 in
        let v__X = v_E3 in
        v__X

  let rec f14'1 : ('tA * 'tA) * ('tA * 'tA) -> 'tA * 'tA = function
    | v_P ->
        let (((v_El1, v_El2), (v__p'El12, v__p'El23)) as v_d_pat'3) = v_P in
        let v_X, v__p'X5 = v_d_pat'3 in
        let true = v__p'X5 = v_X in
        let true = v__p'El23 = v_El2 in
        let true = v__p'El12 = v_El1 in
        v_X
end
