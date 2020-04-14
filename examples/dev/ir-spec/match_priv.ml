module Match_priv : sig
  val f1'0 : unit -> int * int

  val f2'0 : unit -> int

  val f4'0 : unit -> int

  val f5'2 : ('tA * 'tB) * ('tA * 'tB) -> bool
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
end
