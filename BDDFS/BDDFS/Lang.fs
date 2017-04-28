module Lang

type Variable = int

type BinOp =
    And | Or | Imp | BiImp

type Exp =
    | VarDeref of Variable
    | ZeroExp
    | OneExp
    | Not of Exp
    | Bin of Exp * BinOp * Exp

let evalExp env =
    let rec ev =
        function
        | VarDeref v -> env v
        | ZeroExp -> false
        | OneExp -> true
        | Not e -> not <| ev e
        | Bin(e1, binOp, e2) ->
            let op =
                match binOp with
                | And -> (&&)
                | Or -> (||)
                | Imp -> fun b1 b2 -> (not b1) || b2
                | BiImp -> (=)
            op (ev e1) (ev e2)
    ev

let substExp env =
    let rec s =
        function
        | VarDeref v -> env v
        | ZeroExp -> ZeroExp
        | OneExp -> OneExp
        | Not e -> Not <| s e
        | Bin(e1, binOp, e2) -> Bin(s e1, binOp, s e2)
    s
