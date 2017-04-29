module Lang

type Variable = int
type Value = int

type BinOp =
    And | Or | Imp | BiImp

type Exp =
    | VarEq of Variable * Value
    | ZeroExp
    | OneExp
    | Not of Exp
    | Bin of Exp * BinOp * Exp

let evalExp env =
    let rec ev =
        function
        | VarEq(v, vl) -> env v = vl
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
