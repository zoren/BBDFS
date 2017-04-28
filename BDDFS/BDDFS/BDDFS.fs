module BDDFS

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

type BDD =
    | Zero of Variable
    | One of Variable
    | Node of Variable * BDD * BDD

let eval env =
    let rec e =
        function
        | Zero _ -> false
        | One _ -> true
        | Node(v, l, h) ->
            if env v
            then e h
            else e l
    e

let test =
    Node(1, One 2, Zero 2)

type Builder(n: int) =
    let T = new System.Collections.Generic.List<_>([|(n + 1, None, None); (n + 1, None, None)|])
    let H = new System.Collections.Generic.Dictionary<_, _>()
    
    member this.addT(i, l, h) =
        T.Add((i, Some l, Some h))
        T.Count

    member this.insert(i, l, h, u) =
        H.Add((i, l, h), u)

    member this.MK(i, l, h) =
        if l = h
        then l
        else
            match H.TryGetValue((i, l, h)) with
            | true, v -> v
            | _ ->
                let u = this.addT((i, l, h))
                this.insert(i, l, h, u)
                u

    member this.Build t =
        let rec build' (t, i) =
            if i > n
            then
                if not <| evalExp (fun _ -> failwith "no free var expected") t then 0 else 1
            else
                let v0 = build'(substExp (fun v -> if v = i then ZeroExp else VarDeref v) t, i + 1)
                let v1 = build'(substExp (fun v -> if v = i then OneExp else VarDeref v) t, i + 1)
                this.MK(i, v0, v1)
        build'(t, 1)

[<EntryPoint>]
let main argv =
    let env = Map.ofList [1, true]
    printfn "%A" <| eval (fun v -> Map.find v env) test
    0 // return an integer exit code
