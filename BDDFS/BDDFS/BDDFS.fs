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

type Builder(n: int) =
    let T = new System.Collections.Generic.List<_>([|(n + 1, None, None); (n + 1, None, None)|])
    let H = new System.Collections.Generic.Dictionary<_, _>()
    
    member this.addT(i, l, h) =
        let u = T.Count
        T.Add((i, Some l, Some h))
        u

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

    member this.GetExport u =
        printfn "%A" u
        printfn "%A" T
        let rec exp i =
            if i < 0
            then failwith "i lt 0"
            if i >= T.Count
            then failwithf "%i >= %i" i T.Count
            
            let v, l, h  = T.[i]
            match i with
            | 0 -> Zero v
            | 1 -> One v
            | _ -> Node(v, exp <| Option.get l, exp <| Option.get h)
        exp u

let v i = VarDeref i
let andE t1 t2 = Bin(t1, And, t2)
let orE t1 t2 = Bin(t1, Or, t2)
let impE t1 t2 = Bin(t1, Imp, t2)
let biimpE t1 t2 = Bin(t1, BiImp, t2)

[<EntryPoint>]
let main argv =
    let v1, v2, v3 = v 1, v 2, v 3
    let t = orE (biimpE v1 v2) v3
    let b = Builder(3)
    let bddTopIndex = b.Build t
    let bdd = b.GetExport bddTopIndex
    let env = Map.ofList [1, true; 2, false; 3, true]
    printfn "%A" <| eval (fun v -> Map.find v env) bdd
    printfn "%A" <| evalExp (fun v -> Map.find v env) t
    0 // return an integer exit code
