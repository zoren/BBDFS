module BDD

open Lang

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
