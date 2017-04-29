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

type TEntry =
    abstract member v: int
    abstract member l: int
    abstract member h: int

let DummyEntry(v) =
    {
        new TEntry with
        member __.v = v
        member __.l = failwith "not meant to be accessed"
        member __.h = failwith "not meant to be accessed"
    }

let Entry(v, l, h) =
    {
        new TEntry with
        member __.v = v
        member __.l = l
        member __.h = h
    }

type Builder(n: int) =
    let T = new System.Collections.Generic.List<_>([|DummyEntry(n + 1); DummyEntry(n + 1)|])
    let H = new System.Collections.Generic.Dictionary<_, _>()
    
    member this.addT(i, l, h) =
        let u = T.Count
        T.Add(Entry(i, l, h))
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
            
            let entry = T.[i]
            match i with
            | 0 -> Zero entry.v
            | 1 -> One entry.v
            | _ -> Node(entry.v, exp entry.l, exp entry.h)
        exp u
