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

    member this.BuildEnv t =
        let rec build' (env, i) =
            if i > n
            then
                if not <| evalExp (fun v -> Map.find v env) t then 0 else 1
            else
                let v0 = build'(Map.add i false env, i + 1)
                let v1 = build'(Map.add i true env, i + 1)
                this.MK(i, v0, v1)
        build'(Map.empty, 1)

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

    member this.Apply(op, u1, u2) =
        let G: int option [,] = Array2D.create u1 u2 None
        let rec app(u1, u2) =
            match G.[u1, u2] with
            | Some u -> u
            | None ->
                let u =
                    if (u1 = 0 || u1 = 1) && (u2 = 0 || u2 = 1)
                    then op(u1, u2)
                    else
                        let e1, e2 = T.[u1], T.[u2]
                        let v1, l1, h1 = e1.v, e1.l, e1.h
                        let v2, l2, h2 = e2.v, e2.l, e2.h
                        if v1 = v2
                        then this.MK(v1, app(l1, l2), app(h1, h2))
                        else
                            if v1 < v2
                            then this.MK(v1, app(l1, u2), app(h1, u2))
                            else this.MK(v2, app(u1, l2), app(u1, h2))
                G.[u1, u2] <- Some u
                u
        app(u1, u2)

    member this.ApplyN(op, us) =
        let G = new System.Collections.Generic.Dictionary<_, _>()
        let rec app (us: int[]) =
            match G.TryGetValue(us) with
            | true, u -> u
            | _ ->
                let u =
                    if Array.forall (fun u -> u = 0 || u = 1) us
                    then op us
                    else
                        let min = Seq.minBy (fun u -> T.[u].v) us
                        let minVar = T.[min].v
                        let l = Array.map (fun u -> let n = T.[u]
                                                    if n.v = minVar then n.l else u) us
                        let h = Array.map (fun u -> let n = T.[u]
                                                    if n.v = minVar then n.h else u) us
                        this.MK(minVar, app l, app h)
                G.[us] <- u
                u
        app us
        