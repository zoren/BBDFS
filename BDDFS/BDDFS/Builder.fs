module BDD

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

    member this.BuildEnv pred =
        let rec build' (env, i) =
            if i > n
            then
                if pred (fun v -> Map.find v env) then 1 else 0
            else
                let v0 = build'(Map.add i 0 env, i + 1)
                let v1 = build'(Map.add i 1 env, i + 1)
                this.MK(i, v0, v1)
        build'(Map.empty, 1)

    member this.Restrict(u, j, b) =
        let rec res u =
            let n = T.[u]
            match compare n.v j with
            | 1 -> u
            | -1 -> this.MK(n.v, res(n.l), res(n.h))
            | 0 ->
                if b = 0
                then res(n.l)
                else res(n.h)
        res u

    member this.Apply(op, u1, u2) =
        let G: int option [,] = Array2D.create T.Count T.Count None
        let rec app(u1, u2) =
            match G.[u1, u2] with
            | Some u -> u
            | None ->
                let u =
                    if (u1 = 0 || u1 = 1) && (u2 = 0 || u2 = 1)
                    then op(u1, u2)
                    else
                        let e1, e2 = T.[u1], T.[u2]
                        if e1.v = e2.v
                        then this.MK(e1.v, app(e1.l, e2.l), app(e1.h, e2.h))
                        else
                            if e1.v < e2.v
                            then this.MK(e1.v, app(e1.l, u2), app(e1.h, u2))
                            else this.MK(e2.v, app(u1, e2.l), app(u1, e2.h))
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

    member this.Compose (u1, x, u2) =
        let ite [|x; y0; y1|] = if x = 1 then y0 else y1
        this.ApplyN(ite, [|u1; x; u2|])

    member this.Exists(x, t) =
        let t0 = this.Restrict(t, 0, x)
        let t1 = this.Restrict(t, 1, x)
        let orF = fun(b1, b2) -> if b1 = 1 || b2 = 1 then 1 else 0
        this.Apply(orF, t0, t1)

    member this.Eval(env, t) =
        let rec eval t =
            match t with
            | 0 -> false
            | 1 -> true
            | _ ->
                let n = T.[t]
                let vl = env(n.v)
                let c = if vl = 1 then n.h else n.l
                eval c
        eval t
