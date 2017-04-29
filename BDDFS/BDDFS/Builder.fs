module BDD

type IEntry =
    abstract member Var: int
    abstract member Low: int
    abstract member High: int

let DummyEntry(v) =
    {
        new IEntry with
        member __.Var = v
        member __.Low = failwith "not meant to be accessed"
        member __.High = failwith "not meant to be accessed"
    }

let Entry(v, l, h) =
    {
        new IEntry with
        member __.Var = v
        member __.Low = l
        member __.High = h
    }

type Builder(n: int) =
    let nodes = new System.Collections.Generic.List<_>([|DummyEntry(n + 1); DummyEntry(n + 1)|])
    let hashTable = new System.Collections.Generic.Dictionary<_, _>()
    
    member this.AddT(i, l, h) =
        let u = nodes.Count
        nodes.Add(Entry(i, l, h))
        u

    member this.Insert(i, l, h, u) =
        hashTable.Add((i, l, h), u)

    member this.MK(i, l, h) =
        if l = h
        then l
        else
            match hashTable.TryGetValue((i, l, h)) with
            | true, v -> v
            | _ ->
                let u = this.AddT((i, l, h))
                this.Insert(i, l, h, u)
                u

    member this.BuildEnv pred =
        let rec build' (env, i) =
            if i > n
            then
                if pred env then 1 else 0
            else
                let v0 = build'(Map.add i 0 env, i + 1)
                let v1 = build'(Map.add i 1 env, i + 1)
                this.MK(i, v0, v1)
        build'(Map.empty, 1)

    member this.Restrict(u, j, b) =
        let rec res u =
            let n = nodes.[u]
            match compare n.Var j with
            | 1 -> u
            | -1 -> this.MK(n.Var, res(n.Low), res(n.High))
            | 0 ->
                if b = 0
                then res(n.Low)
                else res(n.High)
            | _ -> failwith "not expected"
        res u

    member this.Apply(op, u1, u2) =
        let memTable: int option [,] = Array2D.create nodes.Count nodes.Count None
        let rec app(u1, u2) =
            match memTable.[u1, u2] with
            | Some u -> u
            | None ->
                let u =
                    if (u1 = 0 || u1 = 1) && (u2 = 0 || u2 = 1)
                    then op(u1, u2)
                    else
                        let e1, e2 = nodes.[u1], nodes.[u2]
                        if e1.Var = e2.Var
                        then this.MK(e1.Var, app(e1.Low, e2.Low), app(e1.High, e2.High))
                        else
                            if e1.Var < e2.Var
                            then this.MK(e1.Var, app(e1.Low, u2), app(e1.High, u2))
                            else this.MK(e2.Var, app(u1, e2.Low), app(u1, e2.High))
                memTable.[u1, u2] <- Some u
                u
        app(u1, u2)

    member this.ApplyN(op, us) =
        let memTable = new System.Collections.Generic.Dictionary<_, _>()
        let rec app (us: int[]) =
            match memTable.TryGetValue(us) with
            | true, u -> u
            | _ ->
                let u =
                    if Array.forall (fun u -> u = 0 || u = 1) us
                    then op us
                    else
                        let min = Seq.minBy (fun u -> nodes.[u].Var) us
                        let minVar = nodes.[min].Var
                        let l = Array.map (fun u -> let n = nodes.[u]
                                                    if n.Var = minVar then n.Low else u) us
                        let h = Array.map (fun u -> let n = nodes.[u]
                                                    if n.Var = minVar then n.High else u) us
                        this.MK(minVar, app l, app h)
                memTable.[us] <- u
                u
        app us

    member this.Compose (u1, x, u2) =
        let ite =
            function
            | [|x; y0; y1|] -> if x = 1 then y0 else y1
            | _ -> failwith "expected 3 args"
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
                let n = nodes.[t]
                let vl = Map.find n.Var env
                let c = if vl = 1 then n.High else n.Low
                eval c
        eval t
