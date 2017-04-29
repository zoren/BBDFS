module NDD

type TEntry =
    abstract member v: int
    abstract Children : int[] with get

let DummyEntry(v) =
    {
        new TEntry with
        member __.v = v
        member __.Children with get() = failwith "not meant to be accessed"
    }

let Entry(v, children: int[]) =
    {
        new TEntry with
        member __.v = v
        member __.Children with get() = children
    }

type NDDBuilder(domainSizes: int array) =
    do Array.iter(fun d -> if d < 2 then failwith "domain size") domainSizes
    let n = domainSizes.Length
    let T = new System.Collections.Generic.List<_>([|DummyEntry(n + 1); DummyEntry(n + 1)|])
    let H = new System.Collections.Generic.Dictionary<_, _>()
    
    let getDomainValues i = [|0 .. domainSizes.[i - 1] - 1|]

    member this.addT(i, children) =
        let u = T.Count
        T.Add(Entry(i, children))
        u

    member this.insert(i, children, u) =
        H.Add((i, children), u)

    member this.MK(i, children: int[]) =
        let firstChild = children.[0]
        if Array.forall((=)firstChild) children
        then firstChild
        else
            match H.TryGetValue((i, children)) with
            | true, v -> v
            | _ ->
                let u = this.addT((i, children))
                this.insert(i, children, u)
                u

    member this.BuildEnv pred =
        let rec build' (env, i) =
            if i > n
            then
                if pred (fun v -> Map.find v env) then 1 else 0
            else
                let values =
                    Array.map (fun value -> build'(Map.add i value env, i + 1)) <| getDomainValues i
                this.MK(i, values)
        build'(Map.empty, 1)

    member this.Restrict(u, j, b) =
        let rec res u =
            let n = T.[u]
            match compare n.v j with
            | 1 -> u
            | -1 -> this.MK(n.v, Array.map res n.Children)
            | 0 -> res(n.Children.[b])
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
                        then
                            let values = getDomainValues e1.v
                            this.MK(e1.v, values |> Array.map (fun value -> app(e1.Children.[value], e2.Children.[value])))
                        else
                            if e1.v < e2.v
                            then
                                let values = getDomainValues e1.v
                                this.MK(e1.v, values |> Array.map (fun value -> app(e1.Children.[value], u2)))
                            else
                                let values = getDomainValues e2.v
                                this.MK(e2.v, values |> Array.map (fun value -> app(u1, e2.Children.[value])))
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
                        let values = getDomainValues minVar
                        this.MK(minVar, values |>
                                    Array.map
                                        (fun value ->
                                            app(
                                                us |> Array.map (fun u -> let n = T.[u]
                                                                          if n.v = minVar then n.Children.[value] else u))))
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
                eval n.Children.[vl]
        eval t

