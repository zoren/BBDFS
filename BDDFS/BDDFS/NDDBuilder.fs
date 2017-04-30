module NDD

type IEntry =
    abstract member Var: int
    abstract Children : int[] with get

let DummyEntry(v) =
    {
        new IEntry with
        member __.Var = v
        member __.Children with get() = failwith "not meant to be accessed"
    }

let Entry(v, children: int[]) =
    {
        new IEntry with
        member __.Var = v
        member __.Children with get() = children
    }

type NDDBuilder(domainSizes: int array) =
    do Array.iter(fun d -> if d < 2 then failwith "domain size") domainSizes
    let n = domainSizes.Length
    let nodes = new System.Collections.Generic.List<_>([|DummyEntry(n); DummyEntry(n)|])
    let hash = new System.Collections.Generic.Dictionary<_, _>()
    
    let getDomainValues i = [|0 .. domainSizes.[i] - 1|]

    member this.AddT(i, children) =
        let u = nodes.Count
        nodes.Add(Entry(i, children))
        u

    member this.Insert(i, children, u) =
        hash.Add((i, children), u)

    member this.MK(i, children: int[]) =
        let firstChild = children.[0]
        if Array.forall((=)firstChild) children
        then firstChild
        else
            match hash.TryGetValue((i, children)) with
            | true, v -> v
            | _ ->
                let u = this.AddT((i, children))
                this.Insert(i, children, u)
                u

    member this.BuildEnv pred =
        let rec build' (env, i) =
            if i >= n
            then
                if pred env then 1 else 0
            else
                let values =
                    Array.map (fun value -> build'(Map.add i value env, i + 1)) <| getDomainValues i
                this.MK(i, values)
        build'(Map.empty, 0)

    member this.Restrict(u, j, b) =
        let rec res u =
            let n = nodes.[u]
            match compare n.Var j with
            | 1 -> u
            | -1 -> this.MK(n.Var, Array.map res n.Children)
            | 0 -> res(n.Children.[b])
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
                        then
                            let values = getDomainValues e1.Var
                            this.MK(e1.Var, values |> Array.map (fun value -> app(e1.Children.[value], e2.Children.[value])))
                        else
                            if e1.Var < e2.Var
                            then
                                let values = getDomainValues e1.Var
                                this.MK(e1.Var, values |> Array.map (fun value -> app(e1.Children.[value], u2)))
                            else
                                let values = getDomainValues e2.Var
                                this.MK(e2.Var, values |> Array.map (fun value -> app(u1, e2.Children.[value])))
                memTable.[u1, u2] <- Some u
                u
        app(u1, u2)

    member this.ApplyN(op, us) =
        let memDic = new System.Collections.Generic.Dictionary<_, _>()
        let rec app (us: int[]) =
            match memDic.TryGetValue(us) with
            | true, u -> u
            | _ ->
                let u =
                    if Array.forall (fun u -> u = 0 || u = 1) us
                    then op us
                    else
                        let min = Seq.minBy (fun u -> nodes.[u].Var) us
                        let minVar = nodes.[min].Var
                        let values = getDomainValues minVar
                        this.MK(minVar, values |>
                                    Array.map
                                        (fun value ->
                                            app(
                                                us |> Array.map (fun u -> let n = nodes.[u]
                                                                          if n.Var = minVar
                                                                          then n.Children.[value]
                                                                          else u))))
                memDic.[us] <- u
                u
        app us

    member this.Compose (u1, x, u2) =
        let ite =
            function
            | [|x; y0; y1|] -> if x = 1 then y0 else y1
            | _ -> failwith "expected 3 args"
        this.ApplyN(ite, [|u1; x; u2|])

// bug: this only works for boolean vars
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
                eval n.Children.[vl]
        eval t

