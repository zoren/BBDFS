module BDDFS

type Variable = int

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

type Builder(variables: int) =
    let T = new System.Collections.Generic.List<_>([|(variables + 1, None, None); (variables + 1, None, None)|])
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

[<EntryPoint>]
let main argv =
    let env = Map.ofList [1, true]
    printfn "%A" <| eval (fun v -> Map.find v env) test
    0 // return an integer exit code
