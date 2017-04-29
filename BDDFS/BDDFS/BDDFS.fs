open BDD
open NDD

module BDDFS =
    let pred =
        fun env ->
            let e v = Map.find v env 
            let v1, v2, v3 = e 1, e 2, e 3
            v1 = v2 || v3 = 1

    let testBDD() =
        let b = Builder(3)
        let bddTopIndex = b.BuildEnv pred
        let env = Map.ofList [1, 1; 2, 0; 3, 1]
        printfn "%A" <| b.Eval (env, bddTopIndex)
        printfn "%A" <| pred env
        let t = b.Apply((fun(b1, b2) -> if b1 = 1 || b2 = 1 then 1 else 0), bddTopIndex, 1)
        printfn "%A" <| t
        let t2 = b.ApplyN((fun([|b1; b2|]) -> if b1 = 1 || b2 = 1 then 1 else 0), [|bddTopIndex; 1|])
        printfn "%A" <| t2

    let testNDD() =
        let b = NDDBuilder([|2;2;2|])
        let bddTopIndex = b.BuildEnv pred
        let env = Map.ofList [1, 1; 2, 0; 3, 1]
        printfn "%A" <| b.Eval (env, bddTopIndex)
        printfn "%A" <| pred env
        let t = b.Apply((fun(b1, b2) -> if b1 = 1 || b2 = 1 then 1 else 0), bddTopIndex, 1)
        printfn "%A" <| t
        let t2 = b.ApplyN((fun([|b1; b2|]) -> if b1 = 1 || b2 = 1 then 1 else 0), [|bddTopIndex; 1|])
        printfn "%A" <| t2

    [<EntryPoint>]
    let main argv =
        testBDD()
        testNDD()
//        printfn "%A" << eval (fun v -> failwith "hej") <| b.GetExport t
        0 // return an integer exit code
