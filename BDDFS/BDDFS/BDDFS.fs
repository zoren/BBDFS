open Lang
open BDD

module BDDFS =

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
        let bddTopIndex = b.BuildEnv t
        let bdd = b.GetExport bddTopIndex
        let env = Map.ofList [1, true; 2, false; 3, true]
        printfn "%A" <| eval (fun v -> Map.find v env) bdd
        printfn "%A" <| evalExp (fun v -> Map.find v env) t
        let t = b.Apply((fun(b1, b2) -> if b1 = 1 || b2 = 1 then 1 else 0), bddTopIndex, 1)
        printfn "%A" <| t
        let t2 = b.ApplyN((fun([|b1; b2|]) -> if b1 = 1 || b2 = 1 then 1 else 0), [|bddTopIndex; 1|])
        printfn "%A" <| t2
//        printfn "%A" << eval (fun v -> failwith "hej") <| b.GetExport t
        0 // return an integer exit code
