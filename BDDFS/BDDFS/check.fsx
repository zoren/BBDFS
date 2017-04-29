#r "../../packages/FsCheck/lib/net452/FsCheck"

open FsCheck

#load "Builder.fs"

open BDD

let prependAll e ll = List.map (fun l -> e::l) ll

let prod (xs: 'a list) (yss: 'a list list) = xs |> List.map (fun x -> prependAll x yss) |> List.concat

let (.=.) left right = left = right |@ sprintf "%A = %A" left right

let testBDD3 (Fun pred) = 
    let b = Builder(3)
    let bddTopIndex = b.BuildEnv pred
    let p = fun e -> b.Eval(e, bddTopIndex) = pred e 
    let e = prod [0; 1] << prod [0; 1] <| prod [0; 1] [[]]
    e |> List.forall (fun vals ->
                        let env = Map.ofSeq << Seq.map(fun(i, x) -> (i + 1, x)) <| Seq.indexed vals
                        p env)

Check.Quick testBDD3
