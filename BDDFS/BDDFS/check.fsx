#r "../../packages/FsCheck/lib/net452/FsCheck"

open FsCheck

#load "Builder.fs"

open BDD

let prependAll e ll = List.map (fun l -> e::l) ll

let prod (xs: 'a list) (yss: 'a list list) = xs |> List.collect (fun x -> prependAll x yss)

let buildDoms (domSizes: int[]) =
    let rec loop i =
        if i = domSizes.Length
        then [[]]
        else prod [0 .. domSizes.[i] - 1 ] <| loop (i+1)
    loop 0

let (.=.) left right = left = right |@ sprintf "%A = %A" left right

let testBDD3 (Fun pred) = 
    let b = Builder(3)
    let bddTopIndex = b.BuildEnv pred
    let p = fun e -> b.Eval(e, bddTopIndex) = pred e 
    let e = buildDoms [|2;2;2|]
    e |> List.forall (fun vals ->
                        let env = Map.ofSeq << Seq.map(fun(i, x) -> (i + 1, x)) <| Seq.indexed vals
                        p env)

Check.Quick testBDD3

#load "NDDBuilder.fs"

open NDD

let testNDD3 (Fun pred) =
    let domSizes = [|2;3;2|];
    let b = NDDBuilder(domSizes)
    let bddTopIndex = b.BuildEnv pred
    let p = fun e -> b.Eval(e, bddTopIndex) = pred e 
    let e = buildDoms domSizes
    e |> List.forall (fun vals ->
                        let env = Map.ofSeq <| Seq.indexed vals
                        p env)

Check.Quick testNDD3
