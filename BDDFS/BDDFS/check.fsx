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

let tryGetOpt (dic:System.Collections.Generic.Dictionary<_, _>) k =
    match dic.TryGetValue k with
    | true, s -> Some s
    | _ -> None
    
type BinOp = And | Or

type Exp<'Variable, 'Value> =
    | VarEq of 'Variable * 'Value
    | Not of Exp<'Variable, 'Value>
    | BinOp of Exp<'Variable, 'Value> * BinOp * Exp<'Variable, 'Value>

let evalExp env =
    let rec ev =
        function
        | VarEq (var, value) -> Map.find var env = value
        | Not e -> not <| ev e
        | BinOp(e1, op, e2) ->
            let v1, v2 = ev e1, ev e2
            let f =
                match op with
                | And -> (&&)
                | Or -> (||)
            f v1 v2
    ev

let mapExp f =
    let rec ev =
        function
        | VarEq (var, value) -> VarEq <| f (var, value)
        | Not e -> Not <| ev e
        | BinOp(e1, op, e2) -> BinOp(ev e1, op, ev e2)
    ev

open System.Collections.Generic

let getDomain exp =
    let dic = Dictionary<_, _>()
    let rec loop =
        function
        | VarEq (var, value) ->
            let s =
                match tryGetOpt dic var with
                | Some s -> s
                | None ->
                    let s = HashSet<_>()
                    dic.Add(var, s)
                    s
            ignore <| s.Add value        
        | Not e -> loop e
        | BinOp(e1, op, e2) ->
            loop e1
            loop e2
    loop exp
    dic

let testT (inpExp: Exp<int, int>) =
    let strDom = getDomain inpExp
    let getVarIndex v = strDom.Keys |> Seq.findIndex((=)v)
    let getValueIndex v value = strDom.[v] |> Seq.findIndex((=)value)
    let exp =
        mapExp (fun (v, value) -> getVarIndex v, getValueIndex v value) inpExp
    let dom = getDomain exp
    let maxVar = dom.Keys |> Seq.max
    let vars = [|0 .. maxVar|]
    let domSizes = Array.map (fun v -> defaultArg (Option.map seq <| tryGetOpt dom v) (seq [1]) |> Seq.max |> (+)1 |> max 2)  vars
    let b = NDDBuilder(domSizes)
    let pred = fun env -> evalExp env exp
    let bddTopIndex = b.BuildEnv pred
    let p = fun e -> b.Eval(e, bddTopIndex) = pred e 
    let e = buildDoms domSizes
    e |> List.forall (fun vals ->
                        let env = Map.ofSeq <| Seq.indexed vals
                        p env)

Check.Quick testT
