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
    
type BinOp =
    And | Or

type InpExp<'Variable, 'Value> =
    | VarEq of 'Variable * 'Value
    | Not of InpExp<'Variable, 'Value>
    | BinOp of InpExp<'Variable, 'Value> * BinOp * InpExp<'Variable, 'Value>

type Op = And | Or | Not

type Exp<'Variable, 'Value> =
    | VarEq of 'Variable * 'Value
    | Call of Op * Exp<'Variable, 'Value>[]

let rec convert =
    function
    | InpExp.VarEq(v, vl) -> VarEq(v, vl)
    | InpExp.Not e -> Call(Not, [|convert e|])
    | InpExp.BinOp(e1, op, e2) ->
        let f =
            match op with
            | BinOp.And -> Op.And
            | BinOp.Or -> Op.Or
        Call(f, [|convert e1; convert e2|])

let evalFunc op vs =
    match op, vs with
    | And, [|b1; b2|] -> b1 && b2
    | Or, [|b1; b2|] -> b1 || b2
    | Not, [|b|] -> not b
    | _ -> failwithf "arity error"

let evalExp env =
    let rec ev =
        function
        | VarEq (var, value) -> Map.find var env = value
        | Call(op, es) -> evalFunc op <| Array.map ev es
    ev

let mapExp f =
    let rec ev =
        function
        | VarEq (var, value) -> VarEq <| f (var, value)
        | Call(op, es) -> Call(op, Array.map ev es)
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
        | Call(_, es) -> Array.iter loop es
    loop exp
    dic

let testT (inpExp: InpExp<int, int>) =
    let e = convert inpExp
    let strDom = getDomain e
    let getVarIndex v = strDom.Keys |> Seq.findIndex((=)v)
    let getValueIndex v value = strDom.[v] |> Seq.findIndex((=)value)
    let exp =
        mapExp (fun (v, value) -> getVarIndex v, getValueIndex v value) e
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

let getTuple = 
    function 
    | [|x;y|] -> x,y
    | _ -> failwith "expected two elements"

let getSingleton = 
    function 
    | [|x|] -> x
    | _ -> failwith "expected one element"

// build ndd using apply
let buildNDD (b: NDDBuilder) =
    let rec mk e =
        match e with
        | VarEq(v, value) -> b.BuildEnv (fun env -> Map.find v env = value)
        | Call(f, args) ->
            let eargs = Array.map mk args
            let func =
                match f with
                | And -> getTuple >> fun (i0, i1) -> i0 && i1
                | Or -> getTuple >> fun (i0, i1) ->  i0 || i1
                | Not -> getSingleton >> not
            b.ApplyN((fun is -> if func <| Array.map ((=)1) is then 1 else 0), eargs)
    mk

let testBuildUsingApply (inpExp: InpExp<int, int>) =
    let e = convert inpExp
    let strDom = getDomain e
    let getVarIndex v = strDom.Keys |> Seq.findIndex((=)v)
    let getValueIndex v value = strDom.[v] |> Seq.findIndex((=)value)
    let exp =
        mapExp (fun (v, value) -> getVarIndex v, getValueIndex v value) e
    let dom = getDomain exp
    let maxVar = dom.Keys |> Seq.max
    let vars = [|0 .. maxVar|]
    let domSizes = Array.map (fun v -> defaultArg (Option.map seq <| tryGetOpt dom v) (seq [1]) |> Seq.max |> (+)1 |> max 2)  vars
    let b = NDDBuilder(domSizes)
    let bddTopIndex = buildNDD b exp
    let pred = fun env -> evalExp env exp
    let p = fun e -> b.Eval(e, bddTopIndex) = pred e 
    let e = buildDoms domSizes
    e |> List.forall (fun vals ->
                        let env = Map.ofSeq <| Seq.indexed vals
                        p env)

Check.Quick testBuildUsingApply
