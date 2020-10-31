module TravellingSalesmanProblem.Crossover

open System

type CrossoverAlgorithm<'a> = ('a list) -> ('a list) -> ('a list * 'a list)

let cycle : CrossoverAlgorithm<_> = fun p1 p2 ->
    let o1 = Array.zeroCreate p1.Length
    let o2 = Array.zeroCreate p1.Length
    o1.[0] <- p1.[0]
    o2.[0] <- p2.[0]
    
    let rec loop i =
        if o1 |> Array.contains p2.[i] then
            ()
        else 
            let j = p1 |> List.findIndex (fun x -> x = p2.[i])
            o1.[j] <- p1.[j]
            o2.[j] <- p2.[j]
            loop j
    
    loop 0
    
    let missingGenes =
        p1
        |> List.indexed
        |> List.filter (fun (_, g) -> o1 |> Array.contains g |> not)
        |> List.map fst

    for idx in missingGenes do
        o1.[idx] <- p2.[idx]
        o2.[idx] <- p1.[idx]
            
    (o1 |> Array.toList, o2 |> Array.toList)
        
// Not safe - doesn't work for cases where p' are similar.
let cycle2 : CrossoverAlgorithm<_> = fun p1 p2 ->
    let o1 = Array.zeroCreate p1.Length
    let o2 = Array.zeroCreate p1.Length
    
    let step1 i p1Val (p1: _ list) (p2: _ list) =
        o1.[i] <- p1Val
        let pos = p1 |> List.findIndex (fun x -> x = o1.[i])
        let posInOne = p1 |> List.findIndex (fun x -> x = p2.[pos])
        o2.[i] <- p2.[posInOne]
        p1 |> List.findIndex (fun x -> x = o2.[i])
    
    let selectedBit = step1 0 p2.[0] p1 p2
    
    let rec loop exitCondition (p1: _ list) (p2: _ list) selected i =
        if exitCondition() || i = o1.Length then
            i
        else
            o1.[i] <- p2.[selected]
            let posInOne = p1 |> List.findIndex (fun x -> x = o1.[i])
            let valInTwo = p2.[posInOne] //5
            let posInTwo = p1 |> List.findIndex (fun x -> x = valInTwo)
            o2.[i] <- p2.[posInTwo]
            let nextSelect = p1 |> List.findIndex (fun x -> x = o2.[i])
            loop exitCondition p1 p2 nextSelect (i + 1)

    let finalI = loop (fun () -> o2 |> Array.contains p1.[0]) p1 p2 selectedBit 1
    
    if finalI <> o1.Length then
        let missingP1 = p1 |> List.filter (fun x -> o1 |> Array.contains x |> not)
        let missingP2 = p2 |> List.filter (fun x -> o1 |> Array.contains x |> not)
        
        let selectedBit = step1 finalI missingP2.[0] missingP1 missingP2
        
        loop (fun () -> false) missingP1 missingP2 selectedBit (finalI + 1) |> ignore
    
    (o1 |> Array.toList, o2 |> Array.toList)
        
let primitive (rand: Random) : CrossoverAlgorithm<_> = fun (p1: _ list) (p2: _ list) ->
    let halfLength = p1.Length / 2
    let startPos = rand.Next(0, p1.Length / 2)
    let amount = rand.Next(0, halfLength)
    
    let aIndexes = 
        [ for i in startPos..startPos + amount do
            if i > p1.Length - 1 then
                startPos - p1.Length - 1
            else i ]
        
    let aCities = aIndexes |> List.map (fun i -> p1.[i])
    let bCities = p2 |> List.except aCities
    let res = aCities @ bCities
    (res, res)
    
let partiallyMatched a b : CrossoverAlgorithm<_> = fun p1 p2 ->
    let o1 = Array.zeroCreate p1.Length
    let o2 = Array.zeroCreate p1.Length
    
    for i in a..b - 1 do
        o1.[i] <- p2.[i]
        o2.[i] <- p1.[i]
        
    let rec findValue i' (p1': _ list) (p2': _ list) (o: _[]) =
        let idxOpt = o |> Array.tryFindValueIndex p1'.[i']
        match idxOpt with
        | ValueSome _ ->
            let idxIn2 = p2' |> List.findIndex (fun x -> x = p1'.[i'])
            findValue idxIn2 p1' p2' o
        | ValueNone ->
            p1'.[i']
    
    for i = 0 to p1.Length - 1 do
        if i < a || i >= b then
            o1.[i] <- findValue i p1 p2 o1
            o2.[i] <- findValue i p2 p1 o2
        
    (o1 |> Array.toList, o2 |> Array.toList)

let partiallyMatchedRandom (rand: Random) : CrossoverAlgorithm<_> = fun p1 p2 ->
    let a = rand.Next(0, p1.Length)
    let b = rand.Next(a, p1.Length)
    partiallyMatched a b p1 p2
