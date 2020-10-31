[<AutoOpen>]
module TravellingSalesmanProblem.Extensions

[<RequireQualifiedAccess>]
module Array =
    let tryFindValueIndex v (arr: _[]) =
        let rec loop i =
            if i = arr.Length then ValueNone
            elif arr.[i] = v then ValueSome i
            else loop (i + 1)
        loop 0
        
    let tryFindValue f (arr: _[]) =
        let rec loop i =
            if i = arr.Length then ValueNone
            elif f arr.[i] then ValueSome arr.[i]
            else loop (i + 1)
        loop 0
        