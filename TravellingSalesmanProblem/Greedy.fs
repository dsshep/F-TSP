module TravellingSalesmanProblem.Greedy

open System
open System.Collections.Generic
open TravellingSalesmanProblem.Types
open TravellingSalesmanProblem.Extensions

let calculate (lookup: IDictionary<(City * City), float>) (cities: City list) =
    
    let orderedLookup =
        lookup
        |> Seq.map (|KeyValue|)
        |> Seq.sortBy snd
        
    let final = Array.zeroCreate (cities.Length - 1)
    final.[0] <- orderedLookup |> Seq.last |> fst
    
    let orderedLookupList =
        orderedLookup
        |> Seq.toList
    
    let rec removeReverseRoutes curr (set: HashSet<_>) (acc: _ list) =
        let (a, b), c = curr
        let reversed = ((b, a), c)
        
        match set.TryGetValue curr, set.TryGetValue reversed with
        | (true, _), (true, _) ->
            set.Remove reversed |> ignore
        | _ -> ()
        
        match acc |> List.tryHead with
        | Some a ->
            removeReverseRoutes a set (acc |> List.tail)
        | None ->
            set |> Seq.toArray
    
    let lookupNoReverse =
        removeReverseRoutes
            (orderedLookupList |> Seq.head)
            (orderedLookupList |> HashSet)
            orderedLookupList

    let pointCount ps x =
        ps |> Array.filter (fun x' -> x' = x) |> Array.length
    
    let rec loop i lookup =
        if i = final.Length then
            ()
        else
            let notNulls =
                final
                |> Array.filter (fun x -> Object.ReferenceEquals(null, x) |> not)
                
            let allPoints =
                notNulls
                |> Array.Parallel.collect (fun (a, b) -> [| a; b |])
            
            let potentialCities, invalidCities =
                lookup
                |> Array.Parallel.partition (fun ((a, b), _) ->
                    let rec causesLoop wantToVisit cur acc =
                        if cur = wantToVisit then
                            true
                        else 
                            let route =
                                acc
                                |> Array.tryFind (fun (a, b) -> a = cur || b = cur)
                            
                            match route with
                            | Some x' ->
                                let a, b = x'
                                let nextPoint =
                                    if a = cur then b else a
                                let withoutRoute =
                                    acc
                                    |> Array.filter (fun x -> x' <> x)
                                causesLoop wantToVisit nextPoint withoutRoute
                            | None -> false

                    let doesntCauseLoop = lazy (causesLoop a b notNulls |> not)
                    let aPointCount = lazy ((pointCount allPoints a) < 2)
                    let bPointCount = lazy ((pointCount allPoints b) < 2)
                    
                    doesntCauseLoop.Value && aPointCount.Value && bPointCount.Value)
                
            let nextCityOpt =
                potentialCities
                |> Array.sortBy snd
                |> Array.tryHead
                |> Option.map fst
                
            match nextCityOpt with
            | Some n -> 
                final.[i] <- n
                
                let invalidLookup = invalidCities |> HashSet
                   
                let newLookup =
                    lookup
                    |> Array.Parallel.choose (fun x ->
                        let notInLoopList = invalidLookup.Contains x |> not
                        if notInLoopList then Some x else None)
                   
                loop (i + 1) newLookup
            | None ->
                failwith "Couldn't find suitable city"
    
    loop 1 lookupNoReverse
    
    let entryPoints =
        final
        |> Array.toList
        |> List.collect (fun (a, b) -> [ a; b ])
        |> List.countBy id
        |> List.filter (snd >> (=) 1)
        |> List.map fst
        |> List.toArray
        
    let rec stitchPoints at remaining acc =
        let nextLocationOpt =
            remaining
            |> Array.Parallel.choose (fun x ->
                let exists = x |> fst = at || x |> snd = at
                if exists then Some x else None)
            |> Array.tryHead
            
        match nextLocationOpt with
        | Some (a, b) ->
            let nextCity = 
                if a = at then b else a 
            let accumulation = acc @ [ nextCity ]
            stitchPoints nextCity (remaining |> Array.filter ((<>) (a, b))) accumulation
        | None ->
            acc
        
    let stitched =
        stitchPoints (entryPoints.[0]) final [ entryPoints.[0] ]
        
    let distance = ProblemFunctions.calculateRouteDistance lookup stitched
    
    (distance, stitched)