module TravellingSalesmanProblem.BruteForce

open System
open TravellingSalesmanProblem.ProblemFunctions
open TravellingSalesmanProblem.Types

let calculate lookup (cities: City list) =
    let rec permutations list taken = 
        seq { if Set.count taken = List.length list then yield [] else
                for l in list do
                    if not (Set.contains l taken) then 
                        for perm in permutations list (Set.add l taken)  do
                            yield l::perm }

    let mutable recordDistance = calculateRouteDistance lookup cities
    let mutable recordOrder = cities
    
    permutations cities Set.empty
    |> Seq.iter (fun cityOrder ->
        let distance = calculateRouteDistance lookup cityOrder
        if distance < recordDistance then
          recordDistance <- distance
          recordOrder <- cityOrder)
    
    (recordDistance, recordOrder)
    
let localBruteForce (rand: Random) (lookup) (cities: City list) =
    let chunkSize = cities.Length / (rand.Next(3, 4))
    let ret =
        cities
        |> List.indexed
        |> List.groupBy (fun (i, _) -> i % chunkSize)
        |> List.collect (fun (_, x) ->
            let s = x |> List.map (fun (_, a) -> a)
            calculate lookup s |> snd)
    ret
    
let localBruteForce2 (rand: Random) (lookup) (cities: City list) =
    let amountToCalculate = 6
    let startPoint = rand.Next(0, cities.Length - 1 - amountToCalculate)
    
    let section =
        [ for i = startPoint to amountToCalculate + startPoint - 1 do
            cities.[i] ]
        |> calculate lookup
        |> snd

    let ret = 
        [ for i = 0 to cities.Length - 1 do
              if i < startPoint || i > startPoint + amountToCalculate - 1 then
                  cities.[i]
              else
                  section.[i - startPoint] ]
    ret
