module TravellingSalesmanProblem.BruteForce

open System
open TravellingSalesmanProblem.ProblemFunctions
open TravellingSalesmanProblem.Types

let calculate lookup (cities: City array) =
    let rec permutations arr taken = 
        seq { if Set.count taken = Array.length arr then yield [||] else
                for elm in arr do
                    if not (Set.contains elm taken) then 
                        for perm in permutations arr (Set.add elm taken)  do
                            yield [| elm |] |> Array.append perm }

    let mutable recordDistance = calculateRouteDistance lookup cities
    let mutable recordOrder = cities
    
    permutations cities Set.empty
    |> Seq.iter (fun cityOrder ->
        let distance = calculateRouteDistance lookup cityOrder
        if distance < recordDistance then
          recordDistance <- distance
          recordOrder <- cityOrder)
    
    (recordDistance, recordOrder)
    
let localBruteForce (rand: Random) (lookup) (cities: City array) =
    let chunkSize = cities.Length / (rand.Next(3, 4))
   
    cities  
    |> Array.indexed
    |> Array.groupBy (fun (i, _) -> i % chunkSize)
    |> Array.collect (fun (_, x) ->
        let s = x |> Array.map snd
        calculate lookup s |> snd)
    