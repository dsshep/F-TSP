module TravellingSalesmanProblem.ProblemFunctions

open System
open System.Collections.Generic
open TravellingSalesmanProblem.Types

type PopulationFitness = {
    Record: (float * City array) option
    Fitness: float array
}

let calculateRouteDistance (distanceLookup: IDictionary<_,_>) (route: City array) =
    let rec loop (r: City array) (acc: float) =
        match r with
        | [| _ |] -> acc
        | _ ->
            let previous = r.[0]
            let next = r.[1]
            let distance = distanceLookup.[(previous, next)]
            loop (r |> Array.tail) (acc + distance)
    loop route 0.0

let shuffle (rand: Random) arr =
    let asArray = arr |> Array.copy
    Array.iteri (fun i _ ->
        let tmp = asArray.[i]
        let randomIndex = (rand.Next(i, Array.length asArray))
        asArray.[i] <- asArray.[randomIndex]
        asArray.[randomIndex] <- tmp)
        asArray
    asArray
    
let fitnessOfPopulation (distanceLookup) (recordDistance: float) (population: City array array) =
    let mutable record: (float * City array) option = None
    
    let fitnessValues = 
        population
        |> Array.map (fun p ->
            let distance = calculateRouteDistance distanceLookup p
            if distance < recordDistance then
                record <- Some (distance, p)
            1.0 / ((distance ** 8.0) + 1.0))
        
    let totalFitness =
        fitnessValues |> Array.sum
    let fitness = 
        fitnessValues
        |> Array.map (fun f -> (f / totalFitness))
    
    { Record = record; Fitness = fitness  }