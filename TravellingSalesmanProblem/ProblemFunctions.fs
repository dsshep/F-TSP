module TravellingSalesmanProblem.ProblemFunctions

open System
open System.Collections.Generic
open TravellingSalesmanProblem.Types

type PopulationFitness = {
    Record: (float * City list) option
    Fitness: float list
}

let calculateRouteDistance (distanceLookup: IDictionary<_,_>) (route: City list) =
    let rec loop (r: City list) (acc: float) =
        match r with
        | [ _ ] -> acc
        | _ ->
            let previous = r.[0]
            let next = r.[1]
            let distance = distanceLookup.[(previous, next)]
            loop (r |> List.tail) (acc + distance)
    loop route 0.0

let shuffle (rand: Random) l =
    let asArray = l |> List.toArray
    Array.iteri (fun i _ ->
        let tmp = asArray.[i]
        let randomIndex = (rand.Next(i, Array.length asArray))
        asArray.[i] <- asArray.[randomIndex]
        asArray.[randomIndex] <- tmp)
        asArray
    asArray |> Array.toList
    
let fitnessOfPopulation (distanceLookup) (recordDistance: float) (population: City list list) =
    let mutable record: (float * City list) option = None
    
    let fitnessValues = 
        population
        |> List.map (fun p ->
            let distance = calculateRouteDistance distanceLookup p
            if distance < recordDistance then
                record <- Some (distance, p)
            1.0 / ((distance ** 8.0) + 1.0))
        
    let totalFitness =
        fitnessValues |> List.sum
    let fitness = 
        fitnessValues
        |> List.map (fun f -> (f / totalFitness))
    
    { Record = record; Fitness = fitness  }