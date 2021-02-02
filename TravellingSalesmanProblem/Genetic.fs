namespace TravellingSalesmanProblem

open System
open System.Collections.Generic
open System.Diagnostics
open TravellingSalesmanProblem.ProblemFunctions
open TravellingSalesmanProblem.Types

[<RequireQualifiedAccess>]
module Genetic =

    type CalculationResult = {
        TotalIterations: int
        CurrentIteration: int
        Population: City array array
        PopulationFitness: PopulationFitness
        GenerationBests: float array
        MutationRate: float
        Record: (float * City array)
        Elapsed: TimeSpan
        DiversityPercentage: int
    }
    
    type Progress = {
        Generation: int
        GenerationBest: City array
        GenerationBestScore: float
        Best: City array
        BestScore: float
    }

    let calculate seed (progress: Progress -> unit) costFunction lookup (cities: City array) = 
        let random = Random(seed)
        let crossOver =
            Crossover.partiallyMatchedRandom random
        let totalIterations = 3 * cities.Length
        let populationCount = 400
        let allGenerations = HashSet<_>(totalIterations * populationCount)
        
        let run() =
            let sw = Stopwatch.StartNew()
            let fitnessOfPopulation = ProblemFunctions.fitnessOfPopulation lookup
            let initialPopulation =
                [| 1..populationCount |]
                |> Array.map (fun _ -> Mutation.flip random cities)
                |> Array.distinct
                
            let initialPopulation =
                Array.append
                    initialPopulation
                    ([| 1..(populationCount - initialPopulation.Length) |]
                    |> Array.map (fun _ -> Mutation.inversion random cities))
                
            let initialFitness = fitnessOfPopulation infinity initialPopulation
            
            let rec loop state =
                for p in state.Population do
                    allGenerations.Add p |> ignore
                    
                if state.TotalIterations = state.CurrentIteration then
                    state
                else
                    let selection =
                        Selection.tournament
                            (populationCount / 8)
                            state.Population
                            state.PopulationFitness.Fitness
                    
                    let population =
                        [| for i in 0..2..selection.Length - 1 do i |]
                        |> Array.collect (fun i -> 
                           let a = selection.[i]
                           let b = selection.[i + 1]
                           let o1, o2 = crossOver a b
                           let o3, o4 = Crossover.cycle a b
                           [| o1; o2; o3; o4 |])
                        |> Array.Parallel.collect (fun o ->
                            let a =
                                if state.CurrentIteration % 3 = 0 then
                                    BruteForce.localBruteForce random lookup o
                                else
                                    Mutation.inversion random o
                                
                            let b = Mutation.inversion random o
                            let c = Mutation.inversion random o
                            
                            [| o; a; b; c |])
                    
                    let populationList = 
                        population
                        
                    let newFitness =
                        fitnessOfPopulation
                            (fst state.Record)
                            (populationList)
                    
                    let bestFitness = newFitness.Fitness |> Array.max
                    
                    let generationBestIndex =
                        newFitness.Fitness
                        |> Array.findIndex (fun f -> f = bestFitness)
                        
                    let best = population.[generationBestIndex] |> costFunction
                    
                    progress {
                        Generation = state.CurrentIteration
                        GenerationBest = population.[generationBestIndex]
                        GenerationBestScore = best
                        BestScore = fst state.Record
                        Best = snd state.Record
                    }
                    
                    let newMutationRate = 
                        if state.MutationRate > 0.2 then state.MutationRate - 0.1
                        else state.MutationRate
                    
                    let record =
                        match newFitness.Record with
                        | Some r -> r
                        | None -> state.Record
                    
                    loop {
                        state with
                            CurrentIteration = state.CurrentIteration + 1
                            Population = populationList
                            PopulationFitness = newFitness
                            GenerationBests = (Array.append state.GenerationBests [| best |])
                            MutationRate = newMutationRate
                            Record = record
                            Elapsed = sw.Elapsed
                            DiversityPercentage =
                                ((allGenerations.Count |> float) / ((totalIterations * populationCount) |> float)) * 100.0 |> int
                    }
                    
            loop {
                TotalIterations = totalIterations
                CurrentIteration = 0
                Population = initialPopulation
                PopulationFitness = initialFitness
                GenerationBests = Array.empty
                MutationRate = 10.0
                Record = initialFitness.Record.Value
                Elapsed = sw.Elapsed
                DiversityPercentage = 0
            }
        try 
            run()
        with e ->
            printfn "Genetic algorithm failure: %A" e
            reraise()
            