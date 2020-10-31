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
        Population: City list list
        PopulationFitness: PopulationFitness
        GenerationBests: float list
        MutationRate: float
        Record: (float * City list)
        Elapsed: TimeSpan
        DiversityPercentage: int
    }
    
    type Progress = {
        Generation: int
        GenerationBest: City list
        GenerationBestScore: float
        Best: City list
        BestScore: float
    }

    let calculate seed (progress: Progress -> unit) costFunction lookup (cities: City list) = 
        let random = Random(seed)
        let crossOver =
            Crossover.partiallyMatchedRandom random
            //Crossover.cycle
            //Crossover.primitive random
            //Crossover.cycle2
        let totalIterations = 3 * cities.Length
        let populationCount = 400
        let allGenerations = HashSet<_>(totalIterations * populationCount)
        
        let run() =
            let sw = Stopwatch.StartNew()
            let fitnessOfPopulation = ProblemFunctions.fitnessOfPopulation lookup
            let initialPopulation =
                [ 1..populationCount ]
                |> List.map (fun _ -> Mutation.flip random cities)
                |> List.distinct
                
            let initialPopulation =
                initialPopulation @
                ([ 1..(populationCount - initialPopulation.Length) ]
                |> List.map (fun _ -> Mutation.inversion random cities))
            
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
                                    //Mutation.inversion random o
                                    //Mutation.neighbour random o
                                    //Mutation.mutate random state.MutationRate o
                                else
                                    //BruteForce.localBruteForce2 random lookup o
                                    //Mutation.neighbour random o
                                    Mutation.inversion random o
                                    //Mutation.mutate random state.MutationRate o
                                
                            let b = Mutation.inversion random o //scramble
                            let c = Mutation.inversion random o
                            
                            [| o; a; b; c |])
                    
                    let populationList = 
                        population |> Array.toList
                        
                    let newFitness =
                        fitnessOfPopulation
                            (fst state.Record)
                            (populationList)
                    
                    let bestFitness = newFitness.Fitness |> List.max
                    
                    let generationBestIndex =
                        newFitness.Fitness
                        |> List.findIndex (fun f -> f = bestFitness)
                        
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
                        TotalIterations = state.TotalIterations
                        CurrentIteration = state.CurrentIteration + 1
                        Population = populationList
                        PopulationFitness = newFitness
                        GenerationBests = state.GenerationBests @ [ best ]
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
                GenerationBests = []
                MutationRate = 10.0
                Record = initialFitness.Record.Value
                Elapsed = sw.Elapsed
                DiversityPercentage = 0
            }
        try 
            run()
        with e ->
            failwithf "Genetic algorithm failure: %A" e
            