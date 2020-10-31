module TravellingSalesmanProblem.Mutation

open System
open ProblemFunctions

let neighbour (random: Random) (pop: 'a list) =
    let i1 = random.Next(0, pop.Length - 1)
    let i2 = random.Next(i1, pop.Length - 1)
    
    let asArray = pop |> List.toArray
    let temp = asArray.[i1]
    asArray.[i1] <- asArray.[i2]
    asArray.[i2] <- temp
    asArray |> Array.toList

let mutate (rand: Random) (rate: float) (pop: _ list) =
    let asArray = pop |> List.toArray
    
    for i in 0..pop.Length - 1 do
        if (rand.NextDouble() < rate) then
            let i1 = rand.Next(0, pop.Length)
            let i2 = rand.Next(0, pop.Length)
            let temp = asArray.[i1]
            asArray.[i1] <- asArray.[i2]
            asArray.[i2] <- temp
            
    asArray |> Array.toList

let flip (rand: Random) (pop: 'a list) =
    let a = rand.Next(0, pop.Length - 1)
    let start = pop |> List.take a
    let end' = pop |> List.skip a
    end' @ start

let scramble (rand: Random) (pop: 'a list) =
    let a = rand.Next(1, pop.Length - 1)
    let b = rand.Next(a, pop.Length - 1)
    
    let subSection =
        pop.[ a..b ]
        |> shuffle rand
    
    let scrambled = 
        [ for i = 0 to pop.Length - 1 do
            if i < a || i > b then
                pop.[i]
            else
                subSection.[i - a] ]

    scrambled

let inversion (rand: Random) (pop: 'a list) =
    let a = rand.Next(0, pop.Length - 1)
    let b = rand.Next(a, pop.Length - 1)
    let subSection =
        pop.[ a..b ]
        |> List.rev
        
    let inverted =
        [ for i = 0 to pop.Length - 1 do
            if i < a || i > b then
                pop.[i]
            else 
                subSection.[i - a] ]

    inverted
    