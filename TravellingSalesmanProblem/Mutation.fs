module TravellingSalesmanProblem.Mutation

open System
open ProblemFunctions

let neighbour (random: Random) (pop: 'a array) =
    let i1 = random.Next(0, pop.Length - 1)
    let i2 = random.Next(i1, pop.Length - 1)
    
    let asArray = pop |> Array.copy
    let temp = asArray.[i1]
    asArray.[i1] <- asArray.[i2]
    asArray.[i2] <- temp
    asArray

let mutate (rand: Random) (rate: float) (pop: 'a array) =
    let asArray = pop |> Array.copy
    
    for i in 0..pop.Length - 1 do
        if (rand.NextDouble() < rate) then
            let i1 = rand.Next(0, pop.Length)
            let i2 = rand.Next(0, pop.Length)
            let temp = asArray.[i1]
            asArray.[i1] <- asArray.[i2]
            asArray.[i2] <- temp
            
    asArray

let flip (rand: Random) (pop: 'a array) =
    let a = rand.Next(0, pop.Length - 1)
    let start = pop |> Array.take a
    let end' = pop |> Array.skip a
    Array.append end' start

let scramble (rand: Random) (pop: 'a array) =
    let a = rand.Next(1, pop.Length - 1)
    let b = rand.Next(a, pop.Length - 1)
    
    let subSection =
        pop.[ a..b ]
        |> shuffle rand
    
    let scrambled = 
        [| for i = 0 to pop.Length - 1 do
             if i < a || i > b then
                 pop.[i]
             else
                 subSection.[i - a] |]

    scrambled

let inversion (rand: Random) (pop: 'a array) =
    let a = rand.Next(0, pop.Length - 1)
    let b = rand.Next(a, pop.Length - 1)
    let subSection =
        pop.[ a..b ]
        |> Array.rev
        
    let inverted =
        [| for i = 0 to pop.Length - 1 do
             if i < a || i > b then
                 pop.[i]
             else 
                 subSection.[i - a] |]

    inverted
    