module TravellingSalesmanProblem.Selection

let private snd (_, a', _) = a'

let private thd (_, _, a') = a'

let tournament (numTournaments) (population: 'a array array) (fitness: float array) =
    let grouped =
        population
        |> Array.mapi (fun i p -> (i, p, fitness.[i]))
        |> Array.groupBy (fun (i, _, _) -> i % numTournaments)
    
    [ for g in grouped do
          let _, b = g
          b
          |> Array.maxBy thd
          |> snd ]
    