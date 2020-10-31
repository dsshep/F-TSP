module TravellingSalesmanProblem.Selection

let private snd (_, a', _) = a'

let private thd (_, _, a') = a'

let tournament (numTournaments) (population: 'a list list) (fitness: float list) =
    let grouped =
        population
        |> List.mapi (fun i p -> (i, p, fitness.[i]))
        |> List.groupBy (fun (i, _, _) -> i % numTournaments)
    
    [ for g in grouped do
          let _, b = g
          b
          |> List.maxBy thd
          |> snd ]
    