module TravellingSalesmanProblem.Tests.Lookup

open TravellingSalesmanProblem
open TravellingSalesmanProblem.Types
open Xunit

let lookupXY = seq {
    let buildArr a b c =
        [| a :> obj
           b :> obj
           c :> obj |] |> box
    
    buildArr (0.0, 0.0) (0.0, 0.0) 0.0
    
    buildArr (2.0, 0.0) (0.0, 0.0) 2.0
    buildArr (0.0, 3.0) (0.0, 0.0) 3.0
    buildArr (0.0, 0.0) (4.0, 0.0) 4.0
    buildArr (0.0, 0.0) (0.0, 5.0) 5.0
   
    buildArr (-2.0, 0.0) (0.0, 0.0) 2.0
    buildArr (0.0, -3.0) (0.0, 0.0) 3.0
    buildArr (0.0, 0.0) (-4.0, 0.0) 4.0
    buildArr (0.0, 0.0) (0.0, -5.0) 5.0
    
    buildArr (0.0, 0.0) (5.0, 5.0) (50.0 |> sqrt)
    buildArr (6.0, 6.0) (0.0, 0.0) (72.0 |> sqrt)
    buildArr (6.0, 6.0) (-1.0, -1.0) (98.0 |> sqrt)
}

[<Fact>]
let ``Empty city list creates empty lookup``() =
    let lookup = Lookup.buildLookup [ ]
    
    Assert.Empty(lookup)

[<Theory; MemberData("lookupXY")>]
let ``Generated lookup table has correct distance between cities``(city1Coords, city2Coords, expected) =
    let city1: City = {
        Name = "1"
        Location = {
            X = city1Coords |> fst
            Y = city1Coords |> snd
        }
    }
    let city2: City = {
        Name = "2"
        Location = {
            X = city2Coords |> fst
            Y = city2Coords |> snd
        }
    }
    
    let lookup = Lookup.buildLookup [ city1; city2 ]
    
    let distance1 = lookup.[(city1, city2)]
    let distance2 = lookup.[(city2, city1)]
    
    Assert.Equal(expected, distance1)
    Assert.Equal(expected, distance2)
    