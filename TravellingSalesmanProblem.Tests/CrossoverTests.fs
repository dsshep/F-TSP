module TravellingSalesmanProblem.Tests.Crossover

open Xunit
open TravellingSalesmanProblem

// Crossover algorithm papers:
// http://downloads.hindawi.com/journals/cin/2017/7430125.pdf
// https://arxiv.org/pdf/1203.3097.pdf

let private assertNoDuplicates l =
    let count = l |> List.distinct |> List.length
    Assert.Equal (l.Length, count)

let cycleData = seq {
    box [|
         ([ 1; 2; 3; 4; 5; 6; 7; 8; ], [ 8; 5; 2; 1; 3; 6; 4; 7; ])
         ([ 1; 5; 2; 4; 3; 6; 7; 8; ], [ 8; 2; 3; 1; 5; 6; 4; 7; ])
    |]
    box [|
         ([ 1; 2; 3; 4; 5; 6; 7; ], [ 7; 5; 1; 3; 2; 6; 4; ])
         ([ 1; 5; 3; 4; 2; 6; 7; ], [ 7; 2; 1; 3; 5; 6; 4; ])
    |]
}

[<Theory; MemberData("cycleData")>]
let cycle ((parents: int list * int list), (expected: int list * int list)) =
    let parent1, parent2 = parents
    
    let child1, child2 = Crossover.cycle parent1 parent2
    
    let expectedChild1, expectedChild2 = expected
    
    assertNoDuplicates child1
    assertNoDuplicates child2
    Assert.Equal<List<_>> (expectedChild1, child1)
    Assert.Equal<List<_>> (expectedChild2, child2)

let cycle2Data = seq {
    box [|
         ([ 3; 4; 8; 2; 7; 1; 6; 5 ], [ 4; 2; 5; 1; 6; 8; 3; 7 ])
         ([ 4; 8; 6; 2; 5; 3; 1; 7 ], [ 1; 7; 4; 8; 6; 2; 5; 3 ])
    |]
    box [|
         ([ 1; 2; 3; 4; 5; 6; 7; 8 ], [ 2; 7; 5; 8; 4; 1; 6; 3 ])
         ([ 2; 1; 6; 7; 5; 3; 8; 4 ], [ 6; 7; 2; 1; 8; 4; 5; 3 ])
    |]
    (*box [|
         ([ 1; 2; 3; 4; 5; 6; 7; 8 ], [ 1; 2; 3; 4; 5; 6; 8; 7; ])
         ([ 2; 1; 6; 7; 5; 3; 8; 4 ], [ 6; 7; 2; 1; 8; 4; 5; 3 ])
    |]*)
}

[<Theory; MemberData("cycle2Data")>]
let cycle2 ((parents: int list * int list), (expected: int list * int list)) =
    let parent1, parent2 = parents
    
    let child1, child2 = Crossover.cycle2 parent1 parent2
    
    let expectedChild1, expectedChild2 = expected
    
    assertNoDuplicates child1
    assertNoDuplicates child2
    Assert.Equal<List<_>> (expectedChild1, child1)
    Assert.Equal<List<_>> (expectedChild2, child2)

let partiallyMatchedData = seq {
    box [|
         ([ 3; 4; 8; 2; 7; 1; 6; 5; ], [ 4; 2; 5; 1; 6; 8; 3; 7; ])
         ([ 3; 4; 2; 1; 6; 8; 7; 5; ], [ 4; 8; 5; 2; 7; 1; 3; 6; ])
    |]
}

[<Theory; MemberData("partiallyMatchedData")>]
let partiallyMatched ((parents: int list * int list), (expected: int list * int list)) =
    let parent1, parent2 = parents
    
    let child1, child2 = Crossover.partiallyMatched 3 6 parent1 parent2
    
    let expectedChild1, expectedChild2 = expected
    
    assertNoDuplicates child1
    assertNoDuplicates child2
    Assert.Equal<List<_>> (expectedChild1, child1)
    Assert.Equal<List<_>> (expectedChild2, child2)
    