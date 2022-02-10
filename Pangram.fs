module Pangram
let isPangram (input: string): bool = 
    let setOfInput = input.ToLower() |> Set.ofSeq
    let testSet = ['a'..'z'] |> Set.ofSeq
    Set.intersect setOfInput testSet = testSet 