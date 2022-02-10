module HighScores

let scores (values: int list): int list = values


let latest (values: int list): int = values |> List.rev |> List.head


let personalBest (values: int list): int = values |> List.max

let personalTopThree (values: int list): int list = 
    let len = values.Length 
    match len with 
    | x when x = 0 -> []
    | x when (x <= 3 && x >= 1) -> values |> List.sortDescending
    | x -> values |> List.sortDescending |> List.take 3