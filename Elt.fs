module Etl

let transform (scoresWithLetters: Map<int, char list>): Map<char, int> = 
        scoresWithLetters 
        |> Map.toList 
        |> List.map ( fun (x,lst) 
                        -> List.map (fun z -> (System.Char.ToLower z,x)) lst )
        |> List.fold (@) []
        |> Map.ofList 