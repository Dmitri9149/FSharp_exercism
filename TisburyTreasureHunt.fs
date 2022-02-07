module TisburyTreasureHunt
let getCoordinate (line: string * string): string =
    line |> snd
let convertCoordinate (coordinate: string): int * char = 
    coordinate[0]  |> string |> int , coordinate[1]
let compareRecords (azarasData: string * string) (ruisData: string * (int * char) * string) : bool = 
    match azarasData , ruisData with 
        | (x,y) , (z,v,k) when convertCoordinate y = v -> true  
        | _ -> false 
let createRecord (azarasData: string * string) (ruisData: string * (int * char) * string) : (string * string * string * string) =
        match azarasData , ruisData with 
        | (x,y) , (z,v,k) when convertCoordinate y = v -> (y,z,k,x) 
        | _ -> ("","","","")