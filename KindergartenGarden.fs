// I used some tips from here 
// https://github.com/tomi/fsharp-exercism/blob/master/14-kindergarten-garden/KindergartenGarden.fs
module KindergartenGarden
// TODO: define the Plant type
type Plant = 
    | Radishes 
    | Clover 
    | Grass 
    | Violets
let charToPlant c = 
    match c with
    | 'R' -> Plant.Radishes
    | 'C' -> Plant.Clover
    | 'G' -> Plant.Grass
    | 'V' -> Plant.Violets
    | _ -> failwith (sprintf "unexpected character %c" c)
let studentToIndex = 
    let children = [|"Alice"; "Bob"; "Charlie"; "David";
"Eve"; "Fred"; "Ginny"; "Harriet";
"Ileana"; "Joseph"; "Kincaid"; "Larry"|]
    let hlp = 
        seq { 0..11 } 
        |> Seq.map (fun x -> (children[x] , x * 2)) 
        |> Seq.toList
    Map hlp
let plants (diagram: string) (student: string) = 
    let start = Map.tryFind student studentToIndex
    match start with 
    | None -> []
    | Some x -> diagram.Split '\n'
                    |> Seq.collect (Seq.skip x >> Seq.take 2)
                    |> Seq.map charToPlant
                    |> Seq.toList                    
