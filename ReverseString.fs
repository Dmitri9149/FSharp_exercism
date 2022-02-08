open System
let reverse (input: string): string = 
    let rec helper lst acc = match lst with 
                                | [] -> acc
                                | x::xs -> helper xs (x::acc)
    let hlp = (helper (Seq.toList input) []) 
    System.String.Concat(Array.ofList(hlp))