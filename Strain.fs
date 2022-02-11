module Seq

let keep pred xs = 
    let rec helper pred xs acc = 
        match xs with 
            | [] -> acc
            | x::[] -> if pred x then x::acc else acc
            | x::lst -> if pred x 
                            then helper pred lst (x::acc) 
                            else helper pred lst acc 
    let xlst = Seq.toList xs
    helper pred xlst [] |> List.rev |> List.toSeq

let discard pred xs = keep (fun x -> pred x |> not) xs