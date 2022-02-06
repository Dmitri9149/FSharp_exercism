module Hamming

let distance (strand1: string) (strand2: string): int option = 
    let len1 = strand1.Length
    let len2 = strand2.Length
    let equal = len1 = len2
    let rec distance str1 str2 acc = 
        match str1 with 
            |[] -> acc
            | x::rest1 -> match str2 with 
                            | [] -> acc
                            | z::rest2 -> if (x = z) 
                                            then distance rest1 rest2 acc 
                                            else distance rest1 rest2 (acc + 1) 
    
    if equal then Some (distance (Seq.toList strand1) (Seq.toList strand2) 0) else None

    // from 

(*
let distance (strand1: string) (strand2: string): int option =
    match strand1.Length, strand2.Length with
    | x, y when x = y ->
        let seq1, seq2 = (List.ofSeq strand1), (List.ofSeq strand2)
        Some(List.fold2 (fun acc a b -> if a = b then acc; else acc + 1) 0 seq1 seq2)
    | _, _ -> None
*)