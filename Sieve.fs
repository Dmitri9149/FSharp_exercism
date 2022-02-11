// I changed imperative code in Python from 
// https://www.geeksforgeeks.org/sieve-of-eratosthenes/
// to functional code in F#
// the division or remainder operations 
// (div, /, mod or % depending on the language) are not used   :) 
module Sieve

let primes limit = 

    let trueLst = [ for i in 0..limit -> (true, i)  ]
    let inSeq x lst = List.exists ((=) x) lst

    let helper2 (limit: int) (p: int) (acc: (bool*int) list)   = 
        let rnge = [ for i in p * p..p..limit -> i ]
        List.map (fun (x,y) -> if inSeq y rnge then (false, y) else (x,y) ) acc

    let rec helper1 (limit: int) (p: int) (acc: (bool*int) list) =
        let condition = acc |> List.skip p |> List.head |> fst = true
        if p * p <= limit 
            then 
                if condition 
                    then  helper1 limit (p + 1) (helper2 limit p acc)
                    else helper1 limit (p + 1) acc
            else acc

    match limit with 
        | x when x = 1 -> []
        | _ -> 
                (helper1 limit 2 trueLst ) 
                    |> List.skip 2
                    |> List.filter (fun x -> fst x = true) 
                    |> List.map (fun x -> snd x)