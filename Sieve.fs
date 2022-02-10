module Sieve

let primes limit = 
    let trueLst = [ for i in 0..limit -> (true, i)  ]
   
    let inSeq x lst = Seq.exists ((=) x) lst

    let helper2 (limit: int) (p: int) (acc: (bool*int) list)   = 
        let rnge = seq { for i in p * p..p..limit -> i }
//        let inSeq x lst = Seq.exists ((=) x) lst
        List.map (fun (x,y) -> if inSeq y rnge then (false, y) else x,y ) acc

    let rec helper1 (limit: int) (p: int) (acc: (bool*int) list) = 
        if p * p <= limit 
            then 
                if acc |> List.take p |> List.rev |> List.head |> fst = true 
                    then 
                        match p with 
                        | x when x = limit -> acc 
                        | x when x < limit -> helper1 limit (x + 1) (helper2 limit x acc) 
                        | _ -> [(true, 2)]
                    else helper1 limit (p+1) acc
            else acc
        
    helper1 limit 2 trueLst 