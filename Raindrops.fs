module Raindrops

let convert (number: int): string = 
    let lst = seq [(3,"Pling"); (5,"Plang"); (7,"Plong")]
    let numString = number |> string
    let usedNot = Seq.forall (fun z -> ((number % (fst z)) <> 0)) lst
    let helper = fun x y -> x + (if (number % (fst y) = 0) then (snd y) else "")
    if usedNot |> not then Seq.fold helper "" lst else numStrin