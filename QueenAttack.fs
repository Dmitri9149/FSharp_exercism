
module QueenAttack
let create (position: int * int) = if fst position = snd position then true else false
let canAttack (queen1: int * int) (queen2: int * int) = 
    match queen1 , queen2 with 
        | (x , y) , (z , v) when x = z || y = v -> true 
        | (x , y) , (z , v) when abs (x - z)  =  abs (y - v) -> true
        | _ -> false 
