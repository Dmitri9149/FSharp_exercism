module PerfectNumbers

type Classification = Perfect | Abundant | Deficient 

let classify n : Classification option = 
    if n <= 0 then None 
    else 
        let mutable sum = 0 
    
        for i in 1..n-1 do 
            if n % i = 0 then sum <- sum + i 
    
        if sum = n then Some Perfect 
        elif sum > n then Some Abundant
        else Some Deficient