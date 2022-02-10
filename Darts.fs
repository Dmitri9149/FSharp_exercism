module Darts

let score (x: double) (y: double): int = 
    let sqRad = x ** 2 + y **2 
    if sqRad > 100.00 then 0
    elif sqRad <= 100.00 && sqRad > 25.00 then 1
    elif sqRad <= 25.00 && sqRad > 1.00 then 5
    else 10 