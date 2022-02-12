module Triangle

let condition1 x y z = 
    x > 0.0 && y > 0.0 && z > 0.0 && (x + y) >= z && (x + z) >= y && (y + z) >= x
let conditionEqui x y z = 
    if x = y && x<> 0.0 then 
        if y = z then true 
        else false
    else false
let conditionIso x y z = 
    if x = y && x <> 0.0 || y = z && z <> 0.0 || x = z && z <> 0.0 then true 
    else false

let conditionSca x y z = 
        if x <> y && z <> x &&  z <> y then true 
         else false

let equilateral triangle = 
    match triangle with 
    |[] -> false    
    |x::y::[] -> false
    |x::y::z::[] -> condition1 x y z && conditionEqui x y z 
    | _ -> false
let isosceles triangle = 
    match triangle with 
    |[] -> false    
    |x::y::[] -> false
    |x::y::z::[] -> condition1 x y z && conditionIso x y z
    | _ -> false

let scalene triangle = 
    match triangle with 
    |[] -> false    
    |x::y::[] -> false
    |x::y::z::[] -> condition1 x y z && conditionSca x y z
    | _ -> false