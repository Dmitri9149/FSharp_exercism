module Clock

let create hours minutes = 
    let totalMins = hours * 60 + minutes 
    let hrs , mns  = (totalMins / 60) % 24  , totalMins % 60 
    let hrsString =
        match hrs with 
        | x when x <= 9 -> "0" + string x
        | x -> string x
    let mnsString  = 
        match mns with 
        | x when x <= 9 -> "0" + string x
        | x -> string x

    hrsString + ":" + mnsString

 
let add minutes clock = 
    let clockMins = 
        match clock with 
            | x when x[-2] = '0' -> int x[0..1] * 60 + int x[-1..-1] + minutes
            | x  -> int x[0..1] * 60 + int x[-2..-1] + minutes
    create 0 clockMins

 
let subtract minutes clock = failwith "You need to implement this function."

let display clock = failwith "You need to implement this function."  