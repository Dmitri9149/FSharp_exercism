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

 // clock is string like this "09:35"
let add (minutes: int) (clock: string) = 
    let hrs = match clock with 
                | x when x[0] = '0' -> int (string x[1])
                | _ -> int clock[0..1]
    let mns = match clock with 
                | x when x[3] ='0' -> int (string x[4])
                | _ -> int clock[3..4]
    create hrs (minutes + mns)
 
let subtract minutes clock = failwith "You need to implement this function."

let display clock = failwith "You need to implement this function."  