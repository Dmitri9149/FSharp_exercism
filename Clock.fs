module Clock

let create hours minutes = 
    let createPositive hours minutes = 
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
    let total = hours * 60 + minutes
    if total >= 0 
        then createPositive hours minutes 
        else createPositive 0 (24 * 60 + (total % (24 * 60)))

// clock is string like this "09:35"
let add (minutes: int) (clock: string) = 
    let hrs = match clock with 
                | x when x[0] = '0' -> int (string x[1])
                | _ -> int clock[0..1]
    let mns = match clock with 
                | x when x[3] ='0' -> int (string x[4])
                | _ -> int clock[3..4]
    create hrs (minutes + mns)
 
let subtract (minutes: int) (clock: string) = 
    let hrs = match clock with 
                | x when x[0] = '0' -> int (string x[1])
                | _ -> int clock[0..1]
    let mns = match clock with 
                | x when x[3] ='0' -> int (string x[4])
                | _ -> int clock[3..4]
    let diff = hrs * 60 + mns  - minutes
    if diff >= 0 
       then create 0 diff 
       else create 0 ( 24 * 60 + (diff % (24 * 60)) ) 

let display clock = clock 