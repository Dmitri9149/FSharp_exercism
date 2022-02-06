module Leap

let leapYear (year: int): bool = 
    let by400 =(year % 400 = 0)
    let by100 = (year % 100 = 0)
    let by4 = (year % 4 = 0)

    match by4, by100, by400 with 
        |_, _, true -> true
        |true, false, _ -> true
        |false, _, _ -> false
        |_, true, false -> false