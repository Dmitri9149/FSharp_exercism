// I used some tips from here 
// https://github.com/mikecoop/exercism-fsharp/blob/master/grains/Grains.fs

module Grains

let squareUInt64 (n:int): uint64 = pown 2UL (n-1)
let square (n: int): Result<uint64,string> = 
        match n with 
            | x when x < 1 || x > 64 -> Error "square must be between 1 and 64"
            | x -> Ok(squareUInt64 x)

let total: Result<uint64,string> = 
    let res = [1..64] |> List.sumBy (squareUInt64)
    Ok res