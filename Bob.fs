module Bob
open System
let response (input: string): string = 
    let inputUp = input.ToUpper()
    if input.Trim().Length = 0 then "Fine. Be that way!"
    elif Seq.exists Char.IsLetter input && (input = inputUp) && input.Trim().EndsWith('?') then "Calm down, I know what I'm doing!"
    elif Seq.exists Char.IsLetter input && (input = inputUp) then "Whoa, chill out!"
    elif input.Trim().EndsWith('?') then "Sure."
    else "Whatever."  