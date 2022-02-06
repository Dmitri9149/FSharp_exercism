module GuessingGame
let reply (guess: int): string = 
    let (|Correct|SoClose|TooLow|TooHigh|) guess = 
        if guess = 42 then Correct
        elif abs (guess - 42) = 1 then  SoClose
        elif guess < 41 then TooLow 
        else TooHigh
    match guess with 
    |Correct -> "Correct"
    |SoClose -> "So close"
    |TooLow ->  "Too low"
    |TooHigh -> "Too high"