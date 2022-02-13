module Proverb

let recite (input: string list): string list = 
    if input = [] then []
    else 
        let hlp = (fun (x,y) -> sprintf "For want of a %s the %s was lost." x y )
        ( input 
        |> List.pairwise
        |> List.map hlp )
        @
        [ sprintf "And all for the want of a %s." (input |> List.head) ]