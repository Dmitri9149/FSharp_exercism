module SecretHandshake

let commands number = 
    let helper (number: int) bt  = number &&& bt = bt
    let actions = [(1, "wink"); (2, "double blink");
                    (4, "close your eyes"); (8, "jump")]
    actions 
    |> List.filter (fun (x,y) -> helper number x ) 
    |> List.map (fun x -> snd x)
    |> (fun acts -> if helper number 16 then List.rev acts else acts)