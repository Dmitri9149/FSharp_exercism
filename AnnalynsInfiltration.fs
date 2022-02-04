module AnnalynsInfiltration
let canFastAttack (knightIsAwake: bool): bool = 
    match knightIsAwake with 
    |true -> false
    |false -> true
let canSpy (knightIsAwake: bool) (archerIsAwake: bool) (prisonerIsAwake: bool): bool =
    let oneOfThem (knightIsAwake: bool) (archerIsAwake: bool) (prisonerIsAwake: bool): bool =  
        (knightIsAwake: bool) || (archerIsAwake: bool) || (prisonerIsAwake: bool) 
    match (oneOfThem (knightIsAwake: bool) (archerIsAwake: bool) (prisonerIsAwake: bool)) with 
        |true -> true
        |false -> false
let canSignalPrisoner (archerIsAwake: bool) (prisonerIsAwake: bool): bool =
    match (archerIsAwake: bool) with 
        |true ->     match (prisonerIsAwake: bool) with 
                     |true -> false
                     |false -> false   
        |false ->    match (prisonerIsAwake: bool) with 
                     |true -> true
                     |false -> false   
let canFreePrisoner (knightIsAwake: bool) (archerIsAwake: bool) (prisonerIsAwake: bool) (petDogIsPresent: bool): bool = 
    match petDogIsPresent with 
        |true -> match archerIsAwake with 
                    |true -> false
                    |false -> true
        |false -> match prisonerIsAwake with 
                    |false -> false
                    |true ->    let both (knightIsAwake: bool) (archerIsAwake: bool) :bool = 
                                        not ((knightIsAwake: bool) || (archerIsAwake: bool)) 
                                match (both (knightIsAwake: bool) (archerIsAwake: bool)) with 
                                        |true -> true
                                        |false -> false 
