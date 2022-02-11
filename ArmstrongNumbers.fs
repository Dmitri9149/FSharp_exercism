module ArmstrongNumbers

let isArmstrongNumber (number: int): bool = 
    let numStr = number |> string |> Seq.toList |> List.map (string >> int)
    let helper x = pown x numStr.Length
    numStr |> List.sumBy helper  = number