module CollatzConjecture

let steps (number: int): int option = 
    let rec helper number steps = 
        match number with 
        | x when x < 1 -> None 
        | x when x = 1 -> Some steps
        | x when x % 2 = 0 -> helper ( x / 2) (steps + 1)
        | x when x % 2 <> 0 -> helper (x * 3 + 1) (steps + 1)
        | _ -> None
    helper number 0