module PizzaPricing

// TODO: please define the 'Pizza' discriminated union type

type Pizza = 
    | Margherita
    | Caprese
    | Formaggio
    | ExtraSauce of Pizza
    | ExtraToppings of Pizza

let rec pizzaPrice (pizza: Pizza): int = 
    match pizza with 
    | Margherita -> 7 
    | Caprese -> 9
    | Formaggio -> 10
    | ExtraSauce x -> pizzaPrice x + 1 
    | ExtraToppings x -> pizzaPrice x + 2
 
let orderPrice(pizzas: Pizza list): int = 
    let help = Seq.fold (+) 0 (Seq.map pizzaPrice pizzas)
    if pizzas.Length = 1 then 3 + help
    elif pizzas.Length = 2 then 2 + help 
    else hel