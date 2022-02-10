module Allergies

open System

type Allergen = 
    |Eggs
    |Peanuts
    |Shellfish
    |Strawberries
    |Tomatoes
    |Chocolate
    |Pollen
    |Cats

let allergicTo codedAllergies allergen = 
    match allergen with
    | Eggs -> codedAllergies &&& 1 <> 0
    | Peanuts -> codedAllergies &&& 2 <> 0
    | Shellfish -> codedAllergies &&& 4 <> 0
    | Strawberries -> codedAllergies &&& 8 <> 0
    | Tomatoes -> codedAllergies &&& 16 <> 0
    | Chocolate -> codedAllergies &&& 32 <> 0
    | Pollen -> codedAllergies &&& 64 <> 0
    | Cats -> codedAllergies &&& 128 <> 0


let list codedAllergies = 
    let allergies = [Eggs; Peanuts; Shellfish; Strawberries; Tomatoes; 
                        Chocolate; Pollen; Cats]
    List.filter (fun x -> allergicTo codedAllergies x) allergies