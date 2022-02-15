module CustomSet
type Set<'a> =
    { Elements: 'a list }
let empty = { Elements = List.empty }
let isEmpty set =
    Seq.isEmpty set.Elements
let size set =
    List.length set.Elements
let fromList list =
    { Elements = list |> List.distinct |> List.sort }
let toList set =
    set.Elements
let contains value set = Seq.contains value set.Elements
let insert value set = 
    if List.contains value set.Elements then set 
    else {set with Elements = value::set.Elements}
let union left right =
    fromList (left.Elements @ right.Elements)
let intersection left right = 
    let leftList , rightList = left.Elements , right.Elements 
    let rec helper lf rt acc = 
        match lf with 
        | [] -> acc
        | x::xs -> if List.contains x rt 
                        then helper xs rt (x::acc) 
                   else helper xs rt acc
    let l1 , l2 = leftList |> List.length , rightList |> List.length
    if l1 <= l2 
        then helper leftList rightList  [] |> List.distinct |> fromList
    else helper rightList leftList [] |> List.distinct |> fromList
let difference left right = 
    if left = empty then empty 
    else 
        (left.Elements |> List.except right.Elements)  
        |> fromList
let isSubsetOf left right = 
    (intersection left right).Elements.Length  = left.Elements.Length
let isDisjointFrom left right = (intersection left right).Elements.Length = 0 
let isEqualTo left right =
    isSubsetOf left right &&
    isSubsetOf right left