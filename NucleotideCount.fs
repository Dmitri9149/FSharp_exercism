module NucleotideCount

let nucleotideCounts (strand: string): Option<Map<char, int>> =  
    let lst = strand |> Seq.toList
    let isIn x lst = List.exists ((=) x) lst
    let nucls = ['A';'C';'G';'T']
    let filt = lst |> List.filter (fun x -> isIn  x nucls |> not)
    if filt.Length > 0 then None 
    else 
        let almoust = lst |> Seq.countBy id |> Seq.toList |> Map
        let folder = 
            fun (x: Map<char, int>) y -> 
            if x |> Map.containsKey y |> not  then x.Add((y, 0)) else x
        nucls |> List.fold folder almoust |> Some