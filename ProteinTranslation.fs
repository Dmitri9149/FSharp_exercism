module ProteinTranslation

let proteins rna = 
    let toProteins x = 
        match x with 
        | "AUG" -> "Methionine"
        | "UUU" | "UUC" -> "Phenylalanine"
        | "UUA" | "UUG" -> "Leucine"
        | "UCU" | "UCC" | "UCA" | "UCG" -> "Serine"
        | "UAU" | "UAC" -> "Tyrosine"
        | "UGU" | "UGC" -> "Cysteine"
        | "UGG" -> "Tryptophan" 
        | "UAA" | "UAG" | "UGA" -> "STOP"
        | _ -> " Error "

    rna 
    |> Seq.chunkBySize 3
    |> Seq.map System.String 
    |> Seq.toList
    |> List.map toProteins
    |> List.takeWhile ((<>) "STOP")