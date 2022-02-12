module ScrabbleScore
let scores =
    [ ([ 'A'; 'E'; 'I'; 'O'; 'U'; 'L'; 'N'; 'R'; 'S'; 'T'], 1);
    ([ 'D'; 'G' ], 2);
    ([ 'B'; 'C'; 'M'; 'P' ], 3);
    ([ 'F'; 'H'; 'V'; 'W'; 'Y' ], 4);
    ([ 'K' ], 5);
    ([ 'J'; 'X' ], 8);
    ([ 'Q'; 'Z' ], 10) ]

let score word =
    let toScore c =
        scores 
        |> List.find (fun (chars, _) -> 
            chars |> List.exists ((=) (System.Char.ToUpper c)))
        |> snd
    word
    |> Seq.map toScore
    |> Seq.sum