module Isogram

let rec isIsogram (str: string) = 
    let letters = ['a'..'z']
    let helper x =  Seq.exists ((=) x) letters    
    match  str.ToLower() with 
    |"" -> true
    |x when x.Length = 1 -> if Seq.exists ((=) x[0]) letters 
                                then true 
                                elif Seq.exists ((=) x[0]) ([' ']@['-']) then true 
                                else false
    |x ->   if Seq.exists ((=) x[0]) letters && Seq.exists ((=) x[0]) x[1..] 
            then false
            else isIsogram x[1..] 