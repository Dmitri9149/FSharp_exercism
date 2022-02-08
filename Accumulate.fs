module Accumulate

let accumulate (func: 'a -> 'b) (input: 'a list): 'b list = 
    Seq.toList ( seq { for d in input do yield (func d)} )