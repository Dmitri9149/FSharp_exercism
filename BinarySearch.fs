// I used Python code from here:
// https://www.geeksforgeeks.org/binary-search/

module BinarySearch

let find (input: int array) (value: int) = 
    let len = input.Length
    let rec helper (arr: int array) l r x = 
        match r with 
        | z when z >= l ->  let mid = l + ((z - l) / 2)
                            let cand = arr[mid]   
                            if cand = x then (Some mid)
                            elif cand > x then  helper arr l (mid - 1) x
                            elif cand < x then helper arr (mid + 1) z x
                            else None
        | _ -> None
  
    helper (input: int array) 0 (len - 1)  value