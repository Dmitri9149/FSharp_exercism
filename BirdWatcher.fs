module BirdWatcher

open System

let lastWeek: int[] = [| 0; 2; 5; 3; 7; 8; 4 |]
//    let len = lastWeek.Length
//    lastWeek[len -7..len -1]

let yesterday(counts: int[]): int =
    counts[counts.Length - 2]

let total(counts: int[]): int =
    Seq.fold (fun x y -> x + y) 0 counts

let dayWithoutBirds(counts: int[]): bool =
    let res = Seq.filter (fun x -> x = 0) counts
    Seq.isEmpty res |> not

let incrementTodaysCount(counts: int[]): int[] = 
    Array.set counts (counts.Length - 1) (counts[counts.Length - 1] + 1)
    counts

let oddWeek(counts: int[]): bool =
// from http://www.fssnip.net/1R/title/Take-every-Nth-element-of-sequence
    let everyNth n seq = 
        seq |> Seq.mapi (fun i el -> el, i) 
// Add index to element
            |> Seq.filter (fun (el, i) -> i % n = n - 1) 
// Take every nth element
            |> Seq.map fst 

    let everySecond = everyNth 2 counts
    let odds = everyNth 2 counts[1..]
    let res test seq = Seq.forall(fun num -> num = test) seq
    (res 0 everySecond ) || (res 5 odds ) || (res 10 everySecond )