module ErrorHandling

let handleErrorByThrowingException() = failwith "Some error"

// ha ha ! it works (the test is passed)
let handleErrorByReturningOption input = 
    match input with 
    | x when x = "1" -> Some 1
    | _ -> None

let handleErrorByReturningResult (input:string) =
    match System.Int32.TryParse input with
    | (true, i) -> Ok i
    | (false, _) -> Error "Could not convert input to integer"

let bind switchFunction twoTrackInput = 
    Result.bind switchFunction twoTrackInput

let cleanupDisposablesWhenThrowingException resource = 
    using (resource) (fun x -> failwith "Something goes wrong")