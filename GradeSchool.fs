module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School): School = 
    let res = match school.TryFind grade with 
                | None -> []
                | Some x -> x
    let sort = List.sortWith (fun a b -> compare a b) (student::res)
    school.Add (grade , sort)

let roster (school: School): string list = 
    let folder x y = x @ (snd y)
    let hlp = Seq.fold folder [] (Map.toArray school) 
    hlp

let grade (number: int) (school: School): string list = 
    let res = match school.TryFind number with 
                | None -> []
                | Some x -> x
    res