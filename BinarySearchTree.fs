// with the help of https://github.com/mikecoop/exercism-fsharp/blob/master/binary-search-tree/BinarySearchTree.fs
module BinarySearchTree

type BinaryTree = { Value: int
                    Left: BinaryTree option
                    Right: BinaryTree option }

let left node  = node.Left
let right node = node.Right
let data node = node.Value 

let rec insert (tree: BinaryTree option) item = 
    match tree with 
    | None ->
        { Value = item; Left = None; Right = None }
    | Some t -> if  item <= t.Value then {t with Left = Some (insert t.Left item) }
                else {t with Right = Some (insert t.Right item) }

let rec create items = 
    let tree = List.fold (fun acc x -> Some (insert acc x)) None items 
    match tree with 
    | Some t -> t
    | None -> failwith "No values in tree"

let rec sortedData node = 
    let rec sortedWithOptions tree =
        match tree with
        | None -> [ ]
        | Some t -> (sortedWithOptions t.Left) 
                     @ 
                     [ t.Value ]
                     @ 
                     (sortedWithOptions t.Right)
    sortedWithOptions (Some node)