module RobotSimulator
type Direction = North | East | South | West
type Position = int * int
type Robot = { direction: Direction; position: Position }
let create direction position = (direction, position)
let move (instructions: string) robot = 
    let right dir = 
        match dir with 
        | Direction.North -> Direction.East
        | Direction.East -> Direction.South
        | Direction.South -> Direction.West
        | Direction.West -> Direction.North
    let left dir = 
        match dir with 
        | Direction.West -> Direction.South
        | Direction.North -> Direction.West
        | Direction.East -> Direction.North
        | Direction.South -> Direction.East
    let advance dir pos = 
        match dir , pos with 
        | Direction.West, (x,y) -> ( x - 1 , y + 0)
        | Direction.North, (x,y) -> (x + 0, y + 1)
        | Direction.East, (x,y) -> (x + 1, y + 0)
        | Direction.South, (x,y) -> ( x + 0, y - 1)
        
        
    let mutable dir , pos = fst robot , snd robot
    for ch in (Seq.toList instructions) do 
        if ch = 'R' then dir <- right dir 
        elif ch = 'A' then pos <- advance dir pos
        else dir <- left dir
    (dir, pos)