#load "utils.fsx"

open Utils
open System.Text.RegularExpressions

let input = getLines "d14.txt"
let test = getLines "d14test.txt"

let re = Regex @"(\d+),(\d+)"

//getCorners: Get the corners of the structures on one line of the input
//line: string: The line from the input file
let getCorners line =
  let ms = re.Matches(line)
  Seq.foldBack (fun (m: Match) acc -> let g = m.Groups
                                      (int g[1].Value, int g[2].Value)::acc) ms []

//genStructure: Generate the set containg the original rock structures
//inp: list<string>: Lines from input file
let genStructure inp =
  let corners = List.map getCorners inp
  let cornersPaired = List.collect List.pairwise corners
  let state = List.fold (fun state ((x1,y1), (x2,y2)) -> match x1 = x2 with
                                                         //same x, create ys
                                                         | true  -> List.fold (fun s y -> Set.add (x1,y) s) state [(min y1 y2)..(max y1 y2)]
                                                         //same y, create xs
                                                         | false -> List.fold (fun s x -> Set.add (x,y1) s) state [(min x1 x2)..(max x1 x2)]
                                                         ) Set.empty cornersPaired
  state

//Drops a single piece of sand until it hits its final position.
//If the sands y-coordinate exceeds the bounds, it is unconditionally placed
//state: set<int*int>: Current state of the system. All entries mark a piece of rock or sand
//(x,y): int*int:      Coordinates of currently falling piece of sand
//bounds: int:         If a piece of sand's y-coordinate exceeds thsi value, it is placed unconditionally
let rec dropSand state (x,y) bounds =
  let below = Set.contains (x,y+1) state
  let left = Set.contains (x-1,y+1) state
  let right = Set.contains (x+1,y+1) state
  if y > bounds
  then Set.add (x,y) state
  else match (below, left, right) with
       | (false,_,_) -> dropSand state (x,y+1) bounds
       | (_,false,_) -> dropSand state (x-1,y+1) bounds
       | (_,_,false) -> dropSand state (x+1,y+1) bounds
       | _ -> Set.add (x,y) state

//keepDropping: Keep dropping pieces of sand from (500,0) until the exit condition is reached
//state: set<int*int>:            Current state of the system. All entries are a piece of sand or stone
//cnt: int:                       Counter for pieces of sand that have dropped so far
//bounds: int:                    If a piece of sand's y-coordinate exceeds thsi value, it is placed unconditionally
//exitCond: set<int*int> -> bool: Condition that determines when the to stop dropping state
let rec keepDropping state cnt bounds exitCond =
  if cnt % 2000 = 0 then printfn "cnt=%d" cnt
  if exitCond state
  then cnt
  else
    let state' = dropSand state (500,0) bounds
    keepDropping state' (cnt+1) bounds exitCond

//solve problem 1
let solve1 inp =
  let state = genStructure inp
  let maxY = Set.maxElement (Set.map (fun (_,y) -> y) state)
  //Subtract 1 from result since item placed outside of bounds doesn't count towards solution
  (keepDropping state 0 (maxY) (Set.exists (fun (_,y) -> y>maxY)) - 1)

//Solve problem 2
let solve2 inp =
  let state = genStructure inp
  let maxY = Set.maxElement (Set.map (fun (_,y) -> y) state)
  keepDropping state 0 (maxY) (Set.contains (500,0))
