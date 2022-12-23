#load "utils.fsx"

open Utils
open System.Text.RegularExpressions

let input = getLines "d22.txt"
let test = getLines "d22test.txt"
let test2 = getLines "d22test2.txt" //https://reddit.com/zst7z3

type Tile = O | W
type Turn = L | R

//Parse a line of the input map. Returns a list of ((r,c), Tile) values for each line
let parseMapLine (r, (line: string)) =
  //Compute the offset (starting column) as the number of spaces in front
  let t = line.Trim ' '
  let offset = (String.length line) - (String.length t)
  let mapped = Seq.mapi (fun i c -> (i+offset, c)) t
  Seq.fold (fun acc (col, char) -> let x = match char with
                                           | '.' -> ((r,col), O)
                                           | '#' -> ((r,col), W)
                                           | _ -> failwithf "Unable to parse %c %d" char (int char)
                                   x::acc) [] mapped

//parseMap: Parse the map in the input file
//Returns a Map<int*int, Tile>, mapping (r,c) coordinates to tiles
let parseMap inp =
  let inp' = List.take (List.length inp - 2) inp

  //Parse and collect all lines, then convert to map
  Map.ofList (List.collect parseMapLine (List.indexed inp'))

//parseInstrs: Exctract the walk-and-turn instructions from the input file
//Returns two lists. One of the moves to be performed, and one of the turns
let parseInstrs inp =
  let l = List.last inp
  let re1 = Regex @"(\d+)"
  let re2 = Regex @"(\D)"

  let moves = Seq.map (fun (m: Match) -> int m.Groups[1].Value) (re1.Matches(l))
  let turns = Seq.map (fun (m: Match) -> let v = m.Groups[1].Value
                                         match v with
                                         | "L" -> L
                                         | "R" -> R
                                         | _ -> failwithf "Unable to parse turn %s" v) (re2.Matches(l))
  (List.ofSeq moves, List.ofSeq turns)

//move: Move in a given direction on the map for some number of steps, or until we hit a wall.
//      This only works for part 1 of the problem
//map: Map<int*int,Tile>: The map that we're moving on
//(r,c): int*int: Current row,col coordinates on the map
//(dr,dc): int*int: Current move direction encoded as delta-values
//steps: int: Remaining steps to be performed
let rec move map (r,c) (dr,dc) steps =
  if steps = 0
  then (r,c)
  else
    //Attempt to find the value at (r+dr, c+dc). If not present, match on dr,dc to find the next eligible tile
    let next = if Map.containsKey (r+dr, c+dc) map then
                (r+dr, c+dc)
               else //Not present, must loop back
                let keys = Map.keys map
                match (dr,dc) with
                | (0,1) ->  let keys' = Seq.filter (fun (r',_) -> r'=r) keys //right. Keep r constant and find minimum c-value
                            Seq.min keys'
                | (0,-1) -> let keys' = Seq.filter (fun (r',_) -> r'=r) keys //left: Keep r constant and find maximum c-value
                            Seq.max keys'
                | (1,0) ->  let keys' = Seq.filter (fun (_,c') -> c'=c) keys //down: Keep c constant, find min r-value
                            Seq.min keys'
                | (-1,0) -> let keys' = Seq.filter (fun (_,c') -> c'=c) keys //up: Keep c constant, find min r-value
                            Seq.max keys'
                | _ -> failwithf "Unable to parse (dr,dc)=(%d,%d)" dr dc

    match Map.find next map with
    | O -> move map next (dr,dc) (steps-1)
    | W -> (r,c)

//turn: Turn Right or Left to change the deltaR and deltaC values
//(dr,dc): int*int*: Current deltaR, deltaC values
//arg: Turn: Direction to turn
let turn (dr,dc) = function
  | L -> (-1*dc, dr)
  | R -> (dc, -1*dr)

//performMoves: Do the moves and turns in the input, moving around on the map
//              Only works for part 1
//moves: int list: List of steps tot ake
//turns: Turn list: Turns to perform after stepping
//map: Map<int*int,Tile>: The map we're walking on
//rc: int*int: Current row,col coordinate on the map
//drc: int*int: Current delta values (direction) for moving
let rec performMoves moves turns map rc drc =
  match moves with
  | [] -> (rc, drc)
  | m::moves' -> let rc' = move map rc drc m
                 if turns = [] then //Final move has no turn after it, detect that here
                  (rc', drc)
                 else
                  let drc' = turn drc (List.head turns)
                  performMoves moves' (List.tail turns) map rc' drc'

//dirScore: Turns the delta (row,col) values into a score
let dirScore = function
  | (0,1) ->  0 //right
  | (0,-1) -> 2 //left
  | (1,0) ->  1 //down
  | (-1,0) -> 3 //up
  | x -> failwithf "Unable to decode direction %A" x

//Solvee part 1
let solve1 inp =
  let map = parseMap inp
  let (moves, turns) = parseInstrs inp
  //Starting coodinate is top-left tile
  let start = Seq.min (Map.keys map)

  let ((r,c), drc) = performMoves moves turns map start (0,1)
  (r+1)*1000 + (c+1)*4 + (dirScore drc)

//parseCubeMapHardcoded: Parse the input map as a folded cube
//Has hardcoded connections between faces
let parseCubeMapHardcoded (inp: string list) =
  let V = if (String.length (List.head inp) > 50) then 50 else 4

  let l12 = List.take V inp
  let l3 = List.take V (List.skip V inp)
  let l45 = List.take V (List.skip (2*V) inp)
  let l6 = List.take V (List.skip (3*V) inp)

  //50x50 grids containing the individual faces (4x4 on toy example)
  //A: Top face.  B: Front face.  C: Right face.  D: Back face.  E: Left face.  F: Bottom face
  let A = List.map (fun (s: string) -> s[V..(2*V-1)]) l12     //Block 1
  let C = List.map (fun (s: string) -> s[(2*V)..(3*V-1)]) l12 //Block 2
  let B = List.map (fun (s: string) -> s[V..(2*V-1)]) l3      //Block 3
  let F = List.map (fun (s: string) -> s[V..(2*V-1)]) l45     //Block 4
  let E = List.map (fun (s: string) -> s[0..(V-1)]) l45       //Block 5
  let D = List.map (fun (s: string) -> s[0..(V-1)]) l6        //Block 6

  //Collect all of A-F in a list, then convert to a Map<int*int,Tile>
  //Then zip and map with corresponding face name
  let z = List.map (fun e -> Map.ofList (List.collect parseMapLine (List.indexed e))) [A;B;C;D;E;F]
  let faceMap = Map.ofList (List.zip ["A";"B";"C";"D";"E";"F"] z)

  //Mappings for each face, showing which face we move onto, and in which direction,
  //when moving off a given face in a given direction
  //D, R, U, L
  let mA = Map.ofList [((1,0), ("B", (1,0)));  ((0,1), ("C", (0,1)));  ((-1,0), ("D", (0,1)));  ((0,-1), ("E", (0,1)))]
  let mB = Map.ofList [((1,0), ("F", (1,0)));  ((0,1), ("C", (-1,0))); ((-1,0), ("A", (-1,0))); ((0,-1), ("E", (1,0)))]
  let mC = Map.ofList [((1,0), ("B", (0,-1))); ((0,1), ("F", (0,-1))); ((-1,0), ("D", (-1,0))); ((0,-1), ("A", (0,-1)))]
  let mD = Map.ofList [((1,0), ("C", (1,0)));  ((0,1), ("F", (-1,0))); ((-1,0), ("E", (-1,0))); ((0,-1), ("A", (1,0)))]
  let mE = Map.ofList [((1,0), ("D", (1,0)));  ((0,1), ("F", (0,1)));  ((-1,0), ("B", (0,1)));  ((0,-1), ("A", (0,1)))]
  let mF = Map.ofList [((1,0), ("D", (0,-1))); ((0,1), ("C", (0,-1))); ((-1,0), ("B", (-1,0))); ((0,-1), ("E", (0,-1)))]

  //This map tells us which face we walk onto and in which direction when we walk off a face
  let changeMap = Map.ofList [("A", mA); ("B", mB); ("C", mC); ("D", mD); ("E", mE); ("F", mF)]
  (faceMap, changeMap)

//coordTransform: Perform a coordinate transform when moving onto a new face of the cube
//dir1: Original direction of movement
//dir2: New direction of movement
//(r,c): Row,column on original face
//m: Maximum (r,c) value a coordinate can have
//return: The new (r,c) coordinates on the new face
let coordTransform dir1 dir2 (r,c) m =
  match (dir1, dir2) with
  | ((0,1), (0,1)) ->  (r,0)     //right->right
  | ((0,1), (1,0)) ->  (0, m-r)  //right->down
  | ((0,1), (0,-1)) -> (m-r,m)   //right->left
  | ((0,1), (-1,0)) -> (m,r)     //right->up

  | ((1,0), (0,1)) ->  (m-c,0)   //down->right
  | ((1,0), (1,0)) ->  (0, c)    //down->down
  | ((1,0), (0,-1)) -> (c, m)    //down->left
  | ((1,0), (-1,0)) -> (m, m-c)  //down->up

  | ((0,-1), (0,1)) ->  (m-r, 0) //left->right
  | ((0,-1), (1,0)) ->  (0, r)   //left->down
  | ((0,-1), (0,-1)) -> (r, m)   //left->left
  | ((0,-1), (-1,0)) -> (m, m-r) //left->up

  | ((-1,0), (0,1)) ->  (c, 0)   //up->right
  | ((-1,0), (1,0)) ->  (0, m-c) //up->down
  | ((-1,0), (0,-1)) -> (m-c, m) //up->left
  | ((-1,0), (-1,0)) -> (m ,c)   //up->up
  | _ -> failwithf "Unable to coordinate transform %A -> %A" dir1 dir2

//move2: Like move, but takes the 6 face-maps and change-maps as inputs.
//Returns coordinates and current map name when finished
//faceMap: Map<string,Map<int*int,Tile>>: Maps from face name to (r,c) coords to Tile
//changeMap: ... : Maps from face name to current direction to (new face name, new direction)
//mn: string: Name of current face
//(r,c): int*int: Current row,col coordinates
//(dr,dc): int*int: Current delta
//steps: int: Remaining steps
//Returns: Final (r,c) (dr,dc) and map name
let rec move2 (faceMap, changeMap) mn (r,c) (dr,dc) steps =
  if steps = 0
  then
    ((r,c),(dr,dc),mn)
  else
    let m = Map.find mn faceMap
    let (nextRC, nextDRC, nextMN) = if Map.containsKey (r+dr, c+dc) m then
                                      ((r+dr, c+dc), (dr,dc), mn)
                                    else //Not present, moving to another face
                                      //Need to extract largest possible R/C-value.
                                      let (max,_) = Seq.max (Map.keys m)
                                      let (newFace, newDRC) = Map.find (dr,dc) (Map.find mn changeMap)
                                      let newRC = coordTransform (dr,dc) newDRC (r,c) max
                                      (newRC, newDRC, newFace)

    match Map.find nextRC (Map.find nextMN faceMap) with
    | O -> move2 (faceMap, changeMap) nextMN nextRC nextDRC (steps-1)
    | W -> ((r,c), (dr,dc), mn)

//performMoves2: Like performMoves, but only works for part 2
//moves: int list: List of steps to take on each move
//turns: Turn list: Turns to perform after moving on each round
//maps: FaceMap*ChangeMap: Face map and change map for use in move2
//mn: Current map name
//rc: Current (r,c) coords
//drc: Current (dr,dc) values
//Returns: Final (r,c), (dr,dc) and map name
let rec performMoves2 moves turns maps mn rc drc =
  match moves with
  | [] -> (rc, drc, mn)
  | m::moves' -> let (rc', drc', mn') = move2 maps mn rc drc m
                 if turns = [] then
                  (rc', drc', mn')
                 else
                  let drc'' = turn drc' (List.head turns)
                  // printfn "Moved %d, turned %A. Now at %A on %A facing %A" m (List.head turns) rc' mn' drc''
                  performMoves2 moves' (List.tail turns) maps mn' rc' drc''

let solve2 inp =
  let maps = parseCubeMapHardcoded inp
  let (moves, turns) = parseInstrs inp
  let ((r,c), drc, mn) = performMoves2 moves turns maps "A" (0,0) (0,1)
  //Convert from face-coords to global map-coords
  let (r', c') = match mn with
                 | "A" -> (r+1, c+50+1)
                 | "B" -> (r+50+1, c+50+1)
                 | "C" -> (r+1, c+100+1)
                 | "D" -> (r+150+1, c+1)
                 | "E" -> (r+100+1, c+1)
                 | "F" -> (r+100+1, c+50+1)
                 | _ -> failwithf "Unrecognized map face %s" mn
  r'*1000+c'*4+ (dirScore drc)