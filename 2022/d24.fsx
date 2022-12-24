#load "utils.fsx"

open Utils

let input = getLines "d24.txt"
let test = getLines "d24test.txt"

type Blizzard = int * int //x,y direction of blizzards
type Board = Map<int*int, Blizzard list> //coords to blizzards occupying those coords
type State = (int*int)*int*Board //Current position, time and current board

let parseLine (y, (line: string)) =
  let trimmed = Seq.indexed line[1..(String.length line - 2)]
  Seq.map (fun (x,c) -> match c with
                        | '>' -> ((x,y),[(1,0)])
                        | '<' -> ((x,y),[(-1,0)])
                        | '^' -> ((x,y),[(0,-1)])
                        | 'v' -> ((x,y),[(0,1)])
                        | '.' -> ((x,y),[])
                        | _ -> failwithf "Unknown character %c when parsing x=%d y=%d" c x y) trimmed

let parseInput inp =
  let t = List.tail inp
  let trimmed = List.indexed (List.take (List.length t - 1) t)
  Map.ofSeq (Seq.collect parseLine trimmed)

let uMod i m = (i%m + m) % m

//moveBlizz: Given a blizzard's coordinates and direction, returns the new coordinates for that blizzard,
//           wrapping around the board as necessary
//(x,y): int*int: Blizzards current position
//(dx,dy): int*int: Direction the blizzard is moving
//(maxX,maxY): int*int: Maximum X,Y coordinates, used for bounds checking
//return: The blizzard's new position (x',y')
let moveBlizz (x,y) (dx,dy) (maxX,maxY) =
  (uMod (x+dx) maxX, uMod (y+dy) maxY)

//canMove: Given the player's position and a delta, determines if they can move in that direction
//(x,y): int*int: Player's current position
//(dx,dy): int*int: Direction we wish to move
//board: Board: Current simulation board
//(maxX,maxY): int*int: Maximum X,Y coordinates for bounds checking
//Return: A tuple of (bool, int*int). The bool indicates if the player can move there, and the tuple is the coordinates
let canMove (x,y) (dx,dy) board (maxX, maxY)=
  //Can only move if not occupied by a blizzard && x,y are both within bounds
  //OR if the position is the start or end field
  let (x',y') = (x+dx, y+dy)
  ((not (Map.containsKey (x',y') board) &&
  0 <= x' && x' < maxX &&
  0 <= y' && y' < maxY) ||
  (x'=0 && y'= -1) ||
  (x'=(maxX-1) && y' = maxY), (x',y'))

//nextBoard: Updates the board to its state on the next simulation cycle
//board: Board: Current simulation board
//maxXY: int*int: Maximum X,Y coordinates for bounds checking
//Return: Board: The board for timestamp t+1
let nextBoard board maxXY =
   Map.fold (fun m xy bzs ->
    List.fold (fun m' dxy ->  let xy' = moveBlizz xy dxy maxXY
                              Map.change xy' (
                                function
                                | None -> Some [dxy]
                                | Some xys -> Some (dxy::xys)) m'
    ) m bzs) Map.empty board

//solve: Primary solving function. Uses BFS to find the shortest path from initial location to goal
//queue: Queue<int*(int*int)>: Queue of timestamp*(position) to visit
//visisted: Set<int*int>: Set of visited nodes and a given timestamp
//board: Board: The current simulation board
//boardTime: int: The simulation timestamp of the board
//maxXY: int*int: Maximum X,Y coords for bounds checking
//goal: int*int: The goal location to visit
let rec solve queue visited board boardTime maxXY goal =
  let ((time,pos), queue') = deq queue
  //if timestamp dequeued = current timestamp, we must advance map to t+1.
  //This is because the map indicates which fields can be moved to.
  //Position implicitly indicates that we can stand there at current timestamp
  let (map', boardTime') =  if time = boardTime then
                              if boardTime%10=0 then printfn "    boardTime=%d. Visited %d so far" boardTime (Set.count visited)
                              (nextBoard board maxXY, time+1)
                            else
                              (board, boardTime)
  if pos = goal then
    printfn "Hit goal %A in %d steps" goal time
    (time, map')
  else
    let moves = List.fold (fun acc dxy -> let (b,xy') = canMove pos dxy map' maxXY
                                          if b then xy'::acc else acc
                          ) [] [(1,0); (0,1); (0,0); (-1,0); (0,-1)]
                |> List.map (fun m -> (time+1,m))
                |> List.filter (fun e -> not (Set.contains e visited))

    //To avoid unneccessary queueing, add to visited-set as soon as we enqueue them instead of when dequeueing
    let queue'' = List.foldBack enq moves queue'
    let visited' = List.foldBack Set.add moves visited
    solve queue'' visited' map' boardTime' maxXY goal

let solve1 inp =
  let board = parseInput inp
  let (maxX, maxY) = Seq.max (Map.keys board)
  let queue = Q ([], [])
  let queue' = enq (0, (0,-1)) queue
  solve queue' Set.empty board 0 (maxX+1, maxY+1) (maxX, maxY+1)

//solve2: Return map aswell. Use that to pathfind from (end) to (start), and then again from(start) to (end)
let solve2 inp =
  let board = parseInput inp
  let (maxX, maxY) = Seq.max (Map.keys board)
  let emptyQueue = Q ([], [])

  //Do three searches in a row. The second and third search pick up exactly where the previous search
  //left off, using the output map to find the next path
  let queue' = enq (0, (0,-1)) emptyQueue
  let (t1, m1) = solve queue' Set.empty board 0 (maxX+1, maxY+1) (maxX, maxY+1)

  let queue' = enq (t1, (maxX, maxY+1)) emptyQueue
  let (t2,m2) = solve queue' Set.empty m1 (t1+1) (maxX+1, maxY+1) (0,-1)

  let queue' = enq (t2, (0,-1)) emptyQueue
  let (t3,_) = solve queue' Set.empty m2 (t2+1) (maxX+1, maxY+1) (maxX, maxY+1)
  t3