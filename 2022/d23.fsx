#load "utils.fsx"

open Utils

let input = getLines "d23.txt"
let test = getLines "d23test.txt"

//parseLine: Parse a line, returning a list of (x,y) coordinates containing elves on that line
let parseLine (y, line) =
  Seq.fold (fun acc (x,c) -> if c='.' then acc else (x,y)::acc) [] (Seq.indexed line)

let parseInput inp =
  Set.ofList (List.collect parseLine (List.indexed inp))

(*
  On each iteration:
  For each entry in the set, apply the 4 checkers-function in order.
    Before checking moves, check if we should stand still
  The first one which indicates that we can move that direction, we add that (from,to) pair to the list of moves
  After doing this, we do List.distinctBy to filter out any moves that would land in the same square
  Then we remove (from), add (to) for each

  Repeat until list of moves BEFORE pruning is empty
*)

//shouldRemain: Returns true if an elf should *not* move because all surrounding tiles are empty
let shouldRemain (x,y) s =
  List.forall (fun (dx,dy) -> not (Set.contains (x+dx, y+dy) s)) [(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(1,0);(1,1)]

//c1-c4 return a bool AND the updated coordinates
//returns true if we can move that direction

let checkWest (x,y) s =
  //north -> y-1
  //east: x+1
  //south: y+1
  //west: x-1
  let b = List.forall (fun dy -> not (Set.contains (x-1, y+dy) s)) [-1;0;1]
  (b, (x-1, y))

let checkNorth (x,y) s =
  let b = List.forall (fun dx -> not (Set.contains (x+dx, y-1) s)) [-1;0;1]
  (b, (x, y-1))

let checkEast (x,y) s =
  let b = List.forall (fun dy -> not (Set.contains (x+1, y+dy) s)) [-1;0;1]
  (b, (x+1, y))

let checkSouth (x,y) s =
  let b = List.forall (fun dx -> not (Set.contains (x+dx, y+1) s)) [-1;0;1]
  (b, (x, y+1))

let rec move s (c1,c2,c3,c4) i exit =
  let moves = Set.fold (fun acc xy -> if shouldRemain xy s then
                                          acc
                                         else
                                          let (b1, xy1) = c1 xy s
                                          let (b2, xy2) = c2 xy s
                                          let (b3, xy3) = c3 xy s
                                          let (b4, xy4) = c4 xy s
                                          match (b1,b2,b3,b4) with
                                          | (true,_,_,_) -> (xy,xy1)::acc //from,to coordinates
                                          | (_,true,_,_) -> (xy,xy2)::acc
                                          | (_,_,true,_) -> (xy,xy3)::acc
                                          | (_,_,_,true) -> (xy,xy4)::acc
                                          | _ -> acc
                                          ) [] s

  //Cannot use distinctBy. Instead, use a filter and exists-check.
  //There cannot exist an item (f',t') where f'=f and t'<>t
  //Instead of filtering, we map each to their t-value. However, if the f',t' predicate is true, we map them to the f-value instead
  let moves' = List.filter (fun (f,t) -> not (List.exists (fun (f',t') -> t=t' && f<>f') moves)) moves
  if i%50 = 0 then printfn "Iteration %d. %d moves (%d filtered)" i (List.length moves) (List.length moves')
  if moves' = [] || i=exit then
    if i=exit then printfn "EARLY EXIT DUE TO I=EXIT"
    (s,i)
  else
    //Moves where no elves move to the same destination (x,y) coordinates
    let newSet = List.fold (fun s' (f, t) -> Set.add t (Set.remove f s')) s moves'
    move newSet (c2,c3,c4,c1) (i+1) exit

let solve1 inp =
  let s = parseInput inp
  let (res,_) = move s (checkNorth, checkSouth, checkWest, checkEast) 0 10
  let (xs,ys) = Set.fold (fun (xs,ys) (x,y) -> (x::xs,y::ys)) ([],[]) res

  let W = (List.max xs) - (List.min xs) + 1
  let H = (List.max ys) - (List.min ys) + 1
  (W,H,Set.count res, W*H - Set.count res, Set.toList res)

let solve2 inp =
  let s = parseInput inp
  let (res,i) = move s (checkNorth, checkSouth, checkWest, checkEast) 0 10000
  i+1