#load "utils.fsx"

open Utils

let input = getLines "d9.txt"
let test = getLines "d9test.txt"
let test2 = getLines "d9test2.txt"

(*
  Rope pulling

  When condition is satisfied, set tail = head. Otherwise, only move head
  right: If tail.x < head.x, increase tail.x. Otherwise, only increment head.x
    - If changing tail.x, *always* set tail.y = head.y
  left: If head.x < tail.x
  up: If tail.y < head.y, increase tail.y. Otherwise, only increment head.y
*)

let rec move (dir,n) (hx,hy) (tx,ty) tp =
  let tp' = Set.add (tx,ty) tp
  match (dir,n) with
  | (_,0) -> ((hx,hy), (tx,ty), tp')
  | ("R", n) -> if tx < hx
                then move (dir,n-1) (hx+1, hy) (hx,hy) tp'
                else move (dir,n-1) (hx+1, hy) (tx,ty) tp'
  | ("L", n) -> if hx < tx
                then move (dir,n-1) (hx-1, hy) (hx,hy) tp'
                else move (dir,n-1) (hx-1, hy) (tx,ty) tp'
  | ("U", n) -> if ty < hy
                then move (dir,n-1) (hx, hy+1) (hx,hy) tp'
                else move (dir,n-1) (hx, hy+1) (tx,ty) tp'
  | ("D", n) -> if hy < ty
                then move (dir,n-1) (hx, hy-1) (hx,hy) tp'
                else move (dir,n-1) (hx, hy-1) (tx,ty) tp'
  | _ -> failwith (sprintf "Unknown input combination (%s,%d)" dir n)

//moveRope: Move the rope along an unseen grid, tracking all positions that the tail of the rope have occupied
//cmds: List of commands. Each entry is a tuple of char, int, representing the number of moves
let rec moveRope cmds head tail tp =
  match cmds with //TODO take the n's into account
  | [] -> tp
  | (dir, n)::cmds' -> let (h', t', tp') = move (dir,n) head tail tp
                       moveRope cmds' h' t' tp'

let solve1 (inp: string list) =
  let cmds = List.map (fun (s: string) -> let x = s.Split " "
                                          (x[0], int x[1])) inp
  let r = moveRope cmds (0,0) (0,0) Set.empty<int * int>
  // (r, Set.count r)
  Set.count r

//head is coordinates of head-element after it was moved
//if we enter this state, we KNOW that head was moved, and question is whether to move tail
//if predicate is satisfied, we also move tail according to offset
//This means that last element (x::[]) will already have been moved and no further propagation needs to happen
let rec propagateMove pos =
  match pos with
  | ((hx,hy)::(tx,ty)::pos') -> let (dx, dy) = (hx-tx, hy-ty)
                                // printfn "h=(%d,%d), t=(%d,%d), d=(%d,%d)" hx hy tx ty dx dy
                                if abs(dx) > 1 || abs (dy) > 1 //if any delta > 1, next element should also be moved
                                then
                                  //By integer division we force values into range [-1,0,-1]
                                  let dx = if dx <> 0 then dx / (abs dx) else 0
                                  let dy = if dy <> 0 then dy / (abs dy) else 0
                                  (hx, hy)::(propagateMove ((tx+dx,ty+dy)::pos'))
                                else
                                  // printfn "PM: Did not match. h=(%d,%d), t=(%d,%d). Returning %A" hx' hy' tx ty ((hx',hy')::(tx,ty)::pos')
                                  //otherwise, just move this element and call it a day
                                  (hx, hy)::(tx,ty)::pos'
  | _ -> pos

//move2: Scalable knot-moving algorithm. Moves the head of the rope in one step, then
//propagates moves on the next knots by using propagateMove
let rec move2 (dir,n) (pos: (int * int) list) (tp: Set<int * int>) =
  let tp' = Set.add (List.last pos) tp
  match pos with
  | [] -> ([], tp')
  | (hx,hy)::pp ->
    match (dir,n) with
    | (_,0) -> (pos, tp')
    | ("R", n) -> let pos' = propagateMove ((hx+1,hy)::pp)
                  move2 (dir, n-1) pos' tp'
    | ("U", n) -> let pos' = propagateMove ((hx,hy+1)::pp)
                  move2 (dir, n-1) pos' tp'
    | ("L", n) -> let pos' = propagateMove ((hx-1,hy)::pp)
                  move2 (dir, n-1) pos' tp'
    | ("D", n) -> let pos' = propagateMove ((hx,hy-1)::pp)
                  move2 (dir, n-1) pos' tp'
    | _ -> failwith "Not yet implemented"

//moveRope2: Move the rope along an unseen grid, tracking all positions that the knots of the rope have occupied
//cmds: List of commands. Each entry is a tuple of string, int, representing the direction number of moves
let rec moveRope2 cmds pos tp =
  match cmds with
  | [] -> tp
  | (dir, n)::cmds' ->  let (pos', tp') = move2 (dir,n) pos tp
                        moveRope2 cmds' pos' tp'

let solve2 inp =
  let cmds = List.map (fun (s: string) -> let x = s.Split " "
                                          (x[0], int x[1])) inp
  let pos = List.init 10 (fun _ -> (0,0))
  let r = moveRope2 cmds pos Set.empty<int * int>
  Set.count r

//Solve problem 1 using the problem 2 solution
let solve1x inp =
  let cmds = List.map (fun (s: string) -> let x = s.Split " "
                                          (x[0], int x[1])) inp
  let pos = List.init 2 (fun _ -> (0,0))
  let r = moveRope2 cmds pos Set.empty<int * int>
  Set.count r