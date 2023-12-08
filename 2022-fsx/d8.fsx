#load "Utils.fsx"

open Utils

let input = getLines "d8.txt"
let test = getLines "d8test.txt"
let t2 = seq test

(*

  Problem can be solved in O(n+m) time by scanning along each row, and then along each column
  Once a tree of height 9 is found, we can terminate early
*)

//scan: scan a row or column of the map, finding the indices of the visible trees
//cs: List of characters being scanned, containing their index as well
//N: Height of currently largest tree
//i: Index we're currently parsing
let rec scan cs N i =
  if N = 10 then [] else //N=10 is an early exit condition
  match cs with
  | [] -> []
  | c::cs' when c > N -> i::(scan cs' c (i+1))
  | _::cs' -> scan cs' N (i+1)

//scanRow: Scan a row of the tree map, returning the (r,c) coordinates of all visible trees
let scanRow r cs =
  List.map (fun c -> (r,c)) (scan cs 0 0)

//scanRow: Scan a column of the tree map, returning the (r,c) coordinates of all visible trees
let scanCol c cs =
  List.map (fun r -> (r,c)) (scan cs 0 0)

//scanMap: Scan a map of integers, returning the coordinates of all visible trees
let scanMap m =
  let R = List.length m (*Num rows*)
  let C = List.length (List.head m) (*Num cols*)

  //Scan all rows left->right
  let lr' = List.mapi scanRow m
  let lr = List.collect id lr'

  //Scan all rows right->left
  let rl' = List.mapi scanRow (List.map List.rev m)
  let rl = List.collect (List.map (fun (r,c) -> (r, C-c-1))) rl'

  let m2 = List.transpose m
  //Scan top->bottom
  let ud' = List.mapi scanCol m2
  let ud = List.collect id ud'
  //Scan bottom->top
  let du' = List.mapi scanCol (List.map List.rev m2)
  let du = List.collect (List.map (fun (r,c) -> (R-r-1, c))) du'

  //Filter out trees visible from multiple sides by using List.distinct
  List.distinct (List.collect id [lr; rl; ud; du])

//Conver the input string list to an (int list list)
let toIntMap inp = List.map (fun s -> List.ofSeq (Seq.map (fun (c: char) -> int c) s)) inp

let solve1 inp =
  let intMap = toIntMap inp
  List.length (scanMap intMap)

//part 2:
//Finding the tree with the highest viewing distance
//Naive algorithm: O(n*m + n + m), parsing each tree at a time
//Better algorithm: Dynamic programming, use memoization. If we can see over
//the tree (right/left/above/below) us, we can find the viewing distance without recomputing
//requires two passes, one going topleft -> bottomright (finds distances up and to the left),
//and another pass going bottomright->topleft (finds distances down and to the right)
//Using the naive approach for now

//scenicScore: Given the int-map of the forest and the (r,c) coordinates of a field,
//calculates the scenic score for that field
let scenicScore map r c =
  let D = Array.length map
  //Extract slices of the map.
  //Then, use tryFindIndex to find first element larger than us
  //If no element is found, we can see the border -> return length of list
  let right = List.ofArray (Array.sub map[r] (c+1) (D-c-1))
  let left = List.rev (List.ofArray (Array.sub map[r] 0 c))
  let up = List.map   (fun r -> map[r][c]) [r-1..-1..0]
  let down = List.map (fun r -> map[r][c]) [r+1..D-1]

  //For each element, find index of first value which is >= current value
  let getDist l =
    List.tryFindIndex (fun v -> v>=map[r][c]) l
    |> function None -> List.length l | Some x -> x+1

  let rv = getDist right
  let lv = getDist left
  let uv = getDist up
  let dv = getDist down
  rv*lv*uv*dv

let solve2 inp =
  let intMap = toIntMap inp
  let intArr = Array.ofList (List.map Array.ofList intMap)

  //compute scenicScore for all elements of the map, find max in that list
  let R = [0 .. (Array.length intArr)-1]
  let C = [0 .. (Array.length intArr[0])-1]
  let scores = List.map (fun (r,c) -> scenicScore intArr r c) (List.allPairs R C)
  List.max scores
