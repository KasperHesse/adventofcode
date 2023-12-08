#load "utils.fsx"

open Utils

let input = getLines "d18.txt"
let test = getLines "d18test.txt"

let parseLine (line: string) =
  let x = line.Split ","
  (int x[0], int x[1], int x[2])

let parseInput inp =
  let xyz = List.map parseLine inp
  Set.ofList xyz

//Add two tuples
let tAdd (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

//surfaceArea: Computes the surface area of a set of voxels
//The surface area is given by the number of faces not touching another voxel in the set
//obj: Set<int*int*int>: The set of voxels to check
let rec surfaceArea obj =
  let delta = [(-1,0,0);(1,0,0);(0,-1,0);(0,1,0);(0,0,-1);(0,0,1)]
  Set.fold (fun cnt e -> (List.sumBy (fun d -> let other = tAdd e d
                                               if Set.contains other obj then 0 else 1) delta) + cnt) 0 obj

//findAllAir: Given the set of nodes occupied by lava, uses BFS to find all air surrounding the lava
//queue: Queue of unexplored nodes
//air: Set of nodes known to be air. Is updated continually on recursive calls
//lava: Set of nodes known to be lava
let rec findAllAir (queue) air lava =
  if queue = Q ([], []) then
    printfn "Exiting due to empty queue"
    air
  else

    let (xyz, q) = deq queue
    let delta = [(-1,0,0);(1,0,0);(0,-1,0);(0,1,0);(0,0,-1);(0,0,1)]
    let deltas = List.map (tAdd xyz) delta
    //For each delta within bounds and delta is NOT contained in lava or previously visited, add to air and enqueue
    let cands = List.filter (fun (x,y,z) -> 0<=x && x<20 && 0<=y && y<20 && 0<=z && z<20 && not (Set.contains (x,y,z) lava) && not (Set.contains (x,y,z) air)) deltas
    let air' = List.foldBack Set.add cands air
    let queue' = List.foldBack enq cands q
    findAllAir queue' air' lava

let solve1 inp =
  let xyz = parseInput inp
  surfaceArea xyz

let solve2 inp =
  let lava = parseInput inp
  let air = findAllAir (Q ([(0,0,0)], [])) (Set.singleton (0,0,0)) lava
  let all = Set.ofSeq (seq {for i in 0..19 do
                             for j in 0..19 do
                               for k in 0..19 do
                                  yield (i,j,k)})
  let rem = (Set.difference all (Set.union lava air))

  //rem is the set of nodes contained inside the lava. Surface area of the outside of the lava
  //bubble must be found by subtracting surface area of trapped air
  let surfLava = surfaceArea lava
  let surfInside = surfaceArea rem
  surfLava - surfInside

