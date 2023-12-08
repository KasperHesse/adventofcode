#load "utils.fsx"

open Utils

let input = getIntArray "d12.txt" 0
let test = getIntArray "d12test.txt" 0

//vertex: Described by an int*int
type Vertex<'a> = V of 'a
//Edge: from this vertex to that vertex, with weight x
type Edge<'a> = E of int * Vertex<'a>


//genGraph: Given a 2D array of ints, builds a graph
//m: 2D array that the graph is built from
//G: Graph being built
//r: row of element under consideration
//c: Column of element under consideration
//update: Function used to update graph, adding new edges to vertices
//        The function should compare the element m[r][c] (current element) to that at m[rn][cn] (new element), and determine
//        if a new edge should be added. If so, add the edge and return the updated graph. Otherwise, should return old graph
let rec genGraph (m: int array array) G r c update =
  let G1 = update m G (r-1) c r c
  let G2 = update m G1 (r+1) c r c
  let G3 = update m G2 r (c-1) r c
  let G4 = update m G3 r (c+1) r c

  if c < (Array.length m[0])-1 then
    genGraph m G4 r (c+1) update
  else
    if r < (Array.length m)-1 then
      genGraph m G4 (r+1) 0 update
    else
      G4

//getMin: Given the set of remaining nodes and array shortest path estimates,
//finds the unexplored node with the lowest distance esimate
//rem: Set of remaining nodes
//dist: Array of distance estimates
//mapping: Function to map from Vertex -> Int
let getMin rem (dist: int array) mapping =
  let (minD, minC) = Set.fold (fun ((m, _) as tup) x ->
    if m < (dist[mapping x]) then tup else (dist[mapping x], x)) (1000, V (0,0)) rem
  (minC, minD)

//relax: Potentially relax an edge in the graph, updating the shortest-path estimate for anode
//dist: Array of distances to all nodes
//prev: Array of parents of nodes
//(thisK, thisD): Key of current node, known shortest path to that node
//E: Edge to the neighbouring node, with weight and key of that node
let relax (dist: int array) (prev: Vertex<'a> array) (thisK, thisD) (E (weight, thatK)) mapping =

  if thisD+weight < (dist[mapping thatK]) then
    dist[mapping thatK] <- thisD + weight
    prev[mapping thatK] <- thisK
  (dist, prev)

//dijkstra: Use Dijkstras algorithm to generate a shortest path tree
//N: Number of nodes in the map used to generate the graph
//G: Map<Vertex, Edge>
//src: Source coordinate to start at
//mapping: Vertex -> Int, for generating indexes into dist and prev-arrays
let dijkstra N G src mapping =
  let rec loop dist prev rem =
    if rem = Set.empty
    then (dist,prev)
    else
      let (minK, minV) = getMin rem dist mapping
      let nbs = Map.find minK G //all neighbours, list<edge>
      //For each neighbor, if minV+dist < nbs.dist, update dist and parent-estimate to this node
      List.fold (fun (d,p) nb -> relax d p (minK, minV) nb mapping) (dist, prev) nbs |> ignore
      loop dist prev (Set.remove minK rem)

  let dist = Array.replicate N 1000
  let prev = Array.replicate N (V (0,0))
  let rem = set (Map.keys G) //Remaining nodes to traverse

  dist[mapping src] <- 0
  loop dist prev rem

//mapSetup: Initial map setup for this problem
//          Convert the field with 'S' to 'a' and the field with 'E' to 'z'+1.
//          Returns (new map, start coords, finish coords)
//inp: 2D-array of int representing the characters
let mapSetup inp =
  //Must find the entry with value S=83 and re-map it to a=97, and then store that coordinate
  let a = int 'a'
  let z = int 'z'
  let E = int 'E'
  let S = int 'S'

  let r' = Array.findIndex (fun arr -> Array.contains S arr) inp
  let c' = Array.findIndex (fun c -> c=S) inp[r']
  let inp' = Array.updateAt r' (Array.updateAt c' a inp[r']) inp

  //Also find entry with value E=69 and remap it to z=121
  let r'' = Array.findIndex (fun c -> Array.contains E c) inp'
  let c'' = Array.findIndex (fun c -> c=E) inp[r'']
  (Array.updateAt r'' (Array.updateAt c'' z inp'[r'']) inp', (r',c'), (r'',c''))

let solve1 inp =
  let update m G rn cn r c =
    if (rn >= 0 && rn < Array.length m) && (cn >= 0 && cn < Array.length m[0])
    then  let x = m[rn][cn]
          if x <= (m[r][c]+1) then
            let e = E(1, V (rn, cn))
            Map.change (V (r,c)) (function None -> Some [e] | Some xs -> Some (e::xs)) G
          else
            G
    else G

  //Pre-process map, turning 'S' into 'a' and 'E' into 'z'. Extract start and finish-coords and generate Graph object
  let (m,start,finish) = mapSetup inp
  let G = genGraph m Map.empty 0 0 update

  //Mapping function used to index into array
  let mapping (V (r,c)) = r * (Array.length inp[0]) + c
  let N = ((Array.length inp) * (Array.length inp[0])) //Number of nodes in graph

  let (dist, _) = dijkstra N G (V start) mapping
  dist[mapping (V finish)]

let solve2 inp =
  let update m G rn cn r c =
    if (rn >= 0 && rn < Array.length m) && (cn >= 0 && cn < Array.length m[0])
    then  let x = m[rn][cn]
          //In problem 1, update required x <= m+1
          //Now, we just flip to condition: x >= m-1
          //A "better" solution would be to just flip the entire graph, but I don't want to do that
          if x >= (m[r][c]-1) then
            let e = E(1, V (rn, cn))
            Map.change (V (r,c)) (function None -> Some [e] | Some xs -> Some (e::xs)) G
          else
            G
    else G

  //Same setup as in problem 1, but with a slightly modifid update function
  let (m,start,finish) = mapSetup inp
  let G = genGraph m Map.empty 0 0 update
  let mapping (V (r,c)) = r * (Array.length inp[0]) + c
  let N = ((Array.length inp) * (Array.length inp[0]))

  //Now, find shortest path tree from finish node
  let (dist, _) = dijkstra N G (V finish) mapping

  //Find all indices of 'a' entries
  //Go through original list, map all items with their coordinates (r,c)
  let rcmap = List.map (fun (r,c) -> (r,c,m[r][c])) (List.allPairs [0..(Array.length inp - 1)] [0..(Array.length inp[0] - 1)])
  let candidates = List.filter (fun (_,_,v) -> v = (int 'a')) rcmap

  //Extract distance to all of those nodes from the finish line, take min as result
  let dists = List.map (fun (r,c,_) -> dist[mapping (V (r,c))]) candidates
  List.min dists