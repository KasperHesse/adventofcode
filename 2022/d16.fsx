#load "utils.fsx"

open Utils
open System.Text.RegularExpressions
open System.Collections.Generic

let input = getLines "d16.txt"
let test = getLines "d16test.txt"

let re = Regex @"Valve (\w+)[^\d]+(\d+)[^\d]+valves? ([\w ,]*)"

type Location = string
type OpenValves = Set<Location>
type Flow = int

type Memo = Map<OpenValves, int>
type MemoD = Dictionary<OpenValves, Flow>
type TravelTimes = Map<Location, Map<Location,int>>

//parseLine: Parse a line of the input file.
//Returns a tuple contained (valve name, valve flow, list of adjacent rooms)
let parseLine line =
  let g = re.Match(line).Groups
  let name = g[1].Value
  let flow = int g[2].Value
  let k: string = "abc"
  let pairedConns = Array.fold (fun l (s: string) -> s.Trim ' '::l) [] ((g[3].Value).Split ",")
  (name, flow, pairedConns)

//setup: Perform initial setup to solve the problem
//Returns a tuple of (flow map, connection map)
//  flow map contains the flow of each room, connection map the list of all adjacent rooms for each room
let setup inp =
  let valves = List.map parseLine inp
  //Create map from room -> associated flow rate
  let flowMap = Map.ofList (List.map (fun (n,f,_) -> (n,f)) valves)
  //Mapping from room -> set of connected rooms
  let connMap = Map.ofList (List.map (fun (n,_,c) -> (n,c)) valves)
  (flowMap, connMap)

//performFlow: Perform the flow optimization problem.
//Dynamic programming at force. Updates a memoization variable (memo) which stores the maximum attainable flow for
//  any combination of open valves
//memo: Map<Set<Location>,int>: Maximum flow attainable for a given combination of open valves
//mins: int: How many minutes we have been running around. Initialize to 1
//finish: int: How many minutes we have total
//loc: Location: Location currently occupied
//valves: Set<Location>: Set of all currently opened valves
//fm: Map<Location,Flow>: Flow map used to compute total flow
//tt: Map<Location,int>: Map of travel time between each room with a non-zero flow valve
//flow: Flow: Total flow that the currently open valves will contribute until 'finish' minutes have passed
let rec performFlow memo mins finish loc valves fm tt flow =
  let updateMemo memo valves newFlow =
    Map.change valves (function
                        | None -> Some newFlow
                        | Some x -> Some (max x newFlow)
    ) memo

  //Update stored flow in this state
  let memo2 = updateMemo memo valves flow

  //Find all connections that are still unexplored. These satisfy all three predicates
    //1: Valve is still not opened
    //2: Valve can be reached within the time limit
    //3: Valve has non-zero flow
  let validCons = Map.filter (fun loc2 dist ->
                              (not (Set.contains loc2 valves)) &&
                              mins+dist<=finish &&
                              (Map.find loc2 fm > 0)) (Map.find loc tt)

  //Process all valid connections
  Map.fold (fun m loc2 dist ->
    let valves' = Set.add loc2 valves
    let newFlow = flow + (Map.find loc2 fm)*(finish-(mins+dist))
    performFlow m (mins+dist+1) finish loc2 valves' fm tt newFlow
  ) memo2 validCons

//Generate a map of travel times from node -> node
//We're only interested in the travel times between nodes that actually have valves with non-zero flow
//cm: Map<Location, Location list>: Connection map generated by setup function
let rec travelTimes cm: TravelTimes =
  let time = Map.empty<string, Map<string, int>>
  let nodes = List.ofSeq (Map.keys cm)

  //Initialize the map for easier processing
  //All connected nodes have distance 1, all unconnected are initialized to 1000
  let time2 = (Seq.fold (
    fun t1 n -> (Seq.fold (
      fun t2 m -> let w = if (List.contains m (Map.find n cm)) then 1 else 1000
                  Map.change n (function
                                | None -> Some (Map.ofList [(m,w)])
                                | Some x -> Some (Map.add m w x)) t2
      )
    t1 nodes)
  ) time nodes)

  //Use Floyd-Warshall to calculate shortest path between all pairs of nodes
  Seq.fold (fun t1 k ->
    Seq.fold (fun t2 i ->
      Seq.fold (fun t3 j ->
        //T[i][j] = min(T[i][j], T[i][k] + T[k][j])
        let tij = Map.find j (Map.find i t3)
        let tik = Map.find k (Map.find i t3)
        let tkj = Map.find j (Map.find k t3)
        //Get T[i], update T[i][j], update T[i] in T
        let m_ti = Map.find i t3
        let m_ti' = Map.add j (min tij (tik+tkj)) m_ti
        Map.add i m_ti' t3
      ) t2 nodes
    ) t1 nodes
  ) time2 nodes


let solve1 inp finish =
  let (fm, cm) = setup inp
  let tt = travelTimes cm
  let res = performFlow Map.empty 1 finish "AA" Set.empty fm tt 0
  Seq.max (Map.values res)

let solve2 inp finish =
  let (fm, cm) = setup inp
  let tt = travelTimes cm
  let res = performFlow Map.empty 1 finish "AA" Set.empty fm tt 0

  //For problem 2, we must find the two non-overlapping sets of valves which together grant the largest
  //amount of flow.
  //To do so, checking all pairs of valves. To reduce search space, only combinations where more than 1/3
  //of valves are turned on are checked
  let fmsz = Map.count (Map.filter (fun _ v -> v>0) fm)
  let filt = Map.toList (Map.filter (fun s _ -> Set.count s > (fmsz/3)) res)
  let pairs = List.allPairs filt filt
  let start = ((Set.empty, 0), (Set.empty, 0))

  let best = List.fold (fun (((_,sf1), (_,sf2)) as state) (((v1, f1), (v2,f2)) as inp) ->
                        let i = Set.intersect v1 v2
                        if i = Set.empty && f1+f2 > sf1+sf2 //non-overlapping valves and better flow than current result
                        then inp
                        else state) start pairs
  let ((_,f1), (_,f2)) = best
  (f1+f2, best)