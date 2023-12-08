#load "utils.fsx"

open Utils
open System.Text.RegularExpressions

let input = getLines "d15.txt"
let test = getLines "d15test.txt"

//Pattern to extract values from each line
let re = Regex @"[^\d]+(\d+)[^\d]+(\d+)[^\d]+(\d+)[^\d]+(\d+)"

//getSensorsBeacons: Given the list of input strings, return a list of ((int*int), (int*int))
//in each tuple, the first value is the (x,y) coords for a sensor and the second is (x,y) for the closest beacon
let getSensorsBeacons inp =
  List.fold (fun s line -> let (g: GroupCollection) = re.Match(line).Groups
                           ((int g[1].Value, int g[2].Value), (int g[3].Value, int g[4].Value))::s) [] inp

(*
  Find the minimum and maximum x-coordinate for a beacon
  Go through each sensor-beacon pair.
  1) Find the manhattan distance between the two
  2) Find the offset dy from our line of interest
  3) Fill in all spaces surrounding that point with our remaining distance
  4) if m = manhattan and dy is difference, then we fill in (sx - (m-dy)/2..sx + (m-dy)/2)
*)

//manhattan: Compute the manhattan distance between a sensor and a beacon
let manhattan ((sx,sy), (bx, by)) =
  abs (sx-bx) + abs (sy-by)

let pointsOnLine y ((sx, sy), (bx, by)) =
  let m = manhattan ((sx,sy), (bx,by))
  let dy = abs (sy-y)
  let r = m - dy
  let xs = [(sx-(m-dy))..(sx+(m-dy))]
  xs

let rec computePoints inp s y =
  match inp with
  | [] -> s
  | x::xs -> let pol = pointsOnLine y x
             let s' = List.fold (fun s' x' -> Set.add x' s') s pol
             computePoints xs s' y

let solve1 inp =
  let sb = getSensorsBeacons inp
  let points = computePoints sb (Set.empty) 2000000
  Set.count points - 1

//getDiamondLines: For a given sensor and beacon, extracts the slope and y-intercept of the 4 lines that
//define the perimeter of the diamond
let getDiamondLines ((sx,sy), (bx,by)) =
  //all slopes are either -1 or +1
  let intersect m (x,y) =
    y-(m*x)

  let m = manhattan ((sx,sy), (bx,by))
  let (u,d) = (sy+m, sy-m)
  (*
    Remember we have a flipped coordinate system. m2 and m3 have positive slope, m1 and m4 have negative
       #
   m1 ### m2
     #####
   m3 ### m4
       #
  *)
  let bs = List.map (fun (m, xy) -> intersect m xy) [(-1, (sx,d)); (1, (sx,d)); (1, (sx,u)); (-1, (sx,u))]
  List.zip [-1;1;1;-1] bs

//findCandidates: Given a list of slopes and y-intercepts, checks if a given y-intercept has a possible
//match in the list (a match has b'=b+2)
//Assumes that the list of slopes and y-intercepts is sorted by y-intercept
let rec findCandidates = function
  //For each element, check if the remaining list contains an element such that abs (b1-b2)=2
  | (m,b)::mbs -> if List.exists (fun (_,b2) -> abs (b-b2)=2) mbs
                  then (b,b+2)::findCandidates mbs
                  else findCandidates mbs
  | _ -> []

//outsideSensorRange: Checks if a given coordinate (x,y) is outside the range of all sensors
//sb: List of sensor-beacon coordinates
//(x,y): coordinate to check
let outsideSensorRange sb (x,y) =
  let m = List.map manhattan sb
  let m' = List.map manhattan (List.map (fun ((sx,sy),_) -> ((sx,sy), (x,y))) sb) //manhattan from sensor to (x,y)
  //Zip them. All m' should be greater than corresponding m for this to be a candidate
  List.forall (fun (m1, m2) -> m2>m1) (List.zip m m')

let solve2 inp =
  (*
    By extracting the slope and intersect of all lines (4*N), we can compare them against each other. If two lines
    have the same slope but their y-intersect differs by two, there is a chance for the point being between them
  *)
  let sb = getSensorsBeacons inp
  let eqs = List.collect getDiamondLines sb
  let (pos,neg) = List.partition (fun (m,_) -> m=1) (List.distinct (List.sortBy (fun (_,b) -> b) eqs))

  //With the positive and negative candidates, we find the coordinates where those intersect
  //m1*x+b1=m2*x+b2 => x=(b2-b1)/(m1-m2)
  //Since m1,m2 can only be -1,1, (m1-m2) can only be (-1-(1))=-2 or (1-(-2)) = 2. Since x,y>0, we just take the abs of (b2-b1)/2 to calculate x
  let (posCand, negCand) = (findCandidates pos), (findCandidates neg)
  //Get all pairs of possible line intersections
  let pairs = List.allPairs (posCand) (negCand)

  //From a pair of lines, extract the (x,y)-coordinates they are centered around
  let rec extractCenter = function
  | ((b1,b2), (b3,b4))::bs -> let x = (abs (b1-b3)/2 + abs (b1-b4)/2 + abs (b2-b3)/2 + abs (b2-b4)/2)/4 //calculates all x-coords. Center coord must be average of those 4
                              let y = ((x+b1) + (x+b2) + (-x+b3) + (-x+b4))/4 //Likewise for y-coordinates
                              (x,y)::extractCenter bs
  | [] -> []

  //Finally, filter the candidates for valid (x,y) coordinates and compute their tuning frequency
  let candPairs = List.filter (fun p -> outsideSensorRange sb p) (extractCenter pairs)
  let r = List.map (fun (x,y) -> (x,y,(uint64 x)*4000000UL+(uint64 y))) candPairs
  r


