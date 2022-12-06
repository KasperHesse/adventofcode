#load "utils.fsx"
open Utils

let i: string list = getLines "d6.txt"
let t: string list= getLines "d6test.txt"

let input = Seq.toList i.[0]
let test = Seq.toList t.[0]

//We could do fancy parsing with a finite automata, or we could just use a sliding window of the four most recently
//encountered characters.
//We'll use an array of size N (our buffer) and then traverse the list input

//create a buffer of the first N items of a list
let createBuf (inp: 'a list) N =
  Array.init N (fun i -> inp.[i])

//Drop the first N items of a list, returning the sublist after that point
let rec drop xs = function
  | 0 -> xs
  | n when n > 0 -> drop (List.tail xs) (n-1)
  | n -> failwith (sprintf "Invalid argument %s" (string n))

//Checks how many characters we must go into a sequence before the most recent N characters are unique
let findFirstDistinctN inp N =
  let rec loop xs buf i N max =
    match xs with
    | [] -> N
    | x::xs -> let N' = (N+1)
               Array.set buf i x
               if (Array.distinct buf) = buf then N'
               else let i = (i+1)%max
                    loop xs buf i N' max

  let buf = createBuf inp N
  if (Array.distinct buf) = buf
  then N
  else loop (drop inp N) buf 0 N N

let getStartOfPacket inp =
  findFirstDistinctN inp 4

let getStartOfMessage inp =
  findFirstDistinctN inp 14