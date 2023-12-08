#load "utils.fsx"
open System.Text.RegularExpressions
open Utils

let input = getLines "d5.txt"
let test = getLines "d5test.txt"

//Split the input into two sets: One set contains the starting stacks in reverse order,
//the other set contains the moves to be performed
let rec splitInput inp acc =
  match inp with
  | x::xs when x="" -> (xs, acc)
  | x::xs -> splitInput xs (x::acc)
  | _ -> failwith "Hit end of input"

//Get the total number of stacks by extracting the last element in the series
let getNumStacks (names: string) =
  names.Split " "
  |> Array.last
  |> int

//Parse the stacks to get the starting state
let parseStacks stacks N =
  let rec parseStack (stacks: string list) i acc =
    match stacks with
    | [] -> acc
    | x::xs -> let c = x.[4*i+1]
               if c = ' ' then acc
               else parseStack xs i (c::acc)

  let rec loop = function
  | n when n < N-1 -> (parseStack stacks n [])::loop (n+1)
  | n -> [parseStack stacks n []]

  loop 0

//Parse the moves, generating a list of tuples instead. tuples are (amount, from, to)
let parseMoves moves =
  let re = Regex @"move (\d+) from (\d+) to (\d+)"
  List.map (fun m ->
              let groups = re.Match(m).Groups
              (int groups[1].Value, int groups[2].Value, int groups[3].Value)) moves

//Updating function be used for part1. Move items off one stack and onto another one at at ime
let rec update1 from too cnt =
  match cnt with
  | 0 -> (from, too)
  | n -> update1 (List.tail from) ((List.head from)::too) (n-1)

let rec update2 from too cnt =
  let (h,t) = (List.splitAt cnt from)
  (t, h@too)

let solve inp update =
  let (moves, names::stacks) = splitInput inp []
  let N = getNumStacks names
  let stacks = Seq.toList (parseStacks stacks N)
  let moves = parseMoves moves

  //Apply it and do stuff
  //For each input, update state and use updated state
  let rec solveInt moves (state: 'a list) =
    match moves with
    | [] -> state
    | (cnt, from, too)::moves' -> let (fromN, toN) = update (state.[from-1]) (state.[too-1]) cnt
                                  solveInt moves' (List.updateAt (too-1) toN (List.updateAt (from-1) fromN state))

  List.fold (fun a b -> a + string (List.head b)) "" (solveInt moves stacks)

solve input update1
solve input update2