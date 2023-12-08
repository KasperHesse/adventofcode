#load "utils.fsx"


open Utils
open System.Text.RegularExpressions

let input = getLines "d13.txt"
let test = getLines "d13test.txt"

//parseInput: Converts the input list of strings into a list of token-lists where all commas have been filtered out
//inp: string list
let parseInput inp =
  let re = Regex @"(\[|\]|\d+)"

  let inp' = List.filter (fun s -> String.length s > 0) inp
  //For the remaining lines, extract all tokens
  let tok = List.map (fun s ->
                      let matches = re.Matches(s)
                      List.ofSeq (Seq.map (fun (m: Match) -> m.Groups[1].Value) matches)
                      ) inp'
  tok

//comp: Compare two string lists of tokens, checking if the left list is sorted respective to the right
//left: string list of tokens
//right: string list of tokens
let rec comp left right =
  // printfn "left=%A      right=%A" left right
  match (left, right) with
  | ("["::ls, "["::rs) -> comp ls rs //start list, continue
  | ("]"::ls, "]"::rs) -> comp ls rs //end list, continue
  | (x::ls, "]"::rs)   -> false //left list is longer than right list -> false
  | ("]"::ls, x::rs)   -> true //right list is longer than left list -> true
  | ("["::ls, x::rs)   -> comp (ls) (x::"]"::rs) //list vs int -> map that int to a list and re-parse
  | (l::ls, "["::rs)   -> comp (l::"]"::ls) (rs)
  | (l::ls, r::rs)     -> let (LL, RR) = (int l, int r)
                          if LL=RR then comp ls rs else LL<RR
  | ([], _::_) -> true //left list empty and right non-empty -> true
  | (_::_, []) -> false //right list empty and left non-empty -> false
  | ([], []) -> false

let solve1 inp =
  let lines = parseInput inp
  let N = (List.length lines)/2
  List.map (fun i -> if (comp lines.[i*2] lines.[i*2+1]) then i+1 else 0) [0..N-1] |>
  List.sum

//Part 2: Use comp as a sorting function
//Then, find the entrises with [[2]] and [[6]]
let solve2 inp =
  //comparer function
  let c left right = if (comp left right) then -1 else 1

  let lines = parseInput ("[[2]]"::"[[6]]"::inp)
  let sorted = List.sortWith c lines

  let two = List.findIndex (fun l -> l = ["["; "["; "2"; "]"; "]"]) sorted
  let six = List.findIndex (fun l -> l = ["["; "["; "6"; "]"; "]"]) sorted
  (two+1)*(six+1)
