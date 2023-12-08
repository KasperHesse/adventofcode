#load "utils.fsx"

open Utils
open System

let input = getLines "d21.txt"
let test = getLines "d21test.txt"

type Monkey = | V of float //Monkey with defined value
              | B of string * string * (float -> float -> float) //Monkey waiting on two monkeys and operation to perform

let parseLine (line: string) =
  let s = line.Split " "
  let name = s[0][0..3]

  match (Array.length s) with
  | 2 -> (name, V (float (s[1])))
  | _ ->  let a = s[1]
          let b = s[3]
          let f = match s[2] with
                  | "+" -> (+)
                  | "-" -> (-)
                  | "*" -> (fun x y -> x*y)
                  | "/" -> (/)
                  | _ -> failwithf "Unable to parse operation %s" s[2]
          (name, B (a, b, f))

let parseInput inp =
  List.map parseLine inp |> Map.ofList

//computeMonkey: Computes the number that a monkey will yell
//name: string: The name of the monkey to determine the number for
//ms: Map<string,Monkey>: Map of monkeys and their relationships
let rec computeMonkey name ms =
  match Map.find name ms with
  | V x -> x
  | B(an,bn,f) -> let a = computeMonkey an ms
                  let b = computeMonkey bn ms
                  f a b

let solve1 inp =
  let ms = parseInput inp
  let v = computeMonkey "root" ms
  int64 v

let solve2 inp =

  //The change in value of a is assumed linear depending on humn's value
  //Using two points to find the difference it provokes in the output
  let ms1 = Map.add "humn" (V 0.0) (parseInput inp)
  let ms2 = Map.add "humn" (V 1000.0) ms1

  let (B(an,bn,_)) = Map.find "root" ms1
  let a1 = computeMonkey an ms1
  let b1 = computeMonkey bn ms1
  let a2 = computeMonkey an ms2

  //Use difference between a1 and a2 to find multiplier
  let m = (a1-a2)/1000.0
  let diff = a1-b1
  //New value for humn should be that difference divided by multiplier
  let x = diff/m

  //Testing, just in case
  let ms3 = Map.add "humn" (V x) ms1
  let a3 = computeMonkey an ms3
  printfn "a3: %f   b1: %f   diff: %f   x=%d" a3 b1 (a3-b1) (int64 x)
  (int64 x)