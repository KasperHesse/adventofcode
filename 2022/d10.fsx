#load "utils.fsx"

open Utils

let input = getLines "d10.txt"
let test = getLines "d10test.txt"

let test2 = List.ofArray (("noop
addx 3
addx -5
").Split "\n")

type Instr = | Nop | Addx of int

let parseInstr (i: string) =
  match (i.Split " ") with
  | [|"noop"|] -> Nop
  | [|"addx"; n|] -> Addx (int n)
  | [|""|] -> Nop //Final newline, just parse as NOP, should do anything different
  | x -> failwith (sprintf "Unable to parse instruction %A" x)

let rec exec is state =
  match is with
  | [] -> state
  | Nop::is' -> exec is' ((List.head state)::state)
  | (Addx n)::is' -> let h = List.head state
                     exec is' ((h+n)::h::state)

let solve1 inp =
  let insns = List.map parseInstr inp
  let res = (List.rev (exec insns [1]))

  let cycles = [20;60;100;140;180;220]
  let strengths = List.map (fun c -> res.[c-1]*c) cycles
  (List.sum strengths, res)

(*
  Part 2: X-register controls *middle* of sprite (ie x is probably between 0 and 40)
  For each element in the list: If index % 40 is close to x (x-1, x or x+1), map to #,
  otherwise map to .
*)

let solve2 inp =
  let insns = List.map parseInstr inp
  let res = (List.rev (exec insns [1]))
  let mapped = List.mapi (fun i x -> if (List.contains (i%40) [x-1;x;x+1]) then "#" else ".") res //Map to # and .)

  //split into groups for 40, and then apply function to those
  let lines = List.chunkBySize 40 mapped
  let f l = List.fold (fun s c -> s+c) "" l //
  List.fold (fun s l -> s + (f l) + "\n") "\n" lines
  // List.s


