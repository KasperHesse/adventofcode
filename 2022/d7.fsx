#load "Utils.fsx"

open Utils

let input = getLines "d7.txt"
let test = getLines "d7test.txt"

type FileSys = Element list //contents of directory
and Element = | File of int * string //size, filename
              | Dir of string * FileSys //dirname, dir contents

//Commands on the input
type Command = | Cd of string
               | Ls
               | E of Element

(*
  Solution:
  1:  Convert text-commands into representation with type Command.
      Makes parsing and processing slightly easier
  2:  Build tree. Assumes that we only ever CD into a directory or and only
      LS that directory once
  3:  Scan directory tree, create list of all directories which are <= max
      Do this recursively, then sum over all elements of list
*)

//converts command strings into Commands for easier encoding/decoding
let cmdToCommand (cmd: string) =
  let split = cmd.Split " "
  match (Array.length split) with
  | 3 -> Cd split[2]
  | 2 -> match (split[0], split[1]) with
         | ("$",_) -> Ls
         | ("dir", n) -> E (Dir (n, []))
         | (size, n) -> E (File (int size, n))
  | _ -> failwith (sprintf "Unable to parse command %s" cmd)

//doLs: Command list -> Element list -> Command * Element list
//parses commands from the command list until a CD or LS command is found.
//At that point, returns a tuple containing the remaining commands in [0] and the contents of the directory in [1]
let rec doLs cmds state =
  match cmds with
  | [] -> ([], state)
  | E x::cmds' -> doLs cmds' (x::state)
  | _ -> (cmds, state)

//generateTree: Command list -> Element list -> Command * Element list
//recursively generates the tree data structure of the file system
//returns the remaining commands (hopefully just an empty list) and the filesystem tree
let rec generateTree cmds state =
  match cmds with
  | [] -> ([], state)
  | Cd x::cmds' -> match x with
                   | "/" -> generateTree cmds' state//Do nothing, only doing this once and skipping it
                   | ".." -> (cmds', state) //When going back, we return contents of this dir
                   //cd into a directory: Find index of that dir in current directory list, parse that dir, then update state and keep aprsing
                   | x -> let idx = List.findIndex (function Dir (n,_) when n=x -> true | _ -> false) state
                          let (remCmds, newState) = generateTree cmds' []
                          //Keep parsing commands
                          generateTree remCmds (List.updateAt idx (Dir (x, newState)) state)
  | Ls::cmds' -> let (cmds'', state'') = doLs cmds' state //state'' is contents of this directory
                 generateTree cmds'' state''
  | E e::cmds' -> failwith (sprintf "generateTree should never encounter an element E %A" e)

//Finds the size of a directory (filesystem)
//This is the size of all files in the directory + all nested directories
let rec fsSize fs =
  match fs with
  | [] -> 0
  | File (sz,_)::fs' -> sz + fsSize fs'
  | Dir (_,fs')::fs'' -> (fsSize fs') + fsSize (fs'')

//recurses through the tree, finding the size of all directories
//returns a list of the sizes of all directories which are at most max bits large
//tree: File system tree being parsed
//acc: Accumulator holding all sizes <= max
//max: The max value that a dir can have
let rec findDirectories tree acc max =
  let (files, dirs) = List.partition (function File (_,_) -> true | _ -> false) tree

  //Fold over subdirectories to update the accumulator
  let acc2 = List.fold (fun a d -> match d with
                                   | Dir(_,fs) -> findDirectories fs a max
                                   | _ -> a) acc dirs
  //Sum of file-sizes and dir-sizes in this directory
  let mysize = fsSize tree
  // let sumFiles = fsSize files
  // let sumDirs = fsSize dirs

  //If sum of this dirs files and directories is less than max, also add us
  if mysize <= max then mysize::acc2 else acc2
  // if (sumFiles + sumDirs <= max) then (sumFiles + sumDirs)::acc2 else acc2

//Problem 1: Find the sum of the sizes of all directories which are smaller than
//or equal to 100_000
let solve1 inp =
  let (_,tree) = generateTree (List.map cmdToCommand inp) []
  let dirs = findDirectories tree [] 100000
  List.sum dirs

//Problem 2: We have 70_000_000 bits of memory, and we need 30_000_000 to perform
//the software update. What is the smallest dir we can delete and get enough space?
let solve2 inp =
  let (_,tree) = generateTree (List.map cmdToCommand inp) []
  let totSize = fsSize tree
  let needed = 30000000 - (70000000 - totSize)
  let dirs = findDirectories tree [] totSize //just getting all dirs to make it easier

  let candidates = (List.filter (fun d -> d >= needed) dirs)
  // (totSize, candidates, List.min candidates)
  List.min candidates