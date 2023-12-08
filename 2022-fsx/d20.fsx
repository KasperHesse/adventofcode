#load "utils.fsx"

open Utils

let input = getLines "d20.txt"
let test = getLines "d20test.txt"

let parseInput (inp: string list) =
  List.map (fun s -> int s) inp

//Unsignd modulo function
let unsMod i m = int ((i%m + m) % m)

//moveItems: Move items throughout the linked list
//idxs: int list: List of indices in the order they should be manipulated
//items: (int*int64) list: List of (index,item) values to be manipulated
//modulo: int64: The modulo value to use when determining new indexes
let rec moveItems idxs items modulo =
  match idxs with
  | [] -> items
  | idx::idxs' -> let listIdx = List.findIndex (fun (i,_) -> i=idx) items
                  let (_,v) = List.item listIdx items

                  let newList = List.removeAt listIdx items
                  let newIdx = unsMod ((int64 listIdx)+v) modulo
                  let items' = List.insertAt newIdx (idx,v) newList
                  moveItems idxs' (items') modulo

//General solver function
//inp: The input to process
//mult: int64: Multiplier to add to all values before processing
//rounds: int: The number of rounds to perform shuffling
let solve inp mult rounds =
  let items = parseInput inp
              |> List.map (fun v -> (int64 v)*mult)
              |> List.indexed

  let L = List.length inp
  let modulo = int64 (L-1)

  let res = List.fold (fun items' i -> printfn "%d" i;
                                       moveItems [0..(L-1)] items' modulo) items [0..(rounds-1)]
            |> List.map (fun (_,v) -> v)

  let i0 = List.findIndex ((=)0L) res
  let i1 = res.[(i0+1000)%L]
  let i2 = res.[(i0+2000)%L]
  let i3 = res.[(i0+3000)%L]
  i1+i2+i3

let solve1 inp =
  solve inp 1L 1

let solve2 inp =
  solve inp 811589153L 10