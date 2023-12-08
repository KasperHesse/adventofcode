#load "utils.fsx"

open Utils

let input = getLines "d25.txt"
let test = getLines "d25test.txt"

//int2snafu: Parse a uint64 value, returning the SNAFU representation of that value
//x: uint64: The value to parse
let int2snafu x =
  let rec loop x =
    if x=0UL then
      ""
    else
      match x % 5UL with
      | 0UL -> loop (x/5UL) + "0"
      | 1UL -> loop (x/5UL) + "1"
      | 2UL -> loop (x/5UL) + "2"
      | 3UL -> loop ((x+2UL)/5UL) + "="
      | 4UL -> loop ((x+1UL)/5UL) + "-"
      | _ -> failwithf "Unrecognized x%%5=%d" (x%5UL)

  loop x

//snafu2int: Convert a string representation of a SNAFU value to a uint64
//xs: The value to convert
let snafu2int xs =
  let rec loop xs m =
    match xs with
    | [] -> 0UL
    | x::xs' -> match x with
                | '0' -> loop xs' (m*5UL)
                | '1' -> loop xs' (m*5UL) + m
                | '2' -> loop xs' (m*5UL) + 2UL*m
                | '-' -> loop xs' (m*5UL) - 1UL*m
                | '=' -> loop xs' (m*5UL) - 2UL*m
                | _ -> failwithf "Unable to parse SNAFU character %c" x

  loop (List.ofSeq (Seq.rev xs)) 1UL

let solve1 inp =
  List.sumBy snafu2int inp |> int2snafu



