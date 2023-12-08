#load "utils.fsx"

open Utils

let input = getLines "d11.txt"
let test = getLines "d11test.txt"

//Monkeys have list of items (worry levels), operation, div-test, true-monkey, false-monkey, num items inspected
type Monkey = M of uint64 list * (uint64 -> uint64) * uint64 * int * int * int

//Extract monkey information:
(*
  First line: index
  second line: starting items, starting from index 18
  third: operation. split on space, grab last. if "old" then old, else parse as number
                    second to last: only + or *, match on that
  fourth: test: Split on space, grab last
  fifth: last, throw to if true
  sixth: last, throw to if false
*)

let parseMonkey (inp: string list) =
  let getItems (l: string) =
    let s = ((l.Split(":"))[1]).Split(",") //split on : then split on , to extract all items for a monkey
    List.ofArray (Array.map uint64 s)

  let getOperation (l: string) =
    let s = (l.Split " ")
    let N = Array.length s
    match s[N - 2] with
    | "*" -> match s[N-1] with
             | "old" -> fun x -> x*x
             | n -> fun x -> x * (uint64 n)
    | "+" -> match s[N-1] with
             | "old" -> fun x -> x+x
             | n -> fun x -> x + (uint64 n)
    | _ -> failwithf "Did not recognize operator %s" s[N-2]

  let getLastInt (s: string) =
    let s' = s.Split " "
    int (s'[Array.length s' - 1])


  let items = getItems inp.[1]
  let op = getOperation inp.[2]
  let test = let s = (inp.[3]).Split (" ")
             uint64 (s[Array.length s - 1])
  let t = getLastInt (inp.[4])
  let f = getLastInt (inp.[5])
  M (items, op, test, t, f, 0)

//getMonkeys: Given the input string list, determines the monkey objects
let rec getMonkeys inp =
  match inp with
  | x::xs when x="" -> getMonkeys xs
  | l1::l2::l3::l4::l5::l6::xs -> let m = parseMonkey (l1::l2::l3::l4::l5::[l6])
                                  m::getMonkeys xs
  | _ -> []

//slightly faster append that runs in O(log n) than O(n)
let append x xs = List.rev (x::(List.rev xs))

//monkeyThrow: compute an item's updated worry level and give it to a new monkey
//itm: The original worry level of the item
//f: function used to compute new worry level
//div: Value to divide by after applying f
//test: Moduly test to perform
//ms: Map of <int,Monkey> holding all monkeys
//tr: monkey to throw item to if true
//fa: monkey to throw item to if false
let monkeyThrow itm f div test ms tr fa =
  let worry = (f itm) / div
  let x = if (worry % test = 0UL) then tr else fa
  Map.change x (function Some (M(itms,f,test,tr,fa,cnt)) -> Some (M (append worry itms, f, test, tr, fa, cnt)) | None -> failwithf "Did not find monkey with index %d" x) ms

//monkeyBizniz: Throws all items held by all monkeys
//Returns the state after throwing all items
let monkeyBizniz (ms: Map<int,Monkey>) i div =
  let (M (items, f, test, tr, fa, cnt)) = Map.find i ms
  //Fold over all items held by the monkey, updating state
  let ms' = List.fold (fun ms itm -> monkeyThrow itm f div test ms tr fa) ms items
  //And make the monkey hold nothing after doing so
  Map.add i (M ([], f, test, tr, fa, cnt + List.length items)) ms'

//monkeyRound: Performs one round of monkey business, throwing items between the monkeys
//Starts with monkey 0, terminates after monkey N-1 has finished throwing its items
let monkeyRound ms div =
  List.fold (fun ms i -> monkeyBizniz ms i div) ms [0..(Map.count ms - 1)]


//compute the least common multiple for a list of numbers
let rec lcm ints =
  let rec gcd a b =
    match compare a b with
    | -1 -> gcd a (b-a)
    | 0 -> a
    | 1 -> gcd (a-b) b
    | _ -> failwith "compare returned invalid"

  match ints with
  | x::y::ints -> let a = x * (y / (gcd x y))
                  let b = lcm (ints)
                  a * (b / (gcd a b))
  | x::_ -> x
  | [] -> 1UL

//General solver for the monkey problem
//div: Value to divide by after applying function f to a monkey's items
//r: Number of rounds to run for
let solve monkeys div r =
  let monkeyMap = Map.ofList (List.mapi (fun i m -> (i,m)) monkeys)

  //Perform 10000 rounds of monkey business
  let ms' = List.fold (fun ms _ -> monkeyRound ms div) monkeyMap [0..r-1]
  //Extract their cnt, sort, take two highest
  let top = Map.values ms'
            |> Seq.map (fun (M (_,_,_,_,_,cnt)) -> uint64 cnt)
            |> Seq.sortDescending
            |> Seq.take 2 |> Seq.toList

  top.[0] * top.[1]

let solve1 inp =
  let monkeys = getMonkeys inp
  solve monkeys 3UL 20

let solve2 inp =
  //Need to find another way of keeping worry levels manageable
  //Solution: Use modular arithmetic. All values should be taken modulo some X
  //Value of X: The least common multiple of the monkey's divisibility tests
  //(In reality, they're all prime so we can skip the LCM, but it's fun to have)
  let monkeys = getMonkeys inp
  let test = List.map (fun (M (_,_,t,_,_,_)) -> t) monkeys
  let LCM = lcm test
  //Update each monkey's function, generate map
  let monkeys' = List.map (fun (M (itms, f, div, tr, fa, cnt)) -> M (itms, (fun x -> (f x)%LCM), div, tr, fa, cnt)) monkeys
  solve monkeys' 1UL 10000