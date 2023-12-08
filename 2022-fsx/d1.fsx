let input = System.IO.File.ReadAllText "d1.txt"

let s = List.ofArray (input.Split "\n")

//parse input line-by-line
//if empty string, start new element
//otherwise, add to current head element

let i2 = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"
let i2s = List.ofArray (i2.Split "\n")

let rec genSum xs r a =
  match xs with
  | [] -> r
  | x::xs' -> if x = "" then
                                    genSum xs' (a::r) 0
                                    else let a' = a + (int x)
                                         genSum xs' r a'

let findMax xs n = List.take n (List.rev (List.sort (genSum xs [] 0)))

let sumMax xs n = List.sum (findMax xs n)