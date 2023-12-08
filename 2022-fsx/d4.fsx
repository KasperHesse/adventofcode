let i1 = List.ofSeq (System.IO.File.ReadLines "d4.txt")
let i2x = "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"
let i2 = List.ofArray (i2x.Split "\n")

let extractSections (s: string) =
  let s2 = s.Split ","
  let (s0,s1) = (s2[0].Split "-", s2[1].Split "-")
  ((int s0[0], int s0[1]), (int s1[0], int s1[1]))

let isContainedBy (min1,max1) (min2,max2) =
  min1 >= min2 && max1 <= max2

let rec score inp acc  =
  match inp with
  | x::xs -> let (s0,s1) = extractSections x
             let p = if (isContainedBy s0 s1) || (isContainedBy s1 s0) then 1 else 0
             score xs (acc + p)
  | _ -> acc

(*
  overlapping pairs: if max of range1 is contained in range2
*)
let isOverlappedBy (min1,max1) (min2,max2) =
  min2 <= max1 && max1 <= max2

let rec score2 inp acc  =
  match inp with
  | x::xs -> let (s0,s1) = extractSections x
             let p = if (isOverlappedBy s0 s1) || (isOverlappedBy s1 s0) then 1 else 0
             score2 xs (acc + p)
  | _ -> acc