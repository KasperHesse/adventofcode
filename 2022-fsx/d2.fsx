let input = System.IO.File.ReadAllText "d2.txt"
let i2 = "A Y
B X
C Z"
//A,B,C: Rock, Paper, Scissors
//X,Y,Z: Rock, Paper, Scissors
//Score: shape selected (1,2,3) + outcome (0 loss, 3 draw, 6 win)

(*
  For each line
  calculate points due to selection + points due to outcome
*)

let parse (i: string) =
  List.ofArray (i.Split "\n")
  |> List.map (fun x -> (string x.[0], string x.[2]))


let choicePoints = function
  | "X" | "A" -> 1
  | "Y" | "B" -> 2
  | "Z" | "C" -> 3
  | x -> failwith (sprintf "unknown input %s" x)

let outcomePoints = function
  | ("A", "X") -> 3
  | ("A", "Y") -> 6
  | ("A", "Z") -> 0

  | ("B", "X") -> 0
  | ("B", "Y") -> 3
  | ("B", "Z") -> 6

  | ("C", "X") -> 6
  | ("C", "Y") -> 0
  | ("C", "Z") -> 3
  | x -> failwith (sprintf "unknown input %s" (string x))

let rec score xs acc =
  match xs with
  | [] -> acc
  | (x,y)::xs' -> score xs' (acc + (choicePoints y) + (outcomePoints (x,y)))

//based on whether we need to win, lose or draw, we select a value
//if we need to lose, we get 0 points + the score for our choice
//if we need to win, we get 6 points + our choice
//if we need to draw, we get 3 points + choice


//opponent -> (wins to,loses to,draw)
//rock     -> (sciss,  paper,   rock)

//losing depends on what the opponent chose
let choicePoints2 (x,y) =
  match y with
  | "X" -> (*lose*) match x with
                    | "A" -> 3 //lose to rock = scissors
                    | "B" -> 1 //lose to paper = rock
                    | "C" -> 2 //lose to scissors = paper
                    | z -> failwith (sprintf "unknown argument %s" z)
  | "Y" -> (*draw*) match x with
                    | "A" -> 1
                    | "B" -> 2
                    | "C" -> 3
                    | z -> failwith (sprintf "unknown argument %s" z)
  | "Z" -> (*win*)  match x with
                    | "A" -> 2 //win to rock = paper
                    | "B" -> 3 //win to paper = scissors
                    | "C" -> 1 //win to paper = rock
                    | z -> failwith (sprintf "unknown argument %s" z)

let outcomePoints2 (x,y) =
  match y with //desired outcome
  | "X" -> 0 + choicePoints2 (x,y)
  | "Y" -> 3 + choicePoints2 (x,y)
  | "Z" -> 6 + choicePoints2 (x,y)
  | z -> failwith (sprintf "unknown input %s" z)

let rec score2 xs acc =
  match xs with
  | [] -> acc
  | xy::xs' -> score2 xs' (acc + outcomePoints2 xy)