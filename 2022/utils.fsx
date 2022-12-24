//Get all lines of the input file as a list
let getLines (fname: string): string list = List.ofArray (System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "/" + fname))

//Get all characters of the input file as a 2D-array, each entry holding one character
//Coordinates are (row,col) indexed
let getArray (fname: string) =
  let lines = getLines fname
  Array.ofList (List.map Array.ofSeq lines )

//Get all characters of the input file as a 2D-array, but map each character to an integer value instead
let getIntArray (fname: string) (delta: int) =
  let a = getArray fname
  Array.map ((Array.map (fun c -> (int c)-delta))) a


//Queue utility functions
type Queue<'a> = Q of 'a list * 'a list

let enq e = function
  | Q (eq,dq) -> Q(e::eq, dq)

let deq = function
  | Q ([], []) -> failwith "Empty queue, cannot dequeue"
  | Q (eq, d::dq) -> (d, Q(eq, dq))
  | Q (d::eq, []) -> (d, Q([], List.rev eq))

let qsz (Q (a,b)) = (List.length a + List.length b)

let qstr (Q (a,b)) = b @ List.rev a
