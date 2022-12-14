//Get all lines of the input file as a list
let getLines (fname: string): string list = List.ofArray (System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "/" + fname))

//Get all characters of the input file as a 2D-array, each entry holding one character
//Coordinates are (row,col) indexed
let getArray (fname: string) =
  let lines = getLines fname
  Array.ofList (List.map Array.ofSeq lines )

let getIntArray (fname: string) (delta: int) =
  let a = getArray fname
  Array.map ((Array.map (fun c -> (int c)-delta))) a