let getLines (fname: string): string list = List.ofArray (System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "/" + fname))