type action = 
    On of int
  | Off of int
  | Newline of int

type to_rle =
    Simple of action
  | Complex of action * string * action

type mcell_desc =
    Leaf of action list
  | Node of int * int * int * int * int
