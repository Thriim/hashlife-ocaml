
type ident = string
type filename = string

type expr =
  | EVar of ident
  | EUnion of expr * expr
  | EInter of expr * expr
  | EDiff of expr * expr
  | EReadRle of filename
  | EReadMc of filename
  | EIter of expr * int
  | EMirror of expr
  | ERot90 of expr
  | ERot180 of expr
  | ERot270 of expr
  | EExtend of expr
  | ELxtd of expr
  | ERxtd of expr

type decl = ident * expr

type statement =
  | Draw of expr
  | OutRle of expr * filename
  | OutMc of expr * filename

type file = decl list * statement
