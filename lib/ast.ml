type expr =
  | Num of float
  | Var of string
  | Let of string * expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
