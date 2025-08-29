open Ast

type env = (string * float) list

let rec eval env = function
  | Num n -> n
  | Var id -> (
      try List.assoc id env
      with Not_found -> failwith ("Undefined variable: " ^ id))
  | Let (id, e1, e2) ->
      let v1 = eval env e1 in
      eval ((id, v1) :: env) e2
  | Add (e1, e2) -> eval env e1 +. eval env e2
  | Sub (e1, e2) -> eval env e1 -. eval env e2
  | Mul (e1, e2) -> eval env e1 *. eval env e2
  | Div (e1, e2) -> eval env e1 /. eval env e2
