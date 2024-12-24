type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Var of string

let rec dump_expr e =
  match e with
  | Int i -> string_of_int i
  | Add (e1, e2) -> "(" ^ dump_expr e1 ^ " + " ^ dump_expr e2 ^ ")"
  | Sub (e1, e2) -> "(" ^ dump_expr e1 ^ " - " ^ dump_expr e2 ^ ")"
  | Mul (e1, e2) -> "(" ^ dump_expr e1 ^ " * " ^ dump_expr e2 ^ ")"
  | Div (e1, e2) -> "(" ^ dump_expr e1 ^ " / " ^ dump_expr e2 ^ ")"
  | Var s -> s
