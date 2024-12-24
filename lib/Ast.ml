type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Var of string

let rec dump_expr e =
  match e with
  | Int i -> string_of_int i
  | Add (e1, e2) -> "(" ^ dump_expr e1 ^ " + " ^ dump_expr e2 ^ ")"
  | Sub (e1, e2) -> "(" ^ dump_expr e1 ^ " - " ^ dump_expr e2 ^ ")"
  | Mul (e1, e2) -> "(" ^ dump_expr e1 ^ " * " ^ dump_expr e2 ^ ")"
  | Div (e1, e2) -> "(" ^ dump_expr e1 ^ " / " ^ dump_expr e2 ^ ")"
  | And (e1, e2) -> "(" ^ dump_expr e1 ^ " && " ^ dump_expr e2 ^ ")"
  | Or (e1, e2) -> "(" ^ dump_expr e1 ^ " || " ^ dump_expr e2 ^ ")"
  | Not e1 -> "!" ^ dump_expr e1
  | Var s -> s

let dump_expr_list exprs = String.concat ", " (List.map dump_expr exprs)
