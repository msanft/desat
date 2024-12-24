type expr =
  | Bool of bool
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Var of string

let rec dump_expr e =
  match e with
  | Bool b -> string_of_bool b
  | And (e1, e2) -> "(" ^ dump_expr e1 ^ " && " ^ dump_expr e2 ^ ")"
  | Or (e1, e2) -> "(" ^ dump_expr e1 ^ " || " ^ dump_expr e2 ^ ")"
  | Not e1 -> "!" ^ dump_expr e1
  | Var s -> s

let dump_expr_list exprs = String.concat ", " (List.map dump_expr exprs)
