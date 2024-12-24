type expr = Int of int | Add of expr * expr | Var of string

let rec dump_expr e =
  match e with
  | Int i -> "Int " ^ string_of_int i
  | Add (e1, e2) -> dump_expr e1 ^ " + " ^ dump_expr e2
  | Var s -> "Var " ^ s
