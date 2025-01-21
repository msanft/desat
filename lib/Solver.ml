open Ast
open Dpll
open Cdcl

type approach = Dpll | Cdcl

let solve (a : approach) (formula : cnf) : assignment option =
  match a with Dpll -> solve_dpll formula | Cdcl -> solve_cdcl formula
