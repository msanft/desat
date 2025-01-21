open Ast

let rec find_variable : cnf -> string option =
 fun (CNF clauses) ->
  let rec find_in_clause : clause -> string option =
   fun (Clause lits) ->
    match lits with
    | [] -> None
    | Pos v :: _ -> Some v
    | Neg v :: _ -> Some v
    | Bool _ :: rest -> find_in_clause (Clause rest)
  in
  match clauses with
  | [] -> None
  | Clause [] :: _ -> None
  | clause :: rest -> (
      match find_in_clause clause with
      | Some v -> Some v
      | None -> find_variable (CNF rest))

let substitute_literal : string -> bool -> literal -> literal =
 fun var value lit ->
  match lit with
  | Pos v when v = var -> Bool value
  | Neg v when v = var -> Bool (not value)
  | _ -> lit

let substitute_clause : string -> bool -> clause -> clause =
 fun var value (Clause lits) ->
  Clause (List.map (substitute_literal var value) lits)

let substitute_cnf : string -> bool -> cnf -> cnf =
 fun var value (CNF clauses) ->
  CNF (List.map (substitute_clause var value) clauses)

let evaluate_literal : literal -> bool option = function
  | Bool b -> Some b
  | _ -> None

let evaluate_clause : clause -> bool option =
 fun (Clause lits) ->
  let rec eval_lits : literal list -> bool option = function
    | [] -> Some false
    | lit :: rest -> (
        match evaluate_literal lit with
        | Some true -> Some true
        | Some false -> eval_lits rest
        | None -> None)
  in
  eval_lits lits

let evaluate_cnf : cnf -> bool option =
 fun (CNF clauses) ->
  let rec eval_clauses : clause list -> bool option = function
    | [] -> Some true
    | clause :: rest -> (
        match evaluate_clause clause with
        | Some false -> Some false
        | Some true -> eval_clauses rest
        | None -> None)
  in
  eval_clauses clauses

let find_unit_clause : cnf -> (string * bool) option =
 fun (CNF clauses) ->
  let rec find_in_clause : literal list -> (string * bool) option =
   fun lits ->
    match lits with
    | [] -> None
    | [ Pos v ] -> Some (v, true)
    | [ Neg v ] -> Some (v, false)
    | Bool false :: rest -> find_in_clause rest
    | _ -> None
  in
  let rec search_clauses = function
    | [] -> None
    | Clause lits :: rest -> (
        match find_in_clause lits with
        | Some result -> Some result
        | None -> search_clauses rest)
  in
  search_clauses clauses

let find_pure_literal : cnf -> (string * bool) option =
 fun (CNF clauses) ->
  let rec collect_occurrences : clause list -> string list * string list =
   fun cls ->
    match cls with
    | [] -> ([], [])
    | Clause lits :: rest ->
        let rec process_literals : literal list -> string list * string list =
         fun lits ->
          match lits with
          | [] -> ([], [])
          | lit :: rest_lits -> (
              let pos_vars, neg_vars = process_literals rest_lits in
              match lit with
              | Pos v -> (v :: pos_vars, neg_vars)
              | Neg v -> (pos_vars, v :: neg_vars)
              | Bool _ -> (pos_vars, neg_vars))
        in
        let pos_vars_clause, neg_vars_clause = process_literals lits in
        let pos_vars_rest, neg_vars_rest = collect_occurrences rest in
        (pos_vars_clause @ pos_vars_rest, neg_vars_clause @ neg_vars_rest)
  in
  let pos_vars, neg_vars = collect_occurrences clauses in
  let rec find_pure : string list -> string list -> (string * bool) option =
   fun pos neg ->
    match pos with
    | [] -> (
        match neg with
        | [] -> None
        | v :: _ when not (List.mem v pos) -> Some (v, false)
        | _ :: rest -> find_pure [] rest)
    | v :: _ when not (List.mem v neg) -> Some (v, true)
    | _ :: rest -> find_pure rest neg
  in
  find_pure pos_vars neg_vars

let rec dpll : cnf -> assignment -> assignment option =
 fun formula current_assignments ->
  match evaluate_cnf formula with
  | Some true -> Some current_assignments
  | Some false -> None
  | None -> (
      match find_unit_clause formula with
      | Some (var, value) ->
          let formula_prop = substitute_cnf var value formula in
          dpll formula_prop ((var, value) :: current_assignments)
      | None -> (
          match find_pure_literal formula with
          | Some (var, value) ->
              let formula_pure = substitute_cnf var value formula in
              dpll formula_pure ((var, value) :: current_assignments)
          | None -> (
              match find_variable formula with
              | None -> Some current_assignments
              | Some var -> (
                  let formula_true = substitute_cnf var true formula in
                  match
                    dpll formula_true ((var, true) :: current_assignments)
                  with
                  | Some result -> Some result
                  | None ->
                      let formula_false = substitute_cnf var false formula in
                      dpll formula_false ((var, false) :: current_assignments)))
          ))

let solve : cnf -> assignment option = fun formula -> dpll formula []
