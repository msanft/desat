open Ast
open SolverUtils

type solver_state = {
  formula : cnf;
  assignments : assignment;
  variable_activity : (string * float) list;
}

let find_unit_clause =
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

let find_pure_literal =
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

let bump_variable_activity state var =
  {
    state with
    variable_activity = bump_activity state.variable_activity var 0.95;
  }

let choose_variable state =
  let unassigned_vars =
    List.filter
      (fun (var, _) ->
        not
          (List.exists
             (fun (assigned_var, _) -> assigned_var = var)
             state.assignments))
      state.variable_activity
  in
  match unassigned_vars with
  | [] -> None
  | vars ->
      let chosen_var, _ =
        List.fold_left
          (fun (best_var, best_activity) (var, activity) ->
            if activity > best_activity then (var, activity)
            else (best_var, best_activity))
          (List.hd vars) (List.tl vars)
      in
      Some chosen_var

let rec dpll state =
  match evaluate_cnf state.formula with
  | Some true -> Some state.assignments
  | Some false -> None
  | None -> (
      match find_unit_clause state.formula with
      | Some (var, value) ->
          let new_formula = substitute_cnf var value state.formula in
          let new_state =
            {
              formula = new_formula;
              assignments = (var, value) :: state.assignments;
              variable_activity = state.variable_activity;
            }
          in
          dpll (bump_variable_activity new_state var)
      | None -> (
          match find_pure_literal state.formula with
          | Some (var, value) ->
              let new_formula = substitute_cnf var value state.formula in
              let new_state =
                {
                  formula = new_formula;
                  assignments = (var, value) :: state.assignments;
                  variable_activity = state.variable_activity;
                }
              in
              dpll (bump_variable_activity new_state var)
          | None -> (
              match choose_variable state with
              | None -> Some state.assignments
              | Some var -> (
                  let try_value value =
                    let new_formula = substitute_cnf var value state.formula in
                    let new_state =
                      {
                        formula = new_formula;
                        assignments = (var, value) :: state.assignments;
                        variable_activity = state.variable_activity;
                      }
                    in
                    dpll (bump_variable_activity new_state var)
                  in
                  match try_value true with
                  | Some result -> Some result
                  | None -> try_value false))))

let solve_dpll formula =
  let initial_state =
    {
      formula;
      assignments = [];
      variable_activity = initialize_activities formula;
    }
  in
  dpll initial_state
