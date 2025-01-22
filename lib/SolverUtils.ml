open Ast

let rec find_variable =
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

let substitute_literal =
 fun var value lit ->
  match lit with
  | Pos v when v = var -> Bool value
  | Neg v when v = var -> Bool (not value)
  | _ -> lit

let substitute_clause =
 fun var value (Clause lits) ->
  Clause (List.map (substitute_literal var value) lits)

let substitute_cnf =
 fun var value (CNF clauses) ->
  CNF (List.map (substitute_clause var value) clauses)

let evaluate_literal = function Bool b -> Some b | _ -> None

let evaluate_clause =
 fun (Clause lits) ->
  let rec eval_lits = function
    | [] -> Some false
    | lit :: rest -> (
        match evaluate_literal lit with
        | Some true -> Some true
        | Some false -> eval_lits rest
        | None -> None)
  in
  eval_lits lits

let evaluate_cnf =
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

let get_literals = function Clause lits -> lits

let initialize_activities formula =
  let collect_vars acc = function
    | CNF clauses ->
        List.fold_left
          (fun acc clause ->
            match clause with
            | Clause lits ->
                List.fold_left
                  (fun acc lit ->
                    match lit with
                    | Pos var | Neg var ->
                        if List.mem_assoc var acc then acc
                        else (var, 0.0) :: acc
                    | Bool _ -> acc)
                  acc lits)
          acc clauses
  in
  collect_vars [] formula

let bump_activity activities var decay_factor decay_period =
  let activity = try List.assoc var activities with Not_found -> 0.0 in
  let new_activity = activity *. 2.0 in
  let updated_activities =
    List.map
      (fun (v, act) ->
        if decay_period mod 256 = 0 then (v, act *. decay_factor) else (v, act))
      (List.remove_assoc var activities)
  in
  (var, new_activity) :: updated_activities
