open Ast
open SolverUtils

type decision_info = { level : int; previous : int option }

type solver_state = {
  assignment : assignment;
  decision_levels : (string * decision_info) list;
  current_level : int;
  clauses : clause array;
  learned_clauses : clause list;
  variable_activity : (string * float) list;
  num_conflicts : int;
  next_restart : int;
  restart_count : int;
}

let get_literals = function Clause lits -> lits

let initial_state (CNF clauses) =
  {
    assignment = [];
    decision_levels = [];
    current_level = 0;
    clauses = Array.of_list clauses;
    learned_clauses = [];
    variable_activity = [];
    num_conflicts = 0;
    next_restart = 100;
    restart_count = 1;
  }

let get_level var state =
  match List.assoc_opt var state.decision_levels with
  | Some info -> info.level
  | None -> -1

let get_previous var state =
  match List.assoc_opt var state.decision_levels with
  | Some info -> info.previous
  | None -> None

let evaluate_literal lit assignment =
  match lit with
  | Pos var -> (
      match List.assoc_opt var assignment with
      | Some value -> Some value
      | None -> None)
  | Neg var -> (
      match List.assoc_opt var assignment with
      | Some value -> Some (not value)
      | None -> None)
  | Bool b -> Some b

let find_unit_or_conflict state =
  let check_clause clause_idx clause =
    let unassigned = ref None in
    let unassigned_count = ref 0 in
    let all_false = ref true in
    let lits = get_literals clause in

    List.iter
      (fun lit ->
        match evaluate_literal lit state.assignment with
        | None ->
            incr unassigned_count;
            unassigned := Some lit
        | Some true -> all_false := false
        | Some false -> ())
      lits;

    match (!unassigned_count, !all_false) with
    | 0, true -> `Conflict clause_idx
    | 1, true -> (
        match !unassigned with
        | Some lit -> `Unit (lit, clause_idx)
        | None -> assert false)
    | _ -> `None
  in

  let rec check_all_clauses idx =
    if idx >= Array.length state.clauses then `None
    else
      match check_clause idx state.clauses.(idx) with
      | `Conflict _ as result -> result
      | `Unit _ as result -> result
      | `None -> check_all_clauses (idx + 1)
  in
  check_all_clauses 0

let rec propagate state =
  match find_unit_or_conflict state with
  | `Conflict clause_idx -> [ clause_idx ]
  | `Unit (lit, clause_idx) ->
      let var, value =
        match lit with
        | Pos v -> (v, true)
        | Neg v -> (v, false)
        | Bool _ -> assert false
      in
      let new_state =
        {
          state with
          assignment = (var, value) :: state.assignment;
          decision_levels =
            (var, { level = state.current_level; previous = Some clause_idx })
            :: state.decision_levels;
        }
      in
      propagate new_state
  | `None -> []

let bump_variable_activity state var =
  {
    state with
    variable_activity = bump_activity state.variable_activity var 0.95;
  }

let analyze_conflict state conflict_clause_idx =
  let seen = Hashtbl.create 16 in
  let learned_literals = ref [] in
  let conflict_level = ref 0 in
  let state_ref = ref state in

  let rec analyze_recursive clause_idx count =
    let lits = get_literals !state_ref.clauses.(clause_idx) in
    List.iter
      (fun lit ->
        match lit with
        | Pos var | Neg var -> (
            state_ref := bump_variable_activity !state_ref var;
            match List.assoc_opt var !state_ref.assignment with
            | Some _ ->
                let level = get_level var !state_ref in
                if not (Hashtbl.mem seen var) then (
                  Hashtbl.add seen var true;
                  if level = !state_ref.current_level then incr count
                  else (
                    learned_literals := lit :: !learned_literals;
                    conflict_level := max !conflict_level level);
                  match get_previous var !state_ref with
                  | Some ante_idx when level = !state_ref.current_level ->
                      analyze_recursive ante_idx count
                  | _ -> ())
            | None -> ())
        | Bool _ -> ())
      lits
  in

  let count = ref 0 in
  analyze_recursive conflict_clause_idx count;

  let learned_clause = Clause !learned_literals in
  (!conflict_level, learned_clause, !state_ref)

let rec cdcl state =
  match propagate state with
  | [] -> (
      let satisfies_all_clauses =
        Array.for_all
          (fun clause ->
            let lits = get_literals clause in
            List.exists
              (fun lit ->
                match evaluate_literal lit state.assignment with
                | Some true -> true
                | _ -> false)
              lits)
          state.clauses
      in
      if satisfies_all_clauses then Some state.assignment
      else
        let unassigned_vars =
          List.filter
            (fun (var, _) -> not (List.mem_assoc var state.assignment))
            state.variable_activity
        in
        match unassigned_vars with
        | [] -> Some state.assignment
        | (next_var, _) :: _ -> (
            let new_state =
              {
                state with
                current_level = state.current_level + 1;
                assignment = (next_var, true) :: state.assignment;
                decision_levels =
                  ( next_var,
                    { level = state.current_level + 1; previous = None } )
                  :: state.decision_levels;
              }
            in
            match cdcl new_state with
            | Some result -> Some result
            | None ->
                let new_state =
                  {
                    state with
                    assignment =
                      (next_var, false)
                      :: List.remove_assoc next_var state.assignment;
                    decision_levels =
                      ( next_var,
                        { level = state.current_level + 1; previous = None } )
                      :: List.remove_assoc next_var state.decision_levels;
                  }
                in
                cdcl new_state))
  | conflicts ->
      if state.current_level = 0 then None
      else
        let conflict_idx = List.hd conflicts in
        let backtrack_level, learned_clause, updated_state =
          let backtrack_level', learned_clause', state' =
            analyze_conflict state conflict_idx
          in
          if backtrack_level' >= state'.current_level then
            ( min backtrack_level' (state'.current_level - 1),
              learned_clause',
              state' )
          else (backtrack_level', learned_clause', state')
        in

        let new_assignment =
          List.filter
            (fun (var, _) -> get_level var updated_state <= backtrack_level)
            updated_state.assignment
        in

        let new_decision_levels =
          List.filter
            (fun (_, info) -> info.level <= backtrack_level)
            state.decision_levels
        in

        let new_state =
          {
            state with
            assignment = new_assignment;
            decision_levels = new_decision_levels;
            current_level = backtrack_level;
            clauses = Array.append state.clauses [| learned_clause |];
            learned_clauses = learned_clause :: state.learned_clauses;
            num_conflicts = state.num_conflicts + 1;
          }
        in

        if new_state.num_conflicts >= new_state.next_restart then
          let next_restart base_restart n =
            let rec luby i =
              if i <= 1 then 1
              else if i land 1 = 0 then luby (i lsr 1)
              else luby (i - 1) lsl 1
            in
            base_restart * luby n
          in
          cdcl
            {
              new_state with
              current_level = 0;
              assignment = [];
              decision_levels = [];
              next_restart = next_restart 30 (new_state.restart_count + 1);
              restart_count = new_state.restart_count + 1;
            }
        else cdcl new_state

let solve_cdcl formula =
  let initial = initial_state formula in
  let initial_with_activities =
    { initial with variable_activity = initialize_activities formula }
  in
  cdcl initial_with_activities
