open Ast

let counter = ref 0

let next_var : unit -> string =
 fun () ->
  incr counter;
  "t" ^ string_of_int !counter

(* helper for tests, should only be used internally! *)
let reset_var_counter () = counter := 0
let literal_to_clause (lit : literal) : clause = Clause [ lit ]

let equisat_and (p : string) (a : string) (b : string) : clause list =
  [
    Clause [ Neg p; Pos a ];
    Clause [ Neg p; Pos b ];
    Clause [ Pos p; Neg a; Neg b ];
  ]

let equisat_or (p : string) (a : string) (b : string) : clause list =
  [
    Clause [ Neg p; Pos a; Pos b ];
    Clause [ Pos p; Neg a ];
    Clause [ Pos p; Neg b ];
  ]

let equisat_not (p : string) (a : string) : clause list =
  [ Clause [ Neg p; Neg a ]; Clause [ Pos p; Pos a ] ]

let equisat_imp (p : string) (a : string) (b : string) : clause list =
  [
    Clause [ Neg p; Neg a; Pos b ];
    Clause [ Pos p; Pos a ];
    Clause [ Pos p; Neg b ];
  ]

let equisat_eq (p : string) (a : string) (b : string) : clause list =
  [
    Clause [ Neg p; Neg a; Pos b ];
    Clause [ Neg p; Pos a; Neg b ];
    Clause [ Pos p; Neg a; Neg b ];
    Clause [ Pos p; Pos a; Pos b ];
  ]

let rec tseitin (expr : boolean_expr) : string * cnf =
  match expr with
  | Const true -> ("true", CNF [])
  | Const false -> ("false", CNF [ Clause [] ])
  | Var v -> (v, CNF [])
  | Not e ->
      let v_e, CNF cs_e = tseitin e in
      let p = next_var () in
      let new_clauses = equisat_not p v_e in
      (p, CNF (new_clauses @ cs_e))
  | And es -> (
      match es with
      | [] -> ("true", CNF [])
      | [ e ] -> tseitin e
      | [ a; b ] ->
          let v_a, CNF cs_a = tseitin a in
          let v_b, CNF cs_b = tseitin b in
          let p = next_var () in
          let new_clauses = equisat_and p v_a v_b in
          (p, CNF (new_clauses @ cs_a @ cs_b))
      | e :: es' -> tseitin (And [ e; And es' ]))
  | Or es -> (
      match es with
      | [] -> ("false", CNF [ Clause [] ])
      | [ e ] -> tseitin e
      | [ a; b ] ->
          let v_a, CNF cs_a = tseitin a in
          let v_b, CNF cs_b = tseitin b in
          let p = next_var () in
          let new_clauses = equisat_or p v_a v_b in
          (p, CNF (new_clauses @ cs_a @ cs_b))
      | e :: es' -> tseitin (Or [ e; Or es' ]))
  | Imp (a, b) ->
      let v_a, CNF cs_a = tseitin a in
      let v_b, CNF cs_b = tseitin b in
      let p = next_var () in
      let new_clauses = equisat_imp p v_a v_b in
      (p, CNF (new_clauses @ cs_a @ cs_b))
  | Eq (a, b) ->
      let v_a, CNF cs_a = tseitin a in
      let v_b, CNF cs_b = tseitin b in
      let p = next_var () in
      let new_clauses = equisat_eq p v_a v_b in
      (p, CNF (new_clauses @ cs_a @ cs_b))

let to_cnf (expr : boolean_expr) : cnf =
  let v, CNF clauses = tseitin expr in
  match expr with
  | Const true -> CNF [ Clause [ Pos "true" ] ]
  | Const false -> CNF [ Clause [] ]
  | _ -> CNF (Clause [ Pos v ] :: clauses)
