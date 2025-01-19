open Desat.Ast
open Desat.Dpll

let mk_literal_pos v = Pos v
let mk_literal_neg v = Neg v
let mk_clause lits = Clause lits
let mk_cnf clauses = CNF clauses

let test_sat (formula : cnf) (expected : bool) () : unit =
  let actual = match solve formula with Some _ -> true | None -> false in
  Alcotest.(check bool) "satisfiability" expected actual

let test_assignment (formula : cnf) (expected : assignment) () : unit =
  match solve formula with
  | None -> Alcotest.fail "Expected SAT, got UNSAT"
  | Some result ->
      (* Sort assignments for consistent comparison *)
      let sort_assignments = List.sort compare in
      let actual = sort_assignments result in
      let expected = sort_assignments expected in
      Alcotest.(check (list (pair string bool)))
        "assignment values" expected actual

let () =
  let open Alcotest in
  run "DPLL Tests"
    [
      ( "satisfiability",
        [
          test_case "Empty CNF is SAT" `Quick (test_sat (mk_cnf []) true);
          test_case "Single variable is SAT" `Quick
            (test_sat (mk_cnf [ mk_clause [ mk_literal_pos "x" ] ]) true);
          test_case "Simple SAT case" `Quick
            (test_sat
               (mk_cnf
                  [
                    mk_clause [ mk_literal_pos "x"; mk_literal_neg "y" ];
                    mk_clause [ mk_literal_neg "x"; mk_literal_pos "y" ];
                  ])
               true);
          test_case "Simple UNSAT case" `Quick
            (test_sat
               (mk_cnf
                  [
                    mk_clause [ mk_literal_pos "x" ];
                    mk_clause [ mk_literal_neg "x" ];
                  ])
               false);
          test_case "Complex UNSAT case" `Quick
            (test_sat
               (mk_cnf
                  [
                    mk_clause [ mk_literal_pos "x"; mk_literal_pos "y" ];
                    mk_clause [ mk_literal_neg "x"; mk_literal_pos "y" ];
                    mk_clause [ mk_literal_neg "y" ];
                  ])
               false);
        ] );
      ( "assignments",
        [
          test_case "Single variable assignment" `Quick
            (test_assignment
               (mk_cnf [ mk_clause [ mk_literal_pos "x" ] ])
               [ ("x", true) ]);
          test_case "Two variable assignment" `Quick
            (test_assignment
               (mk_cnf
                  [
                    mk_clause [ mk_literal_pos "x" ];
                    mk_clause [ mk_literal_neg "y" ];
                  ])
               [ ("x", true); ("y", false) ]);
          test_case "OR clause assignment" `Quick
            (test_assignment
               (mk_cnf [ mk_clause [ mk_literal_pos "x"; mk_literal_pos "y" ] ])
               [ ("x", true) ]);
        ] );
    ]
