open Desat.Ast

(* Helper functions to construct test cases more easily *)
let mk_literal_pos v = Pos v
let mk_literal_neg v = Neg v
let mk_clause lits = Clause lits
let mk_cnf clauses = CNF clauses

let test_dump_cnf (cnf : cnf) (expected : string) () : unit =
  let actual = string_of_cnf cnf in
  Alcotest.(check string) "dump_cnf" expected actual

let test_dump_clause (clause : clause) (expected : string) () : unit =
  let actual = string_of_clause clause in
  Alcotest.(check string) "dump_clause" expected actual

let test_dump_assignment (assgn : assignment) (expected : string) () : unit =
  let actual = string_of_assignment assgn in
  Alcotest.(check string) "dump_assignment" expected actual

let () =
  let open Alcotest in
  run "AST Tests"
    [
      ( "dump_cnf",
        [
          test_case "Single variable" `Quick
            (test_dump_cnf
               (mk_cnf [ mk_clause [ mk_literal_pos "foo" ] ])
               "foo");
          test_case "Multiple clauses" `Quick
            (test_dump_cnf
               (mk_cnf
                  [
                    mk_clause [ mk_literal_pos "x"; mk_literal_pos "y" ];
                    mk_clause [ mk_literal_neg "a"; mk_literal_neg "b" ];
                    mk_clause [ mk_literal_neg "z" ];
                  ])
               "(x || y) && (!a || !b) && !z");
          test_case "Assignment with CNF" `Quick
            (test_dump_assignment
               {
                 assignments = [ ("x", true); ("y", false) ];
                 formula = mk_cnf [ mk_clause [ mk_literal_pos "z" ] ];
               }
               "{ x = true, y = false } z");
        ] );
      ( "dump_clause",
        [
          test_case "Single literal" `Quick
            (test_dump_clause (mk_clause [ mk_literal_pos "foo" ]) "foo");
          test_case "Multiple literals" `Quick
            (test_dump_clause
               (mk_clause [ mk_literal_pos "foo"; mk_literal_neg "bar" ])
               "(foo || !bar)");
        ] );
      ( "dump_literal",
        [
          test_case "Boolean true" `Quick
            (test_dump_clause (mk_clause [ Bool true ]) "true");
          test_case "Boolean false" `Quick
            (test_dump_clause (mk_clause [ Bool false ]) "false");
        ] );
    ]
