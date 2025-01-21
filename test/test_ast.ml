open Desat.Ast

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

let test_dump_boolean_expr (expr : boolean_expr) (expected : string) () : unit =
  let actual = string_of_boolean_expr expr in
  Alcotest.(check string) "dump_boolean_expr" expected actual

let () =
  let open Alcotest in
  run "AST Tests"
    [
      ( "dump_cnf",
        [
          test_case "Single literal clause" `Quick
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
        ] );
      ( "dump_assignment",
        [
          test_case "Single assignment" `Quick
            (test_dump_assignment [ ("foo", true) ] "foo = true");
          test_case "Multiple assignments" `Quick
            (test_dump_assignment
               [ ("foo", true); ("bar", false) ]
               "foo = true, bar = false");
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
          test_case "Single variable" `Quick
            (test_dump_clause (mk_clause [ mk_literal_pos "foo" ]) "foo");
          test_case "Boolean true" `Quick
            (test_dump_clause (mk_clause [ Bool true ]) "true");
          test_case "Boolean false" `Quick
            (test_dump_clause (mk_clause [ Bool false ]) "false");
        ] );
      ( "dump_boolean_expr",
        [
          test_case "Single variable" `Quick
            (test_dump_boolean_expr (Var "foo") "foo");
          test_case "Negated variable" `Quick
            (test_dump_boolean_expr (Not (Var "foo")) "!foo");
          test_case "And" `Quick
            (test_dump_boolean_expr
               (And [ Var "foo"; Var "bar" ])
               "(foo && bar)");
          test_case "Or" `Quick
            (test_dump_boolean_expr
               (Or [ Var "foo"; Var "bar" ])
               "(foo || bar)");
          test_case "Const true" `Quick
            (test_dump_boolean_expr (Const true) "true");
          test_case "Const false" `Quick
            (test_dump_boolean_expr (Const false) "false");
          test_case "Implication" `Quick
            (test_dump_boolean_expr (Imp (Var "foo", Var "bar")) "(foo -> bar)");
          test_case "Equivalence" `Quick
            (test_dump_boolean_expr (Eq (Var "foo", Var "bar")) "(foo <-> bar)");
        ] );
    ]
