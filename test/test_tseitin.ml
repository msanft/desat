open Desat.Ast
open Desat.Tseitin

let cnf_equal (CNF c1) (CNF c2) =
  let normalize_clause (Clause lits) = Clause (List.sort compare lits) in
  let normalize_cnf (CNF clauses) =
    List.sort compare (List.map normalize_clause clauses)
  in
  normalize_cnf (CNF c1) = normalize_cnf (CNF c2)

let test_to_cnf (expr : boolean_expr) (expected : cnf) () : unit =
  reset_var_counter ();
  let actual = to_cnf expr in
  Alcotest.(check bool)
    (string_of_boolean_expr expr)
    true
    (cnf_equal actual expected)

let () =
  let open Alcotest in
  run "Tseitin Tests"
    [
      ( "to_cnf",
        [
          test_case "Empty" `Quick
            (test_to_cnf (Const true) (CNF [ Clause [ Pos "true" ] ]));
          test_case "Single Var" `Quick
            (test_to_cnf (Var "a") (CNF [ Clause [ Pos "a" ] ]));
          test_case "Negated Var" `Quick
            (test_to_cnf (Not (Var "a"))
               (CNF
                  [
                    Clause [ Pos "t1" ];
                    Clause [ Neg "t1"; Neg "a" ];
                    Clause [ Pos "t1"; Pos "a" ];
                  ]));
          test_case "True Constant" `Quick
            (test_to_cnf (Const true) (CNF [ Clause [ Pos "true" ] ]));
          test_case "False Constant" `Quick
            (test_to_cnf (Const false) (CNF [ Clause [] ]));
          test_case "Simple And" `Quick
            (test_to_cnf
               (And [ Var "a"; Var "b" ])
               (CNF
                  [
                    Clause [ Pos "t1" ];
                    Clause [ Neg "t1"; Pos "a" ];
                    Clause [ Neg "t1"; Pos "b" ];
                    Clause [ Pos "t1"; Neg "a"; Neg "b" ];
                  ]));
          test_case "Simple Or" `Quick
            (test_to_cnf
               (Or [ Var "a"; Var "b" ])
               (CNF
                  [
                    Clause [ Pos "t1" ];
                    Clause [ Neg "t1"; Pos "a"; Pos "b" ];
                    Clause [ Pos "t1"; Neg "a" ];
                    Clause [ Pos "t1"; Neg "b" ];
                  ]));
          test_case "Simple Implication" `Quick
            (test_to_cnf
               (Imp (Var "a", Var "b"))
               (CNF
                  [
                    Clause [ Pos "t1" ];
                    Clause [ Neg "t1"; Neg "a"; Pos "b" ];
                    Clause [ Pos "t1"; Pos "a" ];
                    Clause [ Pos "t1"; Neg "b" ];
                  ]));
          test_case "Simple Equivalence" `Quick
            (test_to_cnf
               (Eq (Var "a", Var "b"))
               (CNF
                  [
                    Clause [ Pos "t1" ];
                    Clause [ Neg "t1"; Neg "a"; Pos "b" ];
                    Clause [ Neg "t1"; Pos "a"; Neg "b" ];
                    Clause [ Pos "t1"; Neg "a"; Neg "b" ];
                    Clause [ Pos "t1"; Pos "a"; Pos "b" ];
                  ]));
          test_case "Complex And-Or" `Quick
            (test_to_cnf
               (And [ Or [ Var "a"; Var "b" ]; Var "c" ])
               (CNF
                  [
                    Clause [ Pos "t2" ];
                    Clause [ Neg "t1"; Pos "a"; Pos "b" ];
                    Clause [ Pos "t1"; Neg "a" ];
                    Clause [ Pos "t1"; Neg "b" ];
                    Clause [ Neg "t2"; Pos "t1" ];
                    Clause [ Neg "t2"; Pos "c" ];
                    Clause [ Pos "t2"; Neg "t1"; Neg "c" ];
                  ]));
          test_case "Nested Not" `Quick
            (test_to_cnf (Not (Not (Var "a")))
               (CNF
                  [
                    Clause [ Pos "t2" ];
                    Clause [ Neg "t1"; Neg "a" ];
                    Clause [ Pos "t1"; Pos "a" ];
                    Clause [ Neg "t2"; Neg "t1" ];
                    Clause [ Pos "t2"; Pos "t1" ];
                  ]));
        ] );
    ]
