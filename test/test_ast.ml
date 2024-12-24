open Desat.Ast

let test_dump_expr (e : expr) (expected : string) () =
  let actual = dump_expr e in
  Alcotest.(check string) "dump_expr" expected actual

let () =
  let open Alcotest in
  run "AST Tests"
    [
      ( "dump_expr",
        [
          test_case "Single integer" `Quick (test_dump_expr (Int 42) "Int 42");
          test_case "Binary add" `Quick
            (test_dump_expr (Add (Int 13, Int 37)) "Int 13 + Int 37");
          test_case "Variable add" `Quick
            (test_dump_expr
               (Add (Add (Int 13, Int 37), Var "foo"))
               "Int 13 + Int 37 + Var foo");
          test_case "Binary minus" `Quick
            (test_dump_expr (Sub (Int 13, Var "foo")) "Int 13 - Var foo");
          test_case "Binary mul" `Quick
            (test_dump_expr (Mul (Int 13, Var "foo")) "Int 13 * Var foo");
          test_case "Binary div" `Quick
            (test_dump_expr (Div (Int 13, Var "foo")) "Int 13 / Var foo");
        ] );
    ]
