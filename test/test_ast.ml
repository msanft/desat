open Desat.Ast

let test_dump_expr (e : expr) (expected : string) () =
  let actual = dump_expr e in
  Alcotest.(check string) "dump_expr" expected actual

let test_dump_expr_list (es : expr list) (expected : string) () =
  let actual = dump_expr_list es in
  Alcotest.(check string) "dump_expr_list" expected actual

let () =
  let open Alcotest in
  run "AST Tests"
    [
      ( "dump_expr",
        [
          test_case "Single integer" `Quick (test_dump_expr (Int 42) "42");
          test_case "Binary add" `Quick
            (test_dump_expr (Add (Int 13, Int 37)) "(13 + 37)");
          test_case "Variable add" `Quick
            (test_dump_expr
               (Add (Add (Int 13, Int 37), Var "foo"))
               "((13 + 37) + foo)");
          test_case "Binary minus" `Quick
            (test_dump_expr (Sub (Int 13, Var "foo")) "(13 - foo)");
          test_case "Binary mul" `Quick
            (test_dump_expr (Mul (Int 13, Var "foo")) "(13 * foo)");
          test_case "Binary div" `Quick
            (test_dump_expr (Div (Int 13, Var "foo")) "(13 / foo)");
          test_case "Binary and" `Quick
            (test_dump_expr (And (Int 13, Var "foo")) "(13 && foo)");
          test_case "Binary or" `Quick
            (test_dump_expr (Or (Int 13, Var "foo")) "(13 || foo)");
          test_case "Not" `Quick (test_dump_expr (Not (Int 13)) "!13");
        ] );
      ( "dump_expr_list",
        [
          test_case "Single integer" `Quick
            (test_dump_expr_list [ Int 42 ] "42");
          test_case "Multiple integers" `Quick
            (test_dump_expr_list [ Int 13; Int 37 ] "13, 37");
          test_case "Multiple expressions" `Quick
            (test_dump_expr_list
               [ Add (Int 13, Int 37); Mul (Int 42, Int 5) ]
               "(13 + 37), (42 * 5)");
        ] );
    ]
