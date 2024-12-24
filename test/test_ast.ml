open Desat.Ast

let test_dump_expr e expected () =
  let actual = dump_expr e in
  Alcotest.(check string) "dump_expr" expected actual

let test_dump_expr_list es expected () =
  let actual = dump_expr_list es in
  Alcotest.(check string) "dump_expr_list" expected actual

let () =
  let open Alcotest in
  run "AST Tests"
    [
      ( "dump_expr",
        [
          test_case "Single bool" `Quick (test_dump_expr (Bool true) "true");
          test_case "Binary and" `Quick
            (test_dump_expr (And (Bool true, Var "foo")) "(true && foo)");
          test_case "Binary or" `Quick
            (test_dump_expr (Or (Bool false, Var "foo")) "(false || foo)");
          test_case "Not" `Quick (test_dump_expr (Not (Bool false)) "!false");
        ] );
      ( "dump_expr_list",
        [
          test_case "Single bool" `Quick
            (test_dump_expr_list [ Bool true ] "true");
          test_case "Multiple bools" `Quick
            (test_dump_expr_list [ Bool true; Bool false ] "true, false");
          test_case "Multiple expressions" `Quick
            (test_dump_expr_list
               [ And (Bool true, Bool false); Or (Var "foo", Var "bar") ]
               "(true && false), (foo || bar)");
        ] );
    ]
