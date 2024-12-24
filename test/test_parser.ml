open Desat.ParserInterface
open Desat.Ast

let expr =
  let pp ppf e = Fmt.string ppf (dump_expr e) in
  let rec equal a b =
    match (a, b) with
    | Int n1, Int n2 -> n1 = n2
    | Add (e1a, e2a), Add (e1b, e2b) -> equal e1a e1b && equal e2a e2b
    | Sub (e1a, e2a), Sub (e1b, e2b) -> equal e1a e1b && equal e2a e2b
    | Mul (e1a, e2a), Mul (e1b, e2b) -> equal e1a e1b && equal e2a e2b
    | Div (e1a, e2a), Div (e1b, e2b) -> equal e1a e1b && equal e2a e2b
    | And (e1a, e2a), And (e1b, e2b) -> equal e1a e1b && equal e2a e2b
    | Or (e1a, e2a), Or (e1b, e2b) -> equal e1a e1b && equal e2a e2b
    | Not e1, Not e2 -> equal e1 e2
    | Var s1, Var s2 -> s1 = s2
    | _, _ -> false
  in
  Alcotest.testable pp equal

let expr_list =
  let pp ppf es = Fmt.string ppf (String.concat ", " (List.map dump_expr es)) in
  let equal a b =
    try
      List.for_all2
        (fun a b ->
          Alcotest.check expr "" a b;
          true)
        a b
    with Invalid_argument _ -> false
  in
  Alcotest.testable pp equal

let test_parse_expr s expected =
  try
    let actual = parse_expr s in
    Alcotest.(check expr) "parse_expr" expected actual
  with e ->
    Printf.printf "Error parsing '%s': %s\n" s (Printexc.to_string e);
    raise e

let test_parse_expr_list s expected =
  try
    let actual = parse_expr_list s in
    Alcotest.(check expr_list) "parse_expr_list" expected actual
  with e ->
    Printf.printf "Error parsing '%s': %s\n" s (Printexc.to_string e);
    raise e

let () =
  let open Alcotest in
  run "Parser Tests"
    [
      ( "parse_expr",
        [
          test_case "Single integer" `Quick (fun () ->
              test_parse_expr "42" (Int 42));
          test_case "Single digit" `Quick (fun () ->
              test_parse_expr "5" (Int 5));
          test_case "Integer with whitespace" `Quick (fun () ->
              test_parse_expr " 42 " (Int 42));
          test_case "Binary add" `Quick (fun () ->
              test_parse_expr "13 + 37" (Add (Int 13, Int 37)));
          test_case "Variable add" `Quick (fun () ->
              test_parse_expr "13 + 37 + foo"
                (Add (Add (Int 13, Int 37), Var "foo")));
          test_case "Binary minus" `Quick (fun () ->
              test_parse_expr "13 - foo" (Sub (Int 13, Var "foo")));
          test_case "Binary mul" `Quick (fun () ->
              test_parse_expr "13 * foo" (Mul (Int 13, Var "foo")));
          test_case "Binary div" `Quick (fun () ->
              test_parse_expr "13 / foo" (Div (Int 13, Var "foo")));
          test_case "Simple parentheses" `Quick (fun () ->
              test_parse_expr "(42)" (Int 42));
          test_case "Parentheses with addition" `Quick (fun () ->
              test_parse_expr "(13 + 37)" (Add (Int 13, Int 37)));
          test_case "Nested parentheses" `Quick (fun () ->
              test_parse_expr "((13 + 37))" (Add (Int 13, Int 37)));
          test_case "Operator precedence with parentheses" `Quick (fun () ->
              test_parse_expr "(2 + 3) * 4" (Mul (Add (Int 2, Int 3), Int 4)));
          test_case "Operator precedence without parentheses" `Quick (fun () ->
              test_parse_expr "2 + 3 * 4" (Add (Int 2, Mul (Int 3, Int 4))));
          test_case "Complex nested parentheses" `Quick (fun () ->
              test_parse_expr "(2 * (3 + 4)) / 5"
                (Div (Mul (Int 2, Add (Int 3, Int 4)), Int 5)));
          test_case "Parentheses with variables" `Quick (fun () ->
              test_parse_expr "(foo + 3) * bar"
                (Mul (Add (Var "foo", Int 3), Var "bar")));
          test_case "Binary and" `Quick (fun () ->
              test_parse_expr "13 && foo" (And (Int 13, Var "foo")));
          test_case "Binary or" `Quick (fun () ->
              test_parse_expr "13 || foo" (Or (Int 13, Var "foo")));
          test_case "Not" `Quick (fun () ->
              test_parse_expr "!13" (Not (Int 13)));
        ] );
      ( "parse_expr_list",
        [
          test_case "Single expression" `Quick (fun () ->
              test_parse_expr_list "42" [ Int 42 ]);
          test_case "Two simple expressions" `Quick (fun () ->
              test_parse_expr_list "42, 43" [ Int 42; Int 43 ]);
          test_case "Multiple expressions" `Quick (fun () ->
              test_parse_expr_list "1 + 2, 3 * 4, 5"
                [ Add (Int 1, Int 2); Mul (Int 3, Int 4); Int 5 ]);
          test_case "Expressions with variables" `Quick (fun () ->
              test_parse_expr_list "x, y + z"
                [ Var "x"; Add (Var "y", Var "z") ]);
          test_case "Expressions with parentheses" `Quick (fun () ->
              test_parse_expr_list "(1 + 2), (3 * 4)"
                [ Add (Int 1, Int 2); Mul (Int 3, Int 4) ]);
          test_case "Complex mixed expressions" `Quick (fun () ->
              test_parse_expr_list "x && y, (a + b) * c, !d || e"
                [
                  And (Var "x", Var "y");
                  Mul (Add (Var "a", Var "b"), Var "c");
                  Or (Not (Var "d"), Var "e");
                ]);
          test_case "Whitespace around commas" `Quick (fun () ->
              test_parse_expr_list "1 , 2 , 3" [ Int 1; Int 2; Int 3 ]);
          test_case "Empty list" `Quick (fun () ->
              test_parse_expr_list "42" [ Int 42 ]);
        ] );
    ]
