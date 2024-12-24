open Desat.ParserInterface
open Desat.Ast

let expr =
  let pp (ppf : Format.formatter) (e : expr) : unit =
    Fmt.string ppf (dump_expr e)
  in
  let rec equal (a : expr) (b : expr) : bool =
    match (a, b) with
    | Bool n1, Bool n2 -> n1 = n2
    | And (e1a, e2a), And (e1b, e2b) -> equal e1a e1b && equal e2a e2b
    | Or (e1a, e2a), Or (e1b, e2b) -> equal e1a e1b && equal e2a e2b
    | Not e1, Not e2 -> equal e1 e2
    | Var s1, Var s2 -> s1 = s2
    | _, _ -> false
  in
  Alcotest.testable pp equal

let expr_list =
  let pp (ppf : Format.formatter) (es : expr list) : unit =
    Fmt.string ppf (String.concat ", " (List.map dump_expr es))
  in
  let equal (a : expr list) (b : expr list) : bool =
    try
      List.for_all2
        (fun a b ->
          Alcotest.check expr "" a b;
          true)
        a b
    with Invalid_argument _ -> false
  in
  Alcotest.testable pp equal

let test_parse_expr (s : string) (expected : expr) : unit =
  try
    let actual = parse_expr s in
    Alcotest.(check expr) "parse_expr" expected actual
  with e ->
    Printf.printf "Error parsing '%s': %s\n" s (Printexc.to_string e);
    raise e

let test_parse_expr_list (s : string) (expected : expr list) : unit =
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
          test_case "Single bool" `Quick (fun () ->
              test_parse_expr "true" (Bool true));
          test_case "Single variable" `Quick (fun () ->
              test_parse_expr "foo" (Var "foo"));
          test_case "Binary and" `Quick (fun () ->
              test_parse_expr "true && foo" (And (Bool true, Var "foo")));
          test_case "Binary or" `Quick (fun () ->
              test_parse_expr "false || foo" (Or (Bool false, Var "foo")));
          test_case "Not" `Quick (fun () ->
              test_parse_expr "!false" (Not (Bool false)));
          test_case "Parentheses" `Quick (fun () ->
              test_parse_expr "(true && (false || foo))"
                (And (Bool true, Or (Bool false, Var "foo"))));
        ] );
      ( "parse_expr_list",
        [
          test_case "Single bool" `Quick (fun () ->
              test_parse_expr_list "true" [ Bool true ]);
          test_case "Multiple bools" `Quick (fun () ->
              test_parse_expr_list "true, false" [ Bool true; Bool false ]);
          test_case "Mixed bools / variables" `Quick (fun () ->
              test_parse_expr_list "true, false, foo"
                [ Bool true; Bool false; Var "foo" ]);
          test_case "Multiple expressions" `Quick (fun () ->
              test_parse_expr_list "true && false, foo || bar"
                [ And (Bool true, Bool false); Or (Var "foo", Var "bar") ]);
        ] );
    ]
