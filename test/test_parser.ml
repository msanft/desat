open Desat.ParserInterface
open Desat.Ast

let literal =
  let pp ppf = function
    | Pos v -> Fmt.string ppf v
    | Neg v -> Fmt.string ppf ("!" ^ v)
    | Bool b -> Fmt.string ppf (string_of_bool b)
  in
  let equal a b =
    match (a, b) with
    | Pos v1, Pos v2 -> v1 = v2
    | Neg v1, Neg v2 -> v1 = v2
    | Bool b1, Bool b2 -> b1 = b2
    | _, _ -> false
  in
  Alcotest.testable pp equal

let clause =
  let pp ppf (Clause lits) = Fmt.string ppf (string_of_clause (Clause lits)) in
  let equal (Clause lits1) (Clause lits2) =
    try
      List.for_all2
        (fun a b ->
          Alcotest.check literal "" a b;
          true)
        lits1 lits2
    with Invalid_argument _ -> false
  in
  Alcotest.testable pp equal

let cnf =
  let pp ppf (CNF clauses) = Fmt.string ppf (string_of_cnf (CNF clauses)) in
  let equal (CNF clauses1) (CNF clauses2) =
    try
      List.for_all2
        (fun a b ->
          Alcotest.check clause "" a b;
          true)
        clauses1 clauses2
    with Invalid_argument _ -> false
  in
  Alcotest.testable pp equal

let test_parse_cnf (s : string) (expected : cnf) : unit =
  try
    let actual = parse_cnf s in
    Alcotest.(check cnf) "parse_cnf" expected actual
  with e ->
    Printf.printf "Error parsing '%s': %s\n" s (Printexc.to_string e);
    raise e

let contains_substring ~substring str =
  try
    let _ = Str.search_forward (Str.regexp_string substring) str 0 in
    true
  with Not_found -> false

let test_parse_invalid_cnf (s : string) (expected_error : string) : unit =
  try
    let _ = parse_cnf s in
    Alcotest.fail ("Expected parse error for invalid CNF: " ^ s)
  with
  | Failure msg ->
      if not (contains_substring ~substring:expected_error msg) then
        Alcotest.failf "Expected error containing '%s' but got: %s"
          expected_error msg
  | e -> Alcotest.failf "Unexpected error: %s" (Printexc.to_string e)

let () =
  let open Alcotest in
  run "Parser Tests"
    [
      ( "parse_cnf",
        [
          test_case "Single bool" `Quick (fun () ->
              test_parse_cnf "true" (CNF [ Clause [ Bool true ] ]));
          test_case "Single variable" `Quick (fun () ->
              test_parse_cnf "foo" (CNF [ Clause [ Pos "foo" ] ]));
          test_case "Single clause with AND" `Quick (fun () ->
              test_parse_cnf "true && foo"
                (CNF [ Clause [ Bool true ]; Clause [ Pos "foo" ] ]));
          test_case "Single OR clause" `Quick (fun () ->
              test_parse_cnf "(false || foo)"
                (CNF [ Clause [ Bool false; Pos "foo" ] ]));
          test_case "Negated variable" `Quick (fun () ->
              test_parse_cnf "!foo" (CNF [ Clause [ Neg "foo" ] ]));
          test_case "Multiple clauses" `Quick (fun () ->
              test_parse_cnf "(true || foo) && (!bar || baz)"
                (CNF
                   [
                     Clause [ Bool true; Pos "foo" ];
                     Clause [ Neg "bar"; Pos "baz" ];
                   ]));
        ] );
      ( "parse_assignment",
        [
          test_case "Single assignment" `Quick (fun () ->
              let actual = parse_assignment "{ x = true } foo" in
              Alcotest.(check (list (pair string bool)))
                "assignments"
                [ ("x", true) ]
                actual.assignments;
              Alcotest.(check cnf)
                "formula" (CNF [ Clause [ Pos "foo" ] ]) actual.formula);
          test_case "Multiple assignments" `Quick (fun () ->
              let actual =
                parse_assignment "{ a = false, b = true } (a || b)"
              in
              Alcotest.(check (list (pair string bool)))
                "assignments"
                [ ("a", false); ("b", true) ]
                actual.assignments;
              Alcotest.(check cnf)
                "formula"
                (CNF [ Clause [ Pos "a"; Pos "b" ] ])
                actual.formula);
        ] );
      ( "reject_invalid_cnf",
        [
          test_case "Nested OR" `Quick (fun () ->
              test_parse_invalid_cnf "a || (b || c)" "Parser error");
          test_case "Top level OR" `Quick (fun () ->
              test_parse_invalid_cnf "a || b" "Parser error");
          test_case "Complex negation" `Quick (fun () ->
              test_parse_invalid_cnf "!(a || b)" "Parser error");
          test_case "Negated AND" `Quick (fun () ->
              test_parse_invalid_cnf "!(a && b)" "Parser error");
          test_case "Nested AND under OR" `Quick (fun () ->
              test_parse_invalid_cnf "(a || (b && c))" "Parser error");
          test_case "Double negation" `Quick (fun () ->
              test_parse_invalid_cnf "!!a" "Parser error");
          test_case "OR at root with AND" `Quick (fun () ->
              test_parse_invalid_cnf "a || b && c" "Parser error");
          test_case "Empty parentheses" `Quick (fun () ->
              test_parse_invalid_cnf "()" "Parser error");
          test_case "Unmatched parentheses" `Quick (fun () ->
              test_parse_invalid_cnf "(a || b" "Parser error");
        ] );
    ]
