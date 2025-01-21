open Desat.Dpll
open Desat.ParserInterface
open Desat.Format
open Desat.Tseitin
open Desat.Ast

let print_result = function
  | None -> print_string (bold "UNSAT\n")
  | Some assignment ->
      print_string (bold "SAT\n");
      print_string (bold "Assignments:\n");
      List.iter
        (fun (var, value) -> Printf.printf "  %s = %b\n" var value)
        assignment

let () =
  match Array.length Sys.argv with
  | 0 | 1 ->
      Printf.eprintf "Usage: %s \"(x || !y)\"\n"
        (if Array.length Sys.argv > 0 then Sys.argv.(0) else "desat");
      exit 1
  | 2 -> (
      try
        match parse_cnf Sys.argv.(1) with
        | cnf -> solve cnf |> print_result
        | exception Failure _ ->
            print_string
              (bold
                 "Parsing as CNF failed, trying to perform Tseitin's \
                  transformation\n");
            let expr = parse_boolean_expr Sys.argv.(1) in
            let cnf = to_cnf expr in
            print_string (bold "Equi-satisfiable CNF:\n");
            print_string (string_of_cnf cnf ^ "\n");
            solve cnf |> print_result
      with Failure msg ->
        Printf.eprintf "Error: %s\n" msg;
        exit 1)
  | _ ->
      Printf.eprintf "Too many arguments\n";
      exit 1
