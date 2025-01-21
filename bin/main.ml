open Desat.Solver
open Desat.ParserInterface
open Desat.Format
open Desat.Tseitin
open Desat.Ast

let print_result (a : assignment option) : unit =
  match a with
  | None -> print_string (bold "UNSAT\n")
  | Some assignments ->
      print_string (bold "SAT\n");
      print_string (bold "Assignments:\n");
      List.iter
        (fun (var, value) -> Printf.printf "  %s = %b\n" var value)
        assignments

let () =
  match Array.length Sys.argv with
  | 0 | 1 | 2 ->
      Printf.eprintf "Usage: %s <CDCL/DPLL> \"(x || !y)\"\n"
        (if Array.length Sys.argv > 0 then Sys.argv.(0) else "desat");
      exit 1
  | 3 -> (
      let raw_approach = Sys.argv.(1) in
      let raw_formula = Sys.argv.(2) in
      let approach =
        match raw_approach with
        | "CDCL" -> Cdcl
        | "DPLL" -> Dpll
        | _ ->
            Printf.eprintf "Invalid approach: %s\n" raw_approach;
            exit 1
      in
      try
        match parse_cnf raw_formula with
        | cnf -> solve approach cnf |> print_result
        | exception Failure _ ->
            print_string
              (bold
                 "Parsing as CNF failed, trying to perform Tseitin's \
                  transformation\n");
            let expr = parse_boolean_expr raw_formula in
            let cnf = to_cnf expr in
            print_string (bold "Equi-satisfiable CNF:\n");
            print_string (string_of_cnf cnf ^ "\n");
            solve approach cnf |> print_result
      with Failure msg ->
        Printf.eprintf "Error: %s\n" msg;
        exit 1)
  | _ ->
      Printf.eprintf "Too many arguments\n";
      exit 1
