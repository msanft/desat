open Desat.Dpll
open Desat.ParserInterface

let print_result = function
  | None -> Printf.printf "UNSAT\n"
  | Some assignment ->
      Printf.printf "SAT\n";
      Printf.printf "Assignments:\n";
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
        let expr = parse_cnf Sys.argv.(1) in
        solve expr |> print_result
      with Failure msg ->
        Printf.eprintf "Error: %s\n" msg;
        exit 1)
  | _ ->
      Printf.eprintf "Too many arguments\n";
      exit 1
