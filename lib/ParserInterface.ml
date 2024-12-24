open Lexing

let column (pos : position) : int = pos.pos_cnum - pos.pos_bol - 1

let pos_string (pos : position) : string =
  let l = string_of_int pos.pos_lnum and c = string_of_int (column pos + 1) in
  "line " ^ l ^ ", column " ^ c

let parse' f (s : string) =
  let lexbuf = Lexing.from_string s in
  try
    Printf.printf "Parsing: '%s'\n" s;
    f Lexer.token lexbuf
  with
  | Parser.Error ->
      raise (Failure ("Parser error at " ^ pos_string lexbuf.lex_curr_p))
  | Failure msg ->
      raise
        (Failure ("Lexer error: " ^ msg ^ " at " ^ pos_string lexbuf.lex_curr_p))
  | e ->
      raise
        (Failure
           ("Unknown error: " ^ Printexc.to_string e ^ " at "
           ^ pos_string lexbuf.lex_curr_p))

let parse_cnf (s : string) : Ast.cnf = parse' Parser.cnf_expr s

let parse_assignment (s : string) : Ast.assignment =
  parse' Parser.cnf_with_assignments s
