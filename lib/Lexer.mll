{
  open Parser
}

let whitespace = [' ' '\t' '\n' '\r']+
let var = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | whitespace { token lexbuf }
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | "&&"      { AND }
  | "||"      { OR }
  | "->"      { IMP }
  | "<->"     { EQ }
  | '!'       { NOT }
  | "true"    { TRUE }
  | "false"   { FALSE }
  | var       { VAR (Lexing.lexeme lexbuf) }
  | eof       { EOF }
