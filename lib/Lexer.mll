{
    open Parser
}

let true = "true"
let false = "false"
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let comma = ','
let left_paren = '('
let right_paren = ')'
let and_sign = "&&" | "and"
let or_sign = "||" | "or"
let not_sign = '!' | "not"

let bool_constant = true | false

let identifier = alpha (alpha | digit)*

let whitespace = [' ' '\t' '\n' '\r']+

rule token = parse
  | bool_constant { BOOL (bool_of_string (Lexing.lexeme lexbuf)) }
  | and_sign { AND }
  | or_sign { OR }
  | not_sign { NOT }
  | left_paren { LPAREN }
  | right_paren { RPAREN }
  | comma { COMMA }
  | identifier { VARIABLE (Lexing.lexeme lexbuf) }
  | whitespace { token lexbuf }
  | eof { EOF}
  | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }
