{
    open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let plus_sign = '+'

let int_constant = digit+

let identifier = alpha (alpha | digit)*

let whitespace = [' ' '\t' '\n' '\r']+

rule token = parse
  | int_constant { INT_CONSTANT (int_of_string (Lexing.lexeme lexbuf)) }
  | plus_sign { PLUS }
  | identifier { VARIABLE (Lexing.lexeme lexbuf) }
  | whitespace { token lexbuf }
  | eof { EOF}
  | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }
