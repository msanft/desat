{
    open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let plus_sign = '+'
let minus_sign = '-'
let mul_sign = '*'
let div_sign = '/'
let left_paren = '('
let right_paren = ')'
let and_sign = "&&" | "and"
let or_sign = "||" | "or"
let not_sign = '!' | "not"

let int_constant = digit+

let identifier = alpha (alpha | digit)*

let whitespace = [' ' '\t' '\n' '\r']+

rule token = parse
  | int_constant { INT_CONSTANT (int_of_string (Lexing.lexeme lexbuf)) }
  | plus_sign { PLUS }
  | minus_sign { MINUS }
  | mul_sign { MUL }
  | div_sign { DIV }
  | and_sign { AND }
  | or_sign { OR }
  | not_sign { NOT }
  | left_paren { LPAREN }
  | right_paren { RPAREN }
  | identifier { VARIABLE (Lexing.lexeme lexbuf) }
  | whitespace { token lexbuf }
  | eof { EOF}
  | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }
