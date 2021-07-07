{
  open Lexing 
  open Parser
  open Errors
}

let digit = ['0'-'9']

let int = digit+
let float = (digit+ '.' digit*) | (digit* '.' digit+)
let bool = "True" | "False"

let id = ['_' 'a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z']*

rule token = parse 
  | [' ' '\t']+ { token lexbuf }
  | '\r' | '\n' | "\r\n" { new_line lexbuf; token lexbuf }



  | "let" { LET }
  | "->" { ARROW }

  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }


  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | ":" { COLON }
  | "=" { ASSIGN }

  | int { INT (int_of_string (lexeme lexbuf)) }
  | float { FLOAT (float_of_string (lexeme lexbuf)) }
  | bool { BOOL (bool_of_string (String.lowercase_ascii (lexeme lexbuf))) }
  | id { ID (lexeme lexbuf) }

  | _ { raise (LexingError ("Unexpected char: " ^ lexeme lexbuf)) }
  | eof { EOF }

