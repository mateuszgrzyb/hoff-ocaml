{
  open Lexing 
  open Buffer
  open Parser
  open Errors
}

let digit = ['0'-'9']
let newline = '\r' | '\n' | "\r\n"

let int = digit+
let float = (digit+ '.' digit*) | (digit* '.' digit+)
let bool = "True" | "False"

let id = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse 
  | [' ' '\t']+ { token lexbuf }
  | newline { new_line lexbuf; token lexbuf }
  | '#' { token_comment lexbuf }

  | '"' { token_string (create 17) lexbuf }

  | "const" { CONST }
  | "fun" { FUN }
  | "type" { TYPE }
  
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fi" { FI }

  | "let" { LET }
  | "in" { IN }
  | "tel" { TEL }

  | "case" { CASE }
  | "of" { OF }
  
  | "->" { ARROW }
  | "=>" { FATARROW }
  | ".>" { RCHAIN }

  | '|' { BAR }


  | '+' { ADD }
  | '-' { SUB }
  | '*' { MUL }
  | '/' { DIV }
  | '%' { REM }

  | "<=" { LE }
  | "==" { EQ }
  | "!=" { NE }
  | ">=" { GE }
 
  | "&&" { AND }
  | "||" { OR }
  
  | '<' { LT }
  | '>' { GT }


  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | ',' { COMMA }
  | ':' { COLON }
  | '=' { ASSIGN }
  | '.' { LCHAIN }

  | int { INT (int_of_string (lexeme lexbuf)) }
  | float { FLOAT (float_of_string (lexeme lexbuf)) }
  | bool { BOOL (bool_of_string (String.lowercase_ascii (lexeme lexbuf))) }
  | id { ID (lexeme lexbuf) }

  | _ { raise (LexingError ("Unexpected char: " ^ lexeme lexbuf)) }
  | eof { EOF }


and token_string buf = parse
  | '"' { STRING (contents buf) }
  | '\\' '/'  { add_char buf '/';    token_string buf lexbuf }
  | '\\' '\\' { add_char buf '\\';   token_string buf lexbuf }
  | '\\' 'b'  { add_char buf '\b';   token_string buf lexbuf }
  | '\\' 'f'  { add_char buf '\012'; token_string buf lexbuf }
  | '\\' 'n'  { add_char buf '\n';   token_string buf lexbuf }
  | '\\' 'r'  { add_char buf '\r';   token_string buf lexbuf }
  | '\\' 't'  { add_char buf '\t';   token_string buf lexbuf }
  | [^ '"' '\\']+ { add_string buf (lexeme lexbuf); token_string buf lexbuf }
  | _ { raise (LexingError ("Illegal string character: " ^ lexeme lexbuf)) }
  | eof { raise (LexingError ("String is not terminated")) }

and token_comment = parse
  | newline { new_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { token_comment lexbuf }