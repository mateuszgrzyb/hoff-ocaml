type token =
  | INT of (int)
  | FLOAT of (float)
  | ID of (string)
  | BOOL of (bool)
  | ADD
  | SUB
  | MUL
  | DIV
  | LET
  | COLON
  | ASSIGN
  | LPAREN
  | RPAREN
  | COMMA
  | ARROW
  | EOF

val module_ :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.module_t
