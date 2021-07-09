type token =
  | INT of (int)
  | FLOAT of (float)
  | BOOL of (bool)
  | STRING of (string)
  | ID of (string)
  | ADD
  | SUB
  | MUL
  | DIV
  | REM
  | LT
  | LE
  | EQ
  | NE
  | GE
  | GT
  | AND
  | OR
  | NOT
  | COLON
  | ASSIGN
  | LPAREN
  | RPAREN
  | COMMA
  | ARROW
  | FATARROW
  | LBRACK
  | RBRACK
  | LCHAIN
  | RCHAIN
  | CONST
  | FUN
  | TYPE
  | IF
  | THEN
  | ELSE
  | FI
  | LET
  | IN
  | TEL
  | CASE
  | BAR
  | OF
  | EOF

val module_ :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.module_t
