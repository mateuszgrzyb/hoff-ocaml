type token =
  | VAL
  | TYPE
  | TID of string
  | THEN
  | SUB
  | STRING of string
  | REM
  | RC
  | OR
  | OP of string
  | NOT
  | NE
  | MUL
  | LT
  | LET
  | LE
  | LC
  | INT of int
  | IN
  | IF
  | ID of string
  | GT
  | GE
  | FUN
  | FLOAT of float
  | EQ
  | EOF
  | ELSE
  | DIV
  | CONV
  | COMMA
  | COLON
  | CASE
  | BOOL of bool
  | BAR
  | ASSIGN
  | ARROW
  | AND
  | ADD
  | BEGIN
  | END
  | LB
  | RB
[@@deriving show]
