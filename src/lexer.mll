
{
  open Lexing
  open Parser
  open Errors
}

let white = [' ' '\t']+
let newline = ('\r' | '\n' | "\r\n")+
let comment = '#'

let op = ['!' '@' '$' '%' '^' '&' '*' '-' '+' '>' '<' '=' ':' '|' '?' '.']+

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']

let id = ['a'-'z' '_'] (letter | digit)*
let tid = ['A'-'Z'] (letter | digit)*

let int_ = digit+
let bool_ = "true" | "false"
let float_ = digit+ '.' digit*
let string_ = "\"" _* "\""

rule token = parse
  | white { token lexbuf }
  | newline { new_line lexbuf; token lexbuf }
  | comment { parse_comment lexbuf }

  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "%" { REM }

  | "<"  { LT }
  | "<=" { LE }
  | ">=" { GE }
  | ">"  { GT }

  | "==" { EQ }
  | "!=" { NE }

  | "::" { CONV }

  | "("  { LC }
  | ","  { COMMA }
  | "->" { ARROW }
  | ")"  { RC }
  
  | ":"  { COLON }
  | "="  { ASSIGN }
  
  | "fun" { FUN }
  | "val" { VAL }

  | "let" { LET }
  | "in"  { IN }

  | "case" { CASE }

  | "if"   { IF }
  | "then" { THEN }
  | "else" { ELSE }

  | "type" { TYPE }
  | "|"    { BAR }

  | op  { OP (lexeme lexbuf) }

  | int_    { INT (int_of_string (lexeme lexbuf)) }
  | bool_   { BOOL (bool_of_string (lexeme lexbuf)) }
  | float_  { FLOAT (float_of_string (lexeme lexbuf)) }
  | string_ { STRING (lexeme lexbuf) }
  
  | id  { ID (lexeme lexbuf) }
  | tid { TID (lexeme lexbuf) }

  | _   { raise (LexingError (lexeme lexbuf)) }
  | eof { EOF }

and parse_comment = parse
  | newline { new_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { parse_comment lexbuf }