
{
  open Lexing
  open Errors
  open Parser
  open Buffer
}

let white = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let comment = '#'

let op = ['!' '@' '$' '%' '^' '&' '*' '-' '+' '>' '<' '=' ':' '|' '?' '.']+

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']

let id = ['a'-'z' '_'] (letter | digit)*
let tid = ['A'-'Z'] (letter | digit)*

let int_ = digit+
let bool_ = "true" | "false"
let float_ = digit* ((digit '.') | ('.' digit)) digit* 
let float_ = (digit+ '.' digit*) | (digit* '.' digit+)
let string_ = "\"" _* "\""

rule token = parse
  | white { token lexbuf }
  | newline { new_line lexbuf; token lexbuf }
  | comment { parse_comment lexbuf }

  | '"' { parse_string (create 15) lexbuf}

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
  | ";;" { CHAIN }

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

  | int_    { INT (int_of_string (lexeme lexbuf)) }
  | bool_   { BOOL (bool_of_string (lexeme lexbuf)) }
  | float_  { FLOAT (float_of_string (lexeme lexbuf)) }
  
  | op  { OP (lexeme lexbuf) }
  
  | id  { ID (lexeme lexbuf) }
  | tid { TID (lexeme lexbuf) }

  | _   { 
    let pos = Misc.get_lexing_position lexbuf in
    let msg = Misc.parse_pos_error pos ("unexpected token \""^(lexeme lexbuf)^"\"") in
    raise (LexingError msg)
  }
  | eof { EOF }

and parse_string buf = parse
  | '"'       { STRING (contents buf) }
  | '\\' '/'  { add_char buf '/';     parse_string buf lexbuf }
  | '\\' '\\' { add_char buf '\\';    parse_string buf lexbuf }
  | '\\' 'b'  { add_char buf '\b';    parse_string buf lexbuf }
  | '\\' 'f'  { add_char buf '\012';  parse_string buf lexbuf }
  | '\\' 'n'  { add_char buf '\n';    parse_string buf lexbuf }
  | '\\' 'r'  { add_char buf '\r';    parse_string buf lexbuf }
  | '\\' 't'  { add_char buf '\t';    parse_string buf lexbuf }
  | [^ '"' '\\']+ { add_string buf (lexeme lexbuf); parse_string buf lexbuf }
  | _ { raise (LexingError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (LexingError ("String is not terminated")) }

and parse_comment = parse
  | newline { new_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { parse_comment lexbuf }
