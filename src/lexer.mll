
{
  open Lexing
  open Parser
  open Errors


  let extract_op (op_regex: string): (string * int) =
    let op_regex_2 = String.sub op_regex 1 ((String.length op_regex) - 1) in
    let op_regex_list = String.split_on_char ')' op_regex_2 in
    match op_regex_list with
      | op :: prec :: [] -> (op, int_of_string prec)
      | _ -> failwith "ExtractOpError"
}

let white = [' ' '\t']+
let newline = ('\r' | '\n' | "\r\n")+
let comment = '#'

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']

let op_id = ['!' '@' '$' '%' '^' '&' '*' '-' '+']+
let op = '(' op_id ')' digit

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
(*
  | "+" { printf "ADD\n" }
  | "-" { printf "SUB\n" }
  | "*" { printf "MUL\n" }
  | "/" { printf "DIV\n" }

  | id      { printf "ID: %s\n" (lexeme lexbuf) }
  | op      { let (op, prec) = extract_op (lexeme lexbuf) in printf "OP: %s\nPREC: %d\n" op prec }
  
  | int_    { printf "INT: %s\n" (lexeme lexbuf) }
  | float_  { printf "FLOAT: %s\n" (lexeme lexbuf) }

  | _   { printf "Unknown character: %s\n" (lexeme lexbuf) }
  | eof { raise Exit }
*)