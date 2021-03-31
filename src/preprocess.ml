

(*
open Parser

let op_table = Hashtbl.create 10

let extract_op (op_regex: string): (string * int) =
  let op_regex_2 = String.sub op_regex 1 ((String.length op_regex) - 1) in
  let op_regex_list = String.split_on_char ')' op_regex_2 in
  match op_regex_list with
    | op :: prec :: [] -> (op, int_of_string prec)
    | _ -> failwith "ExtractOpError"

let match_prec (op: string * int): (string * token) =
  match op with
  | (op, 1) -> (op, OP1 op)
  | (op, 2) -> (op, OP2 op)
  | (op, 3) -> (op, OP3 op)
  | (op, 4) -> (op, OP4 op)
  | (op, 5) -> (op, OP5 op)
  | (op, 6) -> (op, OP6 op)
  | (op, 7) -> (op, OP7 op)
  | (op, 8) -> (op, OP8 op)
  | (op, 9) -> (op, OP9 op)
  | _ -> failwith "MatchPrecError"

let op = [%sedlex.regexp? Star ('!' | '@' | '$' | '%' | '^' | '&' | '*' | '-' | '+')]
let op_definition = [%sedlex.regexp? '(', op, ')', white_space, '[', xml_digit, ']']
  (*
let op = ['!' '@' '$' '%' '^' '&' '*' '-' '+']+
let op_definition = '(' op ')' white* '[' digit ']'
  *)

let rec preprocess buf = 
  match%sedlex buf with
  | op_definition -> 
    let (op, tkn) = (match_prec (extract_op (Sedlexing.Latin1.lexeme buf))) in
    Hashtbl.add op_table op tkn;
    preprocess buf
  | any -> preprocess buf
  | eof -> ()
  | _ -> failwith "PREPROCESS ERROR"

let main (inx: in_channel) =
  let buf = Sedlexing.Latin1.from_channel inx in
  ignore (preprocess buf);
  Hashtbl.iter (fun a b -> match (a, b) with
  | (op, OP1 _) -> Printf.printf "OP1: %s\n" op
  | (op, OP2 _) -> Printf.printf "OP2: %s\n" op
  | (op, OP3 _) -> Printf.printf "OP3: %s\n" op
  | (op, OP4 _) -> Printf.printf "OP4: %s\n" op
  | (op, OP5 _) -> Printf.printf "OP5: %s\n" op
  | (op, OP6 _) -> Printf.printf "OP6: %s\n" op
  | (op, OP7 _) -> Printf.printf "OP7: %s\n" op
  | (op, OP8 _) -> Printf.printf "OP8: %s\n" op
  | (op, OP9 _) -> Printf.printf "OP9: %s\n" op
  | _           -> ()) op_table;

*)