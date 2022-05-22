module I = Parser.MenhirInterpreter

exception SyntaxError of ((int * int) option * string)

let get_parse_error env =
  match I.stack env with
  | (lazy Nil) -> "Invalid syntax, but I don't know why..."
  | (lazy (Cons (I.Element (state, _, _, _), _))) ->
    (try Parser_messages.message (I.number state) with
    | Not_found -> "Invalid syntax (No message for this error)")
;;

let input_needed (l : Lexing.lexbuf) (c : 'a I.checkpoint) parse =
  let token = Lexer.token l in
  let startp = l.lex_start_p
  and endp = l.lex_curr_p in
  let c1 = I.offer c (token, startp, endp) in
  parse l c1
;;

let shift_reduce lexbuf checkpoint parse =
  let checkpoint = I.resume checkpoint in
  parse lexbuf checkpoint
;;

let handling_error lexbuf env =
  let line, pos = Misc.get_lexing_position lexbuf in
  let err = get_parse_error env in
  raise (SyntaxError (Some (line, pos), err))
;;

let rejected = SyntaxError (None, "invalid syntax (parser rejected the input)")

let rec parse lexbuf (checkpoint : Ast.g_decl_t list I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _ -> input_needed lexbuf checkpoint parse
  | I.Shifting _ | I.AboutToReduce _ -> shift_reduce lexbuf checkpoint parse
  | I.HandlingError env -> handling_error lexbuf env
  | I.Accepted v -> v
  | I.Rejected -> raise rejected
;;

let main lexbuf =
  try parse lexbuf (Parser.Incremental.main lexbuf.lex_curr_p) with
  | SyntaxError (opt_pos, msg) ->
    let new_msg : string =
      match opt_pos with
      | Some pos -> Misc.parse_pos_error pos msg
      | None -> msg
    in
    raise (Errors.ParseError new_msg)
;;
