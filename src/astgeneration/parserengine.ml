

module I = Parser.MenhirInterpreter

exception Syntax_error of ((int * int) option * string)

let get_parse_error env = 
  match I.stack env with
  | lazy Nil -> "Invalid syntax, but I don't know why..."
  | lazy (Cons (I.Element (state, _, _, _), _)) ->
    try (Parser_messages.message (I.number state)) with
    | Not_found -> 
      "Invalid syntax (No message for this error)"

let get_lexing_position lexbuf: (int * int) = 
  let p = Lexing.lexeme_start_p lexbuf in
  (p.pos_lnum, p.pos_cnum - p.pos_bol)

let rec parse lexbuf (checkpoint: Ast.g_decl_t list I.checkpoint) = 
  match checkpoint with
  | I.InputNeeded _env ->
      let token = Lexer.token lexbuf in
      let startp = lexbuf.lex_start_p
      and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      parse lexbuf checkpoint
  | I.Shifting _
  | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse lexbuf checkpoint
  | I.HandlingError _env ->
      let line, pos = get_lexing_position lexbuf in
      let err = get_parse_error _env in
      raise (Syntax_error (Some (line, pos), err))
  | I.Accepted v -> v
  | I.Rejected ->
        raise (Syntax_error (None, "invalid syntax (parser rejected the input)"))

let main lexbuf =
  try
    parse lexbuf (Parser.Incremental.main lexbuf.lex_curr_p)
  with Syntax_error (opt_pos, msg) -> 
    let new_msg: string = (match opt_pos with
    | Some (line, pos) -> Printf.sprintf "(%d, %d): %s" line pos msg
    | None -> msg)
    in raise (Errors.ParseError new_msg)
