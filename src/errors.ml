exception LexingError of string
exception ParsingError of string
exception TypeError of string
exception NameError of string

type pos_t = 
  { ln: int
  ; col: int
  }

let get_pos (pos: Lexing.position): pos_t = 
  { ln = pos.pos_lnum
  ; col = pos.pos_cnum - pos.pos_bol + 1
  }


let lexbuf_pos (buf: Lexing.lexbuf): pos_t = 
  Lexing.lexeme_start_p buf |> get_pos