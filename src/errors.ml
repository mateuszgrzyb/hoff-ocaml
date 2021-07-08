exception LexingError of string
exception ParsingError of string
exception TypeError of string

type pos_t = 
  { ln: int
  ; col: int
  }

let get_pos (pos: Lexing.position): pos_t = 
  { ln = pos.pos_lnum
  ; col = pos.pos_cnum - pos.pos_bol + 1
  }


let lexbuf_pos (buf: Lexing.lexbuf): pos_t = 
  let position = Lexing.lexeme_start_p buf in 
  { ln = position.pos_lnum
  ; col = position.pos_cnum - position.pos_bol
  }