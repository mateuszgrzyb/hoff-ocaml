type token =
  | INT of (int)
  | FLOAT of (float)
  | ID of (string)
  | BOOL of (bool)
  | ADD
  | SUB
  | MUL
  | DIV
  | LET
  | COLON
  | ASSIGN
  | LPAREN
  | RPAREN
  | COMMA
  | ARROW
  | EOF

open Parsing;;
let _ = parse_error;;
# 3 "src/parser/parser.mly"
  open Ast
  open Errors
  let rec split_typed_ids tids =
    match tids with
    | [] -> ([], [])
    | (i, t) :: xs -> 
    let (is, ts) = split_typed_ids xs in 
    (i :: is, t :: ts) 


# 33 "src/parser/parser.ml"
let yytransl_const = [|
  261 (* ADD *);
  262 (* SUB *);
  263 (* MUL *);
  264 (* DIV *);
  265 (* LET *);
  266 (* COLON *);
  267 (* ASSIGN *);
  268 (* LPAREN *);
  269 (* RPAREN *);
  270 (* COMMA *);
  271 (* ARROW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* FLOAT *);
  259 (* ID *);
  260 (* BOOL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\003\000\003\000\
\003\000\007\000\007\000\008\000\008\000\008\000\008\000\006\000\
\006\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\009\000\009\000\009\000\009\000\004\000\004\000\004\000\010\000\
\010\000\010\000\000\000"

let yylen = "\002\000\
\001\000\001\000\000\000\001\000\002\000\001\000\006\000\009\000\
\001\000\003\000\001\000\000\000\001\000\003\000\001\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\001\000\
\001\000\001\000\001\000\001\000\001\000\003\000\001\000\001\000\
\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\004\000\035\000\000\000\009\000\000\000\
\005\000\000\000\000\000\000\000\031\000\029\000\000\000\000\000\
\011\000\000\000\000\000\013\000\000\000\031\000\032\000\000\000\
\000\000\000\000\000\000\000\000\030\000\000\000\024\000\025\000\
\026\000\018\000\027\000\000\000\019\000\010\000\000\000\011\000\
\014\000\033\000\000\000\000\000\000\000\000\000\000\000\020\000\
\021\000\022\000\023\000\000\000\000\000"

let yydgoto = "\002\000\
\005\000\006\000\009\000\016\000\036\000\019\000\020\000\021\000\
\037\000\024\000"

let yysindex = "\035\000\
\005\000\000\000\000\000\000\000\000\000\008\255\000\000\039\255\
\000\000\002\255\006\255\013\255\000\000\000\000\007\255\032\255\
\000\000\034\255\033\255\000\000\031\255\000\000\000\000\024\255\
\030\255\006\255\037\255\035\255\000\000\006\255\000\000\000\000\
\000\000\000\000\000\000\020\255\000\000\000\000\006\255\000\000\
\000\000\000\000\030\255\030\255\030\255\030\255\038\255\000\000\
\000\000\000\000\000\000\030\255\020\255"

let yyrindex = "\000\000\
\004\000\000\000\001\000\000\000\000\000\048\000\000\000\000\000\
\000\000\000\000\000\000\027\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\040\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\003\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\241\255\233\255\000\000\022\000\000\000\
\000\000\000\000"

let yytablesize = 269
let yytable = "\023\000\
\002\000\007\000\008\000\003\000\004\000\013\000\022\000\007\000\
\014\000\014\000\038\000\011\000\017\000\012\000\042\000\018\000\
\008\000\015\000\015\000\048\000\049\000\050\000\051\000\047\000\
\043\000\044\000\045\000\046\000\053\000\031\000\032\000\033\000\
\034\000\035\000\040\000\001\000\029\000\018\000\030\000\012\000\
\012\000\010\000\025\000\026\000\028\000\027\000\039\000\001\000\
\052\000\041\000\000\000\000\000\016\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\006\000\007\000\008\000\000\000\003\000\000\000\000\000\000\000\
\000\000\006\000\007\000\008\000\003\000"

let yycheck = "\015\000\
\000\000\000\000\000\000\000\000\000\000\000\001\000\001\000\001\
\003\001\003\001\026\000\010\001\000\001\012\001\030\000\003\001\
\009\001\012\001\012\001\043\000\044\000\045\000\046\000\039\000\
\005\001\006\001\007\001\008\001\052\000\000\001\001\001\002\001\
\003\001\004\001\000\001\001\000\013\001\003\001\015\001\013\001\
\014\001\003\001\011\001\010\001\014\001\013\001\010\001\000\000\
\011\001\028\000\255\255\255\255\013\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\001\000\001\000\001\255\255\000\001\255\255\255\255\255\255\
\255\255\009\001\009\001\009\001\009\001"

let yynames_const = "\
  ADD\000\
  SUB\000\
  MUL\000\
  DIV\000\
  LET\000\
  COLON\000\
  ASSIGN\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  ARROW\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  FLOAT\000\
  ID\000\
  BOOL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rev_module) in
    Obj.repr(
# 37 "src/parser/parser.mly"
               ( List.rev _1 )
# 207 "src/parser/parser.ml"
               : Ast.module_t))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "src/parser/parser.mly"
          ( raise (ParsingError ("module")) )
# 213 "src/parser/parser.ml"
               : Ast.module_t))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "src/parser/parser.mly"
    ( [] )
# 219 "src/parser/parser.ml"
               : 'rev_module))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "src/parser/parser.mly"
        ( [] )
# 225 "src/parser/parser.ml"
               : 'rev_module))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rev_module) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'g_decl) in
    Obj.repr(
# 44 "src/parser/parser.mly"
                      ( _2 :: _1 )
# 233 "src/parser/parser.ml"
               : 'rev_module))
; (fun __caml_parser_env ->
    Obj.repr(
# 45 "src/parser/parser.mly"
          ( raise (ParsingError ("rev_module")) )
# 239 "src/parser/parser.ml"
               : 'rev_module))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'type_) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 51 "src/parser/parser.mly"
                                   ( GValDecl (_2, _4, _6) )
# 248 "src/parser/parser.ml"
               : 'g_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'typed_id_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'type_) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "src/parser/parser.mly"
                                                               ( 
    let (ids, types) = split_typed_ids _4 in 
    GFunDecl (_2, ids, (types @ [_7]), _9)
  )
# 261 "src/parser/parser.ml"
               : 'g_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "src/parser/parser.mly"
          ( raise (ParsingError ("g_decl")) )
# 267 "src/parser/parser.ml"
               : 'g_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_) in
    Obj.repr(
# 60 "src/parser/parser.mly"
                   ( (_1, _3) )
# 275 "src/parser/parser.ml"
               : 'typed_id))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "src/parser/parser.mly"
          ( raise (ParsingError ("typed_id")) )
# 281 "src/parser/parser.ml"
               : 'typed_id))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "src/parser/parser.mly"
    ( [] )
# 287 "src/parser/parser.ml"
               : 'rev_typed_id_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'typed_id) in
    Obj.repr(
# 66 "src/parser/parser.mly"
             ( [_1] )
# 294 "src/parser/parser.ml"
               : 'rev_typed_id_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rev_typed_id_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typed_id) in
    Obj.repr(
# 67 "src/parser/parser.mly"
                                     ( _3 :: _1 )
# 302 "src/parser/parser.ml"
               : 'rev_typed_id_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "src/parser/parser.mly"
          ( raise (ParsingError ("rev_typed_id_list")) )
# 308 "src/parser/parser.ml"
               : 'rev_typed_id_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rev_typed_id_list) in
    Obj.repr(
# 72 "src/parser/parser.mly"
                      ( List.rev _1 )
# 315 "src/parser/parser.ml"
               : 'typed_id_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "src/parser/parser.mly"
          ( raise (ParsingError ("typed_id_list")) )
# 321 "src/parser/parser.ml"
               : 'typed_id_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "src/parser/parser.mly"
       ( Val (_1) )
# 328 "src/parser/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lit) in
    Obj.repr(
# 80 "src/parser/parser.mly"
        ( Lit (_1) )
# 335 "src/parser/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "src/parser/parser.mly"
                  ( BinOp (_1, Add, _3) )
# 343 "src/parser/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "src/parser/parser.mly"
                  ( BinOp (_1, Sub, _3) )
# 351 "src/parser/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "src/parser/parser.mly"
                  ( BinOp (_1, Mul, _3) )
# 359 "src/parser/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "src/parser/parser.mly"
                  ( BinOp (_1, Div, _3) )
# 367 "src/parser/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "src/parser/parser.mly"
          ( raise (ParsingError ("expr")) )
# 373 "src/parser/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "src/parser/parser.mly"
        ( Int (_1) )
# 380 "src/parser/parser.ml"
               : 'lit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 92 "src/parser/parser.mly"
          ( Float (_1) )
# 387 "src/parser/parser.ml"
               : 'lit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 93 "src/parser/parser.mly"
         ( Bool (_1) )
# 394 "src/parser/parser.ml"
               : 'lit))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "src/parser/parser.mly"
          ( raise (ParsingError ("lit")) )
# 400 "src/parser/parser.ml"
               : 'lit))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 100 "src/parser/parser.mly"
       ( parse_type _1 )
# 407 "src/parser/parser.ml"
               : 'type_))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'rev_type_list) in
    Obj.repr(
# 101 "src/parser/parser.mly"
                                ( FunT (List.rev _2) )
# 414 "src/parser/parser.ml"
               : 'type_))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "src/parser/parser.mly"
          ( raise (ParsingError ("type")) )
# 420 "src/parser/parser.ml"
               : 'type_))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_) in
    Obj.repr(
# 106 "src/parser/parser.mly"
          ( [_1] )
# 427 "src/parser/parser.ml"
               : 'rev_type_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rev_type_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'type_) in
    Obj.repr(
# 107 "src/parser/parser.mly"
                              ( _3 :: _1 )
# 435 "src/parser/parser.ml"
               : 'rev_type_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "src/parser/parser.mly"
          ( raise (ParsingError ("rev_type_list")) )
# 441 "src/parser/parser.ml"
               : 'rev_type_list))
(* Entry module_ *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let module_ (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.module_t)
