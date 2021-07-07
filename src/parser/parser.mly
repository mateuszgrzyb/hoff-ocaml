
%{
  open Ast
  open Errors
  let rec split_typed_ids tids =
    match tids with
    | [] -> ([], [])
    | (i, t) :: xs -> 
    let (is, ts) = split_typed_ids xs in 
    (i :: is, t :: ts) 


%}

%token <int> INT 
%token <float> FLOAT 
%token <string> ID
%token <bool> BOOL

%token ADD SUB MUL DIV

%token LET COLON ASSIGN
%token LPAREN RPAREN COMMA ARROW

%token EOF

%left ADD SUB MUL DIV

%type <Ast.module_t> module_
%start module_

%%

// module_t

module_: 
  | rev_module { List.rev $1 }
  | error { raise (ParsingError ("module")) }
  ;

rev_module: 
  | { [] }
  | EOF { [] }
  | rev_module g_decl { $2 :: $1 }
  | error { raise (ParsingError ("rev_module")) }
  ;

// g_decl_t

g_decl:
  | LET ID COLON type_ ASSIGN expr { GValDecl ($2, $4, $6) }
  | LET ID LPAREN typed_id_list RPAREN COLON type_ ASSIGN expr { 
    let (ids, types) = split_typed_ids $4 in 
    GFunDecl ($2, ids, (types @ [$7]), $9)
  }
  | error { raise (ParsingError ("g_decl")) }
  ;

typed_id:
  | ID COLON type_ { ($1, $3) }
  | error { raise (ParsingError ("typed_id")) }
  ;

rev_typed_id_list:
  | { [] }
  | typed_id { [$1] }
  | rev_typed_id_list COMMA typed_id { $3 :: $1 }
  | error { raise (ParsingError ("rev_typed_id_list")) }
  ;

typed_id_list:
  | rev_typed_id_list { List.rev $1 }
  | error { raise (ParsingError ("typed_id_list")) }
  ;

// expr_t

expr:
  | ID { Val ($1) }
  | lit { Lit ($1) }
  | expr ADD expr { BinOp ($1, Add, $3) }
  | expr SUB expr { BinOp ($1, Sub, $3) }
  | expr MUL expr { BinOp ($1, Mul, $3) }
  | expr DIV expr { BinOp ($1, Div, $3) }
  | error { raise (ParsingError ("expr")) }
  ;

// lit_t

lit:
  | INT { Int ($1) }
  | FLOAT { Float ($1) }
  | BOOL { Bool ($1) }
  | error { raise (ParsingError ("lit")) }
  ;

// type_t

type_:
  | ID { parse_type $1 }
  | LPAREN rev_type_list RPAREN { FunT (List.rev $2) }
  | error { raise (ParsingError ("type")) }
  ;

rev_type_list:
  | type_ { [$1] }
  | rev_type_list ARROW type_ { $3 :: $1 }
  | error { raise (ParsingError ("rev_type_list")) }
  ;

