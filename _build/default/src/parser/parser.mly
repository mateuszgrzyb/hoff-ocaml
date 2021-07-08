
%{
  open Ast
  open Errors
  open Printf

  let rec split_typed_ids tids =
    match tids with
    | [] -> ([], [])
    | (i, t) :: xs -> 
    let (is, ts) = split_typed_ids xs in 
    (i :: is, t :: ts) 

  let _raise_error (msg: string) = 
    let error_pos = get_pos (Parsing.symbol_start_pos ()) in
    raise (ParsingError (sprintf "%s | ln: %d | col: %d \n" msg error_pos.ln error_pos.col))

%}

%token <int> INT 
%token <float> FLOAT 
%token <bool> BOOL
%token <string> STRING

%token <string> ID

%token ADD SUB MUL DIV REM
%token LT LE EQ NE GE GT
%token AND OR NOT

%token COLON ASSIGN
%token LPAREN RPAREN COMMA ARROW FATARROW

%token CONST FUN TYPE
%token IF THEN ELSE FI
%token LET IN TEL
%token CASE BAR OF

%token EOF

// precedences

%nonassoc CASE
%nonassoc LETIN
%nonassoc IFTHENELSE 

%left OR
%left AND
%left EQ NE
%left LT LE GE GT
%left ADD SUB 
%left MUL DIV REM
%right NEG NOT


%type <Ast.module_t> module_
%start module_

%%

// module_t

module_: 
  | rev_module { List.rev $1 }
  | error { _raise_error "module" }
  ;

rev_module: 
  | { [] }
  | EOF { [] }
  | rev_module g_decl { $2 :: $1 }
  | error { _raise_error "rev_expr" }
  ;

// g_decl_t

g_decl:
  | CONST ID COLON type_ ASSIGN expr { GConstDecl ($2, $4, $6) }
  | FUN ID LPAREN typed_id_list RPAREN COLON type_ ASSIGN expr { 
    let (ids, types) = split_typed_ids $4 in 
    GFunDecl ($2, ids, (types @ [$7]), $9)
  }
  | TYPE ID constructor_list { GTypeDecl ($2, $3) }
  | error { _raise_error "g_decl" }
  ;

constructor_list:
  | rev_constructor_list { List.rev $1 }
  | error { _raise_error "constructor_list" }
  ;

rev_constructor_list:
  | BAR constructor { [$2] }
  | rev_constructor_list BAR constructor { $3 :: $1 }
  | error { _raise_error "rev_constructor_list" }
  ;

constructor:
  | ID LPAREN type_list_comma RPAREN { ($1, $3) }
  | error { _raise_error "constructor" }
  ;

type_list_comma:
  | rev_type_list_comma { List.rev $1 }
  | error { _raise_error "type_list_comma" }
  ;

rev_type_list_comma:
  | { [] }
  | type_ { [$1] }
  | rev_type_list_comma COMMA type_ { $3 :: $1 }
  | error { _raise_error "rev_type_list_comma" }
  ;

typed_id:
  | ID COLON type_ { ($1, $3) }
  | error { _raise_error "typed_id" }
  ;

rev_typed_id_list:
  | { [] }
  | typed_id { [$1] }
  | rev_typed_id_list COMMA typed_id { $3 :: $1 }
  | error { _raise_error "rev_typed_id_list" }
  ;

typed_id_list:
  | rev_typed_id_list { List.rev $1 }
  | error { _raise_error "typed_id_list" }
  ;

// decl_t

decl:
  | CONST ID COLON type_ ASSIGN expr { ConstDecl ($2, $4, $6) }
  | FUN ID LPAREN typed_id_list RPAREN COLON type_ ASSIGN expr { 
    let (ids, types) = split_typed_ids $4 in 
    FunDecl ($2, ids, (types @ [$7]), $9)
  }
  | error { _raise_error "decl" }
  ;

// expr_t

expr:
  
  | ID { Val ($1) }
  | lit { Lit ($1) }
 
  | expr ADD expr { BinOp ($1, Add, $3) }
  | expr SUB expr { BinOp ($1, Sub, $3) }
  | expr MUL expr { BinOp ($1, Mul, $3) }
  | expr DIV expr { BinOp ($1, Div, $3) }
  | expr REM expr { BinOp ($1, Rem, $3) }
  
  | expr LT expr { BinOp ($1, Lt, $3) }
  | expr LE expr { BinOp ($1, Le, $3) }
  | expr EQ expr { BinOp ($1, Eq, $3) }
  | expr NE expr { BinOp ($1, Ne, $3) }
  | expr GE expr { BinOp ($1, Ge, $3) }
  | expr GT expr { BinOp ($1, Gt, $3) }

  | expr AND expr { BinOp ($1, And, $3) }
  | expr OR expr { BinOp ($1, Or, $3) }

  | SUB expr %prec NEG { UnOp (Neg, $2) }
  | NOT expr { UnOp (Not, $2) }

  | LPAREN expr RPAREN { $2 }

  | IF expr THEN expr ELSE expr %prec IFTHENELSE { If ($2, $4, $6) }
  | LET decls IN expr %prec LETIN { Let ($2, $4) }
  | CASE expr pattern_match_list %prec CASE { Case ($2, $3) }
  
  | fun_expr LPAREN expr_list RPAREN { Call ($1, $3) }
  | error { _raise_error "expr" }
  ;

pattern_match_list:
  | rev_pattern_match_list { List.rev $1 } 
  | error { _raise_error "pattern_match_list" }
  ;

rev_pattern_match_list:
  | OF pattern_match { [$2] }
  | rev_pattern_match_list OF pattern_match { $3 :: $1 }
  | error { _raise_error "rev_pattern_match_list" }
  ;

pattern_match:
  | ID LPAREN name_list RPAREN ARROW expr { (($1, $3), $6) }
  | error { _raise_error "pattern_match" }
  ;

name_list:
  | rev_name_list { List.rev $1 }
  | error { _raise_error "name_list" }
  ;

rev_name_list:
  | { [] }
  | ID { [$1] }
  | rev_name_list COMMA ID { $3 :: $1 }
  | error { _raise_error "rev_name_list" }
  ;

fun_expr:
  | ID { Val ($1) }
  /*
  | FUN LPAREN typed_id_list RPAREN COLON type_ FATARROW expr { 
    let (ids, types) = split_typed_ids $3 in 
    Lit (Lambda (ids, (types @ [$6]), $8))
  }
  */
  | error { _raise_error "fun_expr" }
  ;

expr_list:
  | rev_expr_list { List.rev $1 }
  | error { _raise_error "expr_list" }
  ;

rev_expr_list:
  | { [] }
  | expr { [$1] }
  | rev_expr_list COMMA expr { $3 :: $1 }
  | error { _raise_error "rev_expr_list" }
  ;

decls:
  | rev_decls { List.rev $1 }
  | error { _raise_error "decls" }
  ;

rev_decls:
  | decl { [$1] }
  | rev_decls decl { $2 :: $1 }
  | error { _raise_error "rev_decls" }
  ;

// lit_t

lit:
  | INT { Int ($1) }
  | FLOAT { Float ($1) }
  | BOOL { Bool ($1) }
  | STRING { String ($1) }
  | error { _raise_error "lit" }
  ;

// type_t

type_:
  | ID { parse_type $1 }
  | LPAREN rev_type_list_arrow RPAREN { FunT (List.rev $2) }
  | error { _raise_error "type" }
  ;

rev_type_list_arrow:
  | type_ { [$1] }
  | rev_type_list_arrow ARROW type_ { $3 :: $1 }
  | error { _raise_error "rev_type_list_arrow" }
  ;

