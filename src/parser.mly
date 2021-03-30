
%{
  open Ast

  let rec list_last (l: 'a list): 'a = match l with
    | [] -> failwith "Empty list"
    | x :: [] -> x
    | _ :: xs -> list_last xs

  let rec list_init (l: 'a list): 'a list = match l with
    | [] -> failwith "Empty list"
    | _ :: [] -> []
    | x :: xs -> x :: (list_init xs)
%}

%token <string> ID
%token <string> TID

%token <int> INT
%token <bool> BOOL
%token <float> FLOAT
%token <string> STRING

%token LC COMMA ARROW RC
%token ASSIGN COLON

%token FUN VAL
%token IF THEN ELSE
%token LET IN
%token CASE
%token TYPE BAR
%token CONV

%token ADD SUB MUL DIV REM
%token AND OR EQ NE 
%token LT LE GE GT
%token NOT

%token OP1 OP2 OP3 OP4 OP5 OP6 OP7 OP8 OP9

%token EOF

%nonassoc ERROR
%right IFTHENELSE LETIN LAMBDA
%left  CONV
%left  OP1 
%left  OP2 AND
%left  OP3 OR
%left  OP4 EQ NE
%left  OP5 LT LE GE GT
%left  OP6 ADD SUB
%left  OP7 MUL DIV REM
%right OP8 NOT NEG
%left  OP9

%type <Ast.g_decl_t list> main
%start main

%%

main: list(g_decl) EOF { $1 }

// g_decl_t

g_decl:
  | FUN ID typed_id_list COLON type_ ASSIGN expr { GFunDecl ($2, $3, $5, $7) }
  | VAL typed_id ASSIGN expr { GValDecl ($2, $4) }
  | TYPE TID user_type { GTypeDecl ($2, $3) }
//| error %prec ERROR { raise (ParseError "g_decl") }


typed_id_list: 
  | LC separated_list(COMMA, typed_id) RC { $2 }
//| error %prec ERROR { raise (ParseError "typed_id_list") }

typed_id: 
  | ID COLON type_ { ($1, $3) }
//| error %prec ERROR { raise (ParseError "typed_id") }

type_:
  | TID { string_to_type $1 }
  | LC separated_list(ARROW, type_) RC { FunT (list_init $2, list_last $2) }
//| error %prec ERROR { raise (ParseError "type") }

user_type:
  | ASSIGN type_ { Alias ($2) }
  | BAR separated_list(BAR, prod_type) { Sum ($2) }
//| error %prec ERROR { raise (ParseError "user_type") }

prod_type:
  | TID { Empty ($1) }
  | TID LC separated_list(COMMA, type_) RC { Product ($1, $3) }
//| error %prec ERROR { raise (ParseError "prod_type") }


/* expr_t */

expr:
  | expr ADD expr { BinOp ($1, Add, $3) }
  | expr SUB expr { BinOp ($1, Sub, $3) }
  | expr MUL expr { BinOp ($1, Mul, $3) }
  | expr DIV expr { BinOp ($1, Div, $3) }
  | expr REM expr { BinOp ($1, Rem, $3) }

  | expr LT expr { BinOp ($1, Lt, $3) }
  | expr LE expr { BinOp ($1, Le, $3) }
  | expr GE expr { BinOp ($1, Ge, $3) }
  | expr GT expr { BinOp ($1, Gt, $3) }
  | expr NE expr { BinOp ($1, Ne, $3) }
  | expr EQ expr { BinOp ($1, Eq, $3) }

  | expr CONV type_ { ConvOp ($1, $3) }

  | LC expr RC { $2 }
  
  | NOT expr           { UnOp (Not, $2) }
  | SUB expr %prec NEG { UnOp (Neg, $2) }

  | IF expr THEN expr ELSE expr %prec IFTHENELSE { If ($2, $4, $6) }
  | LET list(decl) IN expr %prec LETIN           { Let ($2, $4) }
//| CASE BAR separated_list(BAR, pattern) { Case ($3) }

  | lit { Lit ($1) }
  | ID  { Val ($1) }
  | ID LC separated_list(COMMA, expr) RC { Fun ($1, $3) }
//| error %prec ERROR { raise (ParseError "expr") }

// decl_t

decl:
  | FUN ID typed_id_list COLON type_ ASSIGN expr { FunDecl ($2, $3, $5, $7) }
  | VAL typed_id ASSIGN expr { ValDecl ($2, $4) }
//| error %prec ERROR { raise (ParseError "g_decl") }

/* lit_t */

lit:
  | INT    { Int ($1) }
  | BOOL   { Bool ($1) }
  | FLOAT  { Float ($1) }
  | STRING { String ($1) }
  | FUN typed_id_list COLON type_ ARROW expr %prec LAMBDA { Lambda ($2, $4, $6) }
//| error %prec ERROR { raise (ParseError "lit") }

//pattern:
//  | ID ARROW expr { (AnyPat ($1), $3) }
//  | TID LC separated_list(COMMA, ID) RC ARROW expr { (TypePat ($1, $3), $6) }
