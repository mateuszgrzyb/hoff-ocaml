
%{
  open Ast

  let rec list_init (l: 'a list): 'a list = match l with
  | [] -> failwith "ListInitError"
  | _ :: [] -> []
  | x :: xs -> x :: (list_init xs)

  let rec list_last (l: 'a list): 'a = match l with
  | [] -> failwith "ListInitError"
  | x :: [] -> x
  | _ :: xs -> list_last xs


  let op_name (op: string): string = 
    "HOFF_OVERLOADED_OPERATOR_" ^ op

%}

%token <string> ID
%token <string> TID

%token <int> INT
%token <bool> BOOL
%token <float> FLOAT
%token <string> STRING

%token LC "(" COMMA "," ARROW "->" RC ")"
%token ASSIGN "=" COLON ":"

%token FUN "fun" VAL "val"
%token IF "if" THEN "then" ELSE "else"
%token LET "let" IN "in"
%token CASE "case"
%token TYPE "type" BAR "|"
%token CONV "::"

%token ADD "+" SUB "-" MUL "*" DIV "/" REM "%"
%token AND "&&" OR "||" EQ "==" NE "!="
%token LT "<" LE "<=" GE ">=" GT ">"
%token NOT "!"

%token <string> OP

%token EOF

%nonassoc ERROR
%right IFTHENELSE LETIN LAMBDA
%left  CONV
%left  OP 
%left  AND
%left  OR
%left  EQ NE
%left  LT LE GE GT
%left  ADD SUB
%left  MUL DIV REM
%right NOT NEG

%type <Ast.g_decl_t list> main
%start main

%%

main: list(g_decl) EOF { $1 }

// g_decl_t

g_decl:
  | "fun" ID typed_id_list ":" type_ "=" expr { GFunDecl ($2, $3, $5, $7) }
  //| FUN LC OP RC LC typed_id COMMA typed_id RC COLON type_ ASSIGN expr { GFunDecl (op_name $3, [$6;$8], $11, $13) }
  | "fun" "(" OP ")" "(" typed_id "," typed_id ")" ":" type_ "=" expr { GFunDecl (op_name $3, [$6;$8], $11, $13) }
  //| VAL typed_id ASSIGN expr { GValDecl ($2, $4) }
  | "val" typed_id "=" expr { GValDecl ($2, $4) }
  //| TYPE TID user_type { GTypeDecl ($2, $3) }
  | "type" TID user_type { GTypeDecl ($2, $3) }


typed_id_list: 
  | "(" separated_list(",", typed_id) ")" { $2 }

typed_id: 
  | ID ":" type_ { ($1, $3) }

type_:
  | TID { string_to_type $1 }
  | "(" separated_list("->", type_) ")" { FunT (list_init $2, list_last $2) }

user_type:
  | "=" type_ { Alias ($2) }
  | "|" separated_list("|", prod_type) { Sum ($2) }

prod_type:
  | TID { Empty ($1) }
  | TID LC separated_list(COMMA, type_) RC { Product ($1, $3) }


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

  | expr OP expr { Fun (op_name $2, [$1; $3]) }

  | expr CONV type_ { ConvOp ($1, $3) }

  | LC expr RC { $2 }
  
  | NOT expr           { UnOp (Not, $2) }
  | SUB expr %prec NEG { UnOp (Neg, $2) }

  | IF expr THEN expr ELSE expr %prec IFTHENELSE { If ($2, $4, $6) }
  | LET list(decl) IN expr %prec LETIN           { Let ($2, $4) }

  | lit { Lit ($1) }
  | ID  { Val ($1) }
  | ID LC separated_list(COMMA, expr) RC { Fun ($1, $3) }



// decl_t

decl:
  | FUN ID typed_id_list COLON type_ ASSIGN expr { FunDecl ($2, $3, $5, $7) }
  | FUN LC OP RC LC typed_id COMMA typed_id RC COLON type_ ASSIGN expr { FunDecl (op_name $3, [$6;$8], $11, $13) }
  | VAL typed_id ASSIGN expr { ValDecl ($2, $4) }

/* lit_t */

lit:
  | INT    { Int ($1) }
  | BOOL   { Bool ($1) }
  | FLOAT  { Float ($1) }
  | STRING { String ($1) }
  | FUN typed_id_list COLON type_ ARROW expr %prec LAMBDA { Lambda ($2, $4, $6) }