
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
    "$HOFF_OVERLOADED_OPERATOR_" ^ op

%}

%token <string> NAME
%token <string> ID
%token <string> TID

%token <int> INT
%token <bool> BOOL
%token <float> FLOAT
%token <string> STRING

%token LC "(" COMMA "," ARROW "->" RC ")"
%token ASSIGN "=" COLON ":"
%token LB "{" RB "}"

%token FUN "fun" VAL "val"
%token IF "if" THEN "then" ELSE "else"
%token LET "let" IN "in"
%token CASE "case"
%token TYPE "type" BAR "|" 
%token CONV "::" CHAIN ";;"
%token BEGIN "begin" END "end"
%token FROM "from" IMPORT "import"

%token ADD "+" SUB "-" MUL "*" DIV "/" REM "%"
%token AND "&&" OR "||" EQ "==" NE "!="
%token LT "<" LE "<=" GE ">=" GT ">"
%token NOT "!"

%token <string> OP

%token EOF

%left ARG
%left IFTHENELSE LETIN LAMBDA
%left CHAIN
%left  CONV
%left  OP 
%left  AND
%left  OR
%left  EQ NE
%left  LT LE GE GT
%left  ADD SUB
%left  MUL DIV REM
%right NOT NEG
%nonassoc CALL

%type <Ast.g_decl_t list> main
%start main

%%

main: list(import) list(g_decl) EOF { $1 @ $2 }

// g_decl_t

import:
  | "from" ID "import" ID { Import ($2, $4) }
  | "from" ID "import" TID { Import ($2, $4) }

g_decl:
  | "fun" ID typed_id_list ":" type_ "=" expr { GFunDecl ($2, $3, $5, $7) }
  | "fun" "(" OP ")" "(" typed_id "," typed_id ")" ":" type_ "=" expr { GFunDecl (op_name $3, [$6;$8], $11, $13) }
  | "val" typed_id "=" expr { GValDecl ($2, $4) }
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
  | "=" "{" separated_list(",", typed_id) "}" { Record ($3) }
  | "|" separated_list("|", type_) { Union ($2) }


/* expr_t */

expr:
  | expr "+" expr { BinOp ($1, Add, $3) }
  | expr "-" expr { BinOp ($1, Sub, $3) }
  | expr "*" expr { BinOp ($1, Mul, $3) }
  | expr "/" expr { BinOp ($1, Div, $3) }
  | expr "%" expr { BinOp ($1, Rem, $3) }

  | expr "<"  expr { BinOp ($1, Lt, $3) }
  | expr "<=" expr { BinOp ($1, Le, $3) }
  | expr ">=" expr { BinOp ($1, Ge, $3) }
  | expr ">"  expr { BinOp ($1, Gt, $3) }
  | expr "!=" expr { BinOp ($1, Ne, $3) }
  | expr "==" expr { BinOp ($1, Eq, $3) }
  
  | expr "&&" expr { BinOp ($1, And, $3) }
  | expr "||" expr { BinOp ($1, Or, $3) }

  | expr "->" INT { GetOp ($1, $3) }
  | expr "->" ID { NamedGetOp ($1, $3) }

  | expr OP expr { Fun (Val(op_name $2), [$1; $3]) }
 
  | expr "::" type_ { ConvOp ($1, $3) }
  | expr ";;" expr { ChainOp ($1, $3) }

  | "(" expr ")" { $2 }
  | "begin" expr "end" { $2 }
  
  | "!" expr           { UnOp (Not, $2) }
  | "-" expr %prec NEG { UnOp (Neg, $2) }

  | "if" expr "then" expr "else" expr %prec IFTHENELSE { If ($2, $4, $6) }
  | "let" list(decl) "in" expr %prec LETIN             { Let ($2, $4) }
  | "case" expr list(matched_pattern) "end" %prec CASE       { Case ($2, $3) }

  | lit { Lit ($1) }
  | ID  { Val ($1) }
  | expr "(" separated_list(",", expr) ")" %prec CALL { Fun ($1, $3) }

matched_pattern:
  | "|" type_ "->" expr { ($2, $4) }


// decl_t

decl:
  | "fun" ID typed_id_list ":" type_ "=" expr { FunDecl ($2, $3, $5, $7) }
  | "fun" "(" OP ")" "(" typed_id "," typed_id ")" ":" type_ "=" expr { FunDecl (op_name $3, [$6;$8], $11, $13) }
  | "val" typed_id "=" expr { ValDecl ($2, $4) }

/* lit_t */

named_expr:
  | ID "=" expr { ($1, $3) }

lit:
  | INT    { Int ($1) }
  | BOOL   { Bool ($1) }
  | FLOAT  { Float ($1) }
  | STRING { String ($1) }
  | TID "(" separated_list(",", expr) ")" { Struct ($1, $3) }
  | TID "{" separated_list(",", named_expr) "}" { NamedStruct ($1, $3) }
  | FUN typed_id_list ":" type_ "=" expr %prec LAMBDA { Lambda ($2, $4, $6) }