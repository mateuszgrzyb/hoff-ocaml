


type module_t = string * g_decl_t list
  [@@deriving show]

and g_decl_t = 
  | GConstDecl of string * type_t * expr_t
  | GFunDecl of string * (string list) * (type_t list) * expr_t
  | GTypeDecl of string * user_type_t
  [@@deriving show]

and decl_t = 
  | ConstDecl of string * type_t * expr_t
  | FunDecl of string * (string list) * (type_t list) * expr_t
  [@@deriving show]

and user_type_t = 
  | Alias of type_t
  | ADT of constructor_t list
  [@@deriving show]

and constructor_t = string * (type_t list)
  [@@deriving show]

and type_t = 
  | IntT
  | FloatT
  | BoolT
  | StringT
  | FunT of type_t list
  | UserT of string
  [@@deriving show]

and lit_t = 
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Lambda of (string list) * (type_t list) * expr_t
  [@@deriving show]

and expr_t = 
  | Val of id_t
  | Lit of lit_t
  | BinOp of expr_t * binop_t * expr_t
  | UnOp of unop_t * expr_t
  | If of expr_t * expr_t * expr_t
  | Let of (decl_t list) * expr_t
  | Case of expr_t * (pattern_t * expr_t) list
  | Call of id_t * (expr_t list)
  [@@deriving show]

and id_t = 
  | QualifiedId of (string list) * string
  | Id of string
  [@@deriving show]

and pattern_t = string * (string list)
  [@@deriving show]

and binop_t = 
  (* arithmetic operators *)
  | Add | Sub | Mul | Div | Rem
  (* relational operators *)
  | Lt | Le | Eq | Ne | Ge | Gt
  (* logical operators *)
  | And | Or
  (* function composition operators *)
  | LChain | RChain
  [@@deriving show]

and unop_t = 
  (* arithmetic operators *)
  | Neg
  (* logical operators *)
  | Not 
  [@@deriving show]

let parse_type (t: string): type_t = 
  match t with
  | "Int" -> IntT
  | "Float" -> FloatT
  | "Bool" -> BoolT
  | "String" -> StringT
  | unknwn -> UserT (unknwn)
