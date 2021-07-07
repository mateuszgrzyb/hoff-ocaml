


type module_t = g_decl_t list
  [@@deriving show]

and g_decl_t = 
  | GValDecl of string * type_t * expr_t
  | GFunDecl of string * (string list) * (type_t list) * expr_t
  [@@deriving show]

and type_t = 
  | IntT
  | FloatT
  | BoolT
  | FunT of type_t list
  [@@deriving show]

and lit_t = 
  | Int of int
  | Float of float
  | Bool of bool
  [@@deriving show]

and expr_t = 
  | Val of string
  | Lit of lit_t
  | BinOp of expr_t * binop_t * expr_t
  [@@deriving show]

and binop_t = 
  | Add | Sub | Mul | Div
  [@@deriving show]

let parse_type (t: string): type_t = 
  match t with
  | "Int" -> IntT
  | "Float" -> FloatT
  | "Bool" -> BoolT
  | unknw -> raise (Errors.TypeError ("Type " ^ unknw ^ " is unknown"))
