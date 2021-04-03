

type g_decl_t = 
  | GFunDecl of id_t * (typed_id_t list) * type_t * expr_t
  | GValDecl of typed_id_t * expr_t
  | GTypeDecl of id_t * user_type_t
  [@@deriving show]

and expr_t = 
  | BinOp of expr_t * binop_t * expr_t
  | UnOp of unop_t * expr_t
  | ConvOp of expr_t * type_t
  | If of expr_t * expr_t * expr_t
  | Let of (decl_t list) * expr_t
  | Lit of lit_t
  | Val of string
  | Fun of string * (expr_t list)
  [@@deriving show]

and decl_t = 
  | FunDecl of id_t * (typed_id_t list) * type_t * expr_t
  | ValDecl of typed_id_t * expr_t
  [@@deriving show]

and type_t = 
  | IntT
  | BoolT
  | FloatT
  | StringT
  | FunT of (type_t list) * type_t
  | UserT of id_t
  [@@deriving show]

and user_type_t =
  | Alias of type_t
  | Sum of prod_t list
  [@@deriving show]

and prod_t = 
  | Empty of id_t
  | Product of id_t * (type_t list)
  [@@deriving show]

and fun_t = (type_t list) * type_t
  [@@deriving show]

and id_t = string
  [@@deriving show]

and typed_id_t = (id_t * type_t)
  [@@deriving show]

and lit_t = 
  | Int of int
  | Bool of bool
  | Float of float
  | String of string
  | Lambda of (typed_id_t list) * type_t * expr_t
  [@@deriving show]

and binop_t = 
  | Add | Sub | Mul | Div | Rem
  | Lt  | Le  | Ge  | Gt 
  | Ne  | Eq
  [@@deriving show]

and unop_t = 
  | Not | Neg
  [@@deriving show]

let string_to_type (s: string): type_t = match s with
  | "Int" -> IntT
  | "Bool" -> BoolT
  | "Float" -> FloatT
  | "String" -> StringT
  | id -> UserT (id)

let ast_test (): unit = 
  let ast = [
    GTypeDecl ("Napis", Alias (StringT));
    GTypeDecl ("List", Sum ([Empty ("Nill"); Product ("Cons", [IntT; UserT "List"])]))

  ] in List.iter (fun d -> print_endline (show_g_decl_t d)) ast