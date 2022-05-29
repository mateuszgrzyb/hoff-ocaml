type module_t = g_decl_t list [@@deriving show]

and g_decl_t =
  | Import of string * id_t
  | GFunDecl of id_t * typed_id_t list * type_t * expr_t
  | GValDecl of typed_id_t * expr_t
  | GTypeDecl of id_t * user_type_t
[@@deriving show]

and expr_t =
  | BinOp of expr_t * binop_t * expr_t
  | UnOp of unop_t * expr_t
  | ConvOp of expr_t * type_t
  | ChainOp of expr_t * expr_t
  | GetOp of expr_t * int
  | NamedGetOp of expr_t * id_t
  | If of expr_t * expr_t * expr_t
  | Case of expr_t * pm_t list
  | Let of decl_t list * expr_t
  | Lit of lit_t
  | Val of string
  | Fun of expr_t * expr_t list
[@@deriving show]

and pm_t = type_t * expr_t [@@deriving show]

and decl_t =
  | FunDecl of id_t * typed_id_t list * type_t * expr_t
  | ValDecl of typed_id_t * expr_t
[@@deriving show]

and type_t =
  | IntT
  | BoolT
  | FloatT
  | StringT
  | FunT of type_t list * type_t
  | UserT of id_t
[@@deriving show]

and user_type_t =
  | Alias of type_t
  | Record of typed_id_t list
  | Union of type_t list
[@@deriving show]

and fun_t = type_t list * type_t [@@deriving show]

and id_t = string [@@deriving show]

and typed_id_t = id_t * type_t [@@deriving show]

and named_expr_t = id_t * expr_t [@@deriving show]

and lit_t =
  | Int of int
  | Bool of bool
  | Float of float
  | String of string
  | Struct of id_t * expr_t list
  | NamedStruct of id_t * named_expr_t list
  | Lambda of typed_id_t list * type_t * expr_t
[@@deriving show]

and binop_t =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Lt
  | Le
  | Ge
  | Gt
  | Ne
  | Eq
  | And
  | Or
[@@deriving show]

and unop_t =
  | Not
  | Neg
[@@deriving show]

let string_to_type (s : string) : type_t =
  match s with
  | "Int" -> IntT
  | "Bool" -> BoolT
  | "Float" -> FloatT
  | "String" -> StringT
  | id -> UserT id
;;

(*
let ast_test (): unit = 
  let ast = [
    GTypeDecl ("Napis", Alias (StringT));
    GTypeDecl ("List", Sum ([Empty ("Nill"); Product ("Cons", [IntT; UserT "List"])]))

  ] in List.iter (fun d -> print_endline (show_g_decl_t d)) ast
 *)
