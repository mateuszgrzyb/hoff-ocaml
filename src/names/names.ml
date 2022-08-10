open Core
open Printf

module Helpers = struct
  let stdio_names =
    [ "read_int"
    ; "read_bool"
    ; "read_float"
    ; "read_string"
    ; "print_int"
    ; "print_bool"
    ; "print_float"
    ; "print_string"
    ; "GC_malloc"
    ; "main"
    ]
  ;;

  let imported_names = Hashtbl.create ~growth_allowed:true ~size:10 (module String)

  let qualify_id (name : string) (id : Ast.id_t) : Ast.id_t =
    if List.exists stdio_names ~f:(fun e -> String.equal e id) || String.contains id '.'
    then id
    else (
      let imported_name = Hashtbl.find imported_names id in
      match imported_name with
      | None -> sprintf "%s.%s" name id
      | Some name -> sprintf "%s.%s" name id)
  ;;

  let qualify_type (name : string) (t : Ast.type_t) : Ast.type_t =
    match t with
    | UserT id -> UserT (qualify_id name id)
    | _ -> t
  ;;

  let qualify_arg_types (name : string) (ts : Ast.typed_id_t list) : Ast.typed_id_t list =
    ts |> List.map ~f:(fun (id, t) -> qualify_id name id, qualify_type name t)
  ;;
end

let rec qualify_module (name : string) (ds : Ast.g_decl_t list) : Ast.g_decl_t list =
  ds |> List.map ~f:(qualify_g_decl name)

and qualify_g_decl (name : string) (d : Ast.g_decl_t) : Ast.g_decl_t =
  let qualify_import (module_ : string) (id : Ast.id_t) : Ast.g_decl_t =
    Hashtbl.set Helpers.imported_names ~key:id ~data:module_;
    Ast.Import (module_, id)
  in
  let qualify_g_fundecl
      (id : Ast.id_t)
      (ts : Ast.typed_id_t list)
      (rt : Ast.type_t)
      (body : Ast.expr_t)
      : Ast.g_decl_t
    =
    let id = Helpers.qualify_id name id in
    let ts = Helpers.qualify_arg_types name ts in
    let rt = Helpers.qualify_type name rt in
    let body = qualify_expr name body in
    Ast.GFunDecl (id, ts, rt, body)
  in
  let qualify_g_valdecl (id : Ast.id_t) (t : Ast.type_t) (expr : Ast.expr_t)
      : Ast.g_decl_t
    =
    let id = Helpers.qualify_id name id in
    let t = Helpers.qualify_type name t in
    let expr = qualify_expr name expr in
    Ast.GValDecl ((id, t), expr)
  in
  let qualify_g_typedecl (id : Ast.id_t) (ut : Ast.user_type_t) : Ast.g_decl_t =
    let id = Helpers.qualify_id name id in
    Ast.GTypeDecl (id, ut)
  in
  match d with
  | Ast.Import (module_, id) -> qualify_import module_ id
  | Ast.GFunDecl (id, ts, rt, body) -> qualify_g_fundecl id ts rt body
  | Ast.GValDecl ((id, t), expr) -> qualify_g_valdecl id t expr
  | Ast.GTypeDecl (id, ut) -> qualify_g_typedecl id ut

and qualify_expr (name : string) (expr : Ast.expr_t) : Ast.expr_t =
  let qualify_binop (lh : Ast.expr_t) (op : Ast.binop_t) (rh : Ast.expr_t) : Ast.expr_t =
    let lh = qualify_expr name lh in
    let rh = qualify_expr name rh in
    Ast.BinOp (lh, op, rh)
  in
  let qualify_unop (op : Ast.unop_t) (expr : Ast.expr_t) : Ast.expr_t =
    let expr = qualify_expr name expr in
    Ast.UnOp (op, expr)
  in
  let qualify_convop (expr : Ast.expr_t) (t : Ast.type_t) : Ast.expr_t =
    let expr = qualify_expr name expr in
    let t = Helpers.qualify_type name t in
    Ast.ConvOp (expr, t)
  in
  let qualify_chainop (lh : Ast.expr_t) (rh : Ast.expr_t) : Ast.expr_t =
    let lh = qualify_expr name lh in
    let rh = qualify_expr name rh in
    Ast.ChainOp (lh, rh)
  in
  let qualify_getop (expr : Ast.expr_t) (i : int) : Ast.expr_t =
    let expr = qualify_expr name expr in
    Ast.GetOp (expr, i)
  in
  let qualify_namedgetop (expr : Ast.expr_t) (n : Ast.id_t) : Ast.expr_t =
    let expr = qualify_expr name expr in
    Ast.NamedGetOp (expr, n)
  in
  let qualify_if (be : Ast.expr_t) (e1 : Ast.expr_t) (e2 : Ast.expr_t) : Ast.expr_t =
    let be = qualify_expr name be in
    let e1 = qualify_expr name e1 in
    let e2 = qualify_expr name e2 in
    Ast.If (be, e1, e2)
  in
  let qualify_case (expr : Ast.expr_t) (pms : Ast.pm_t list) : Ast.expr_t =
    let qualify_pattern (pm : Ast.pm_t) : Ast.pm_t =
      let p, m = pm in
      let p = Helpers.qualify_type name p in
      let m = qualify_expr name m in
      p, m
    in
    let expr = qualify_expr name expr in
    let pms = pms |> List.map ~f:qualify_pattern in
    Ast.Case (expr, pms)
  in
  let qualify_let (ds : Ast.decl_t list) (expr : Ast.expr_t) : Ast.expr_t =
    let ds = ds |> List.map ~f:(qualify_decl name) in
    let expr = qualify_expr name expr in
    Ast.Let (ds, expr)
  in
  let qualify_lit (lit : Ast.lit_t) : Ast.expr_t =
    let qualify_struct (id : Ast.id_t) (args : Ast.expr_t list) : Ast.lit_t =
      let id = Helpers.qualify_id name id in
      Ast.Struct (id, args)
    in
    let qualify_namedstruct (id : Ast.id_t) (namedargs : Ast.named_expr_t list)
        : Ast.lit_t
      =
      let id = Helpers.qualify_id name id in
      let namedargs = namedargs |> List.map ~f:(fun (n, e) -> n, qualify_expr name e) in
      Ast.NamedStruct (id, namedargs)
    in
    let qualify_lambda (ts : Ast.typed_id_t list) (rt : Ast.type_t) (body : Ast.expr_t)
        : Ast.lit_t
      =
      let ts = Helpers.qualify_arg_types name ts in
      let rt = Helpers.qualify_type name rt in
      let body = qualify_expr name body in
      Ast.Lambda (ts, rt, body)
    in
    let lit =
      match lit with
      | Ast.Struct (id, args) -> qualify_struct id args
      | Ast.NamedStruct (id, namedargs) -> qualify_namedstruct id namedargs
      | Ast.Lambda (ts, rt, body) -> qualify_lambda ts rt body
      | _ -> lit
    in
    Ast.Lit lit
  in
  let qualify_val (id : Ast.id_t) : Ast.expr_t =
    let id = Helpers.qualify_id name id in
    Ast.Val id
  in
  let qualify_fun (f : Ast.expr_t) (args : Ast.expr_t list) : Ast.expr_t =
    let f = qualify_expr name f in
    let args = args |> List.map ~f:(qualify_expr name) in
    Ast.Fun (f, args)
  in
  match expr with
  | Ast.BinOp (lh, op, rh) -> qualify_binop lh op rh
  | Ast.UnOp (op, expr) -> qualify_unop op expr
  | Ast.ConvOp (expr, t) -> qualify_convop expr t
  | Ast.ChainOp (lh, rh) -> qualify_chainop lh rh
  | Ast.GetOp (expr, i) -> qualify_getop expr i
  | Ast.NamedGetOp (expr, n) -> qualify_namedgetop expr n
  | Ast.If (be, e1, e2) -> qualify_if be e1 e2
  | Ast.Case (expr, pms) -> qualify_case expr pms
  | Ast.Let (ds, expr) -> qualify_let ds expr
  | Ast.Lit lit -> qualify_lit lit
  | Ast.Val id -> qualify_val id
  | Ast.Fun (f, args) -> qualify_fun f args

and qualify_decl (name : string) (d : Ast.decl_t) : Ast.decl_t =
  let qualify_fundecl
      (id : Ast.id_t)
      (ts : Ast.typed_id_t list)
      (rt : Ast.type_t)
      (body : Ast.expr_t)
      : Ast.decl_t
    =
    let id = Helpers.qualify_id name id in
    let ts = Helpers.qualify_arg_types name ts in
    let rt = Helpers.qualify_type name rt in
    let body = qualify_expr name body in
    Ast.FunDecl (id, ts, rt, body)
  in
  let qualify_valdecl (id : Ast.id_t) (t : Ast.type_t) (expr : Ast.expr_t) : Ast.decl_t =
    let id = Helpers.qualify_id name id in
    let t = Helpers.qualify_type name t in
    let expr = qualify_expr name expr in
    Ast.ValDecl ((id, t), expr)
  in
  match d with
  | Ast.FunDecl (id, ts, rt, body) -> qualify_fundecl id ts rt body
  | Ast.ValDecl ((id, t), expr) -> qualify_valdecl id t expr
;;
