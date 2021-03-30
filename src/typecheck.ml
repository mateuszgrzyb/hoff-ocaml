
open Ast
open Errors

let typecheck (ds: g_decl_t list): bool =
  let vals: (id_t, type_t) Hashtbl.t = Hashtbl.create 10 in
  let funs: (id_t, fun_t) Hashtbl.t = Hashtbl.create 10 in
  let types: (id_t, type_t) Hashtbl.t = Hashtbl.create 10 in

  (* type checker for global declarations *)
  let rec check_gdecl (d: g_decl_t): unit = match d with 
    | GFunDecl (id, ts, rt, e) -> check_fundecl id ts rt e
    | GValDecl ((id, t), e) -> check_valdecl id t e
    | GTypeDecl _ -> ()
  
  (* type checker for local declarations *)
  and check_decl (d: decl_t): unit = match d with 
    | FunDecl (id, ts, rt, e) -> check_fundecl id ts rt e
    | ValDecl ((id, t), e) -> check_valdecl id t e

  (* type checker for function declaration *)
  and check_fundecl (id: id_t) (ts: typed_id_t list) (rt: type_t) (e: expr_t): unit =
    (* check if arg types exist *)
    List.iter (fun (_, t) -> check_type t) ts;
    (* check if result type exists *)
    check_type rt;
    (* add arguments to namespace *)
    List.iter add_fun_arg_to_namespace ts;
    (* check if declared result type and type of body expression are equal *)
    if ne_types rt (check_expr e)
      then raise (TypeError ("Result type of function "^id^" is mismatched"));
    (* remove arguments from namespace *)
    List.iter (fun (i, _) -> Hashtbl.remove vals i) ts

  (* type checker for value declaration *)
  and check_valdecl (id: id_t) (t: type_t) (e: expr_t): unit =
    (* check if value type exists *)
    check_type t;
    (* check if declared type and expression type are equal*)
    if ne_types t (check_expr e)
      then raise (TypeError ("Declared type of "^id^" is mismatched"))
      else ()

  (* type checker for all expressions *)
  and check_expr (e: expr_t): type_t = match e with
    | BinOp (lh, op, rh) -> check_binop lh op rh
    | UnOp (op, e) -> check_unop op e
    | ConvOp (e, t) -> check_convop e t
    | If (be, e1, e2) -> check_if be e1 e2
    | Let (ds, e) -> check_let ds e
    | Lit (lit) -> check_lit lit
    | Val (id) -> check_val id
    | Fun (n, args) -> check_fun n args
      
  (* type checker for binary operators *)
  and check_binop (lh: expr_t) (op: binop_t) (rh: expr_t): type_t =
    (* check types for left and right sides of operator *)
    let lt = check_expr lh in
    let rt = check_expr rh in
    (* helper *)
    let op_from_pred (pred: (bool)) (v: 'a): 'a option = 
      if pred then Some v else None in
    (* matching operators with valid types *)
    let match_op (t: type_t): type_t option = match op with
    | Add | Sub | Mul | Div -> op_from_pred (t == FloatT || t == IntT) t
    | Rem -> op_from_pred (eq_types t IntT) t
    | Lt | Le | Ge | Gt | Eq | Ne -> Some BoolT in
    (* checking if types for left and right sides are equal and matching with operator *)
    match match_op lt with
    | None -> raise (TypeError ("Types do not match operator "^(show_binop_t op)))
    | Some t -> if eq_types lt rt
      then t
      else raise (TypeError ("Unequal types in operator "^(show_binop_t op)))

  (* type checker for unary operators *)
  and check_unop (op: unop_t) (e: expr_t): type_t = 
    (* checking operand type *)
    let t = check_expr e in
    (* checking if operand type matches operator *)
    match op with
    | Not -> if eq_types t BoolT 
      then t 
      else raise (TypeError ("Types do not match operator "^(show_unop_t op)))
    | Neg -> if (eq_types t FloatT) || (eq_types t IntT)
      then t
      else raise (TypeError ("Types do not match operator "^(show_unop_t op)))

  and check_convop (_: expr_t) (rt: type_t): type_t = rt

  (* type checker for if expression *)
  and check_if (be: expr_t) (e1: expr_t) (e2: expr_t): type_t =
    (* checking types of comparator and branches  *)
    let tb = check_expr be in
    let t1 = check_expr e1 in
    let t2 = check_expr e2 in
    (* type of comparator expr must be boolean and types of branches must be equal *)
    if (ne_types BoolT tb) || (ne_types t1 t2)
      then raise (TypeError "Mismatched types in if expression")
      else t1

  (* type checker for let expression *)
  and check_let (ds: decl_t list) (e: expr_t): type_t =
    (* predeclaration inside whole let scope *)
    List.iter predeclare ds;
    (* checking declaration types *)
    List.iter check_decl ds;
    (* calculating type of body expression *)
    let result = check_expr e in
    (* removing locally declared names *)
    List.iter remove_decl ds;
    (* type of let expression is the type of it's body *)
    result

  (* type checker for literal type *)
  and check_lit (lit: lit_t): type_t = match lit with
    | Int _ -> IntT
    | Bool _ -> BoolT
    | Float _ -> FloatT
    | String _ -> StringT
    | Lambda (args, rt, e) -> 
      let t = check_expr e in
      if eq_types t rt 
        then FunT (List.map snd args, check_expr e)
        else raise (TypeError ("Lambda types do not match"))

  (* name checker for value references *)
  and check_val (id: id_t): type_t =
    try
      (* checking if given value was declared in current scope *)
      Hashtbl.find vals id
    with Not_found -> raise (NameError (id^" does not exist within this scope"))

  (* name and type checker for function call *)
  and check_fun (id: id_t) (args: expr_t list): type_t =
    try
      (* checking if given function was declared in current scope *)
      let ft = Hashtbl.find funs id in
      match ft with | (ts, rt) -> 
        (* checking if given arguments match types with declared argument types *)
        if (List.map check_expr args) <> ts
          then raise (TypeError ("Mismatched argument types in "^id^" function call"))
          else rt
    with Not_found -> raise (NameError (id^" does not exist within this scope"))
 
  (* misc helpers *)

  (* function that populates namespaces with globals before any type and name checking starts *)
  and global_predeclare (d: g_decl_t): unit = match d with
    | GFunDecl (id, ts, rt, _) -> add_fun_to_namespace id ts rt 
    | GValDecl ((id, t), _) -> add_val_to_namespace id t
    | GTypeDecl (id, t) -> add_type_to_namespace id t

  (* function that populates namespaces with locals before any type and name checking starts *)
  and predeclare (d: decl_t): unit = match d with
    | FunDecl (id, ts, rt, _) -> add_fun_to_namespace id ts rt
    | ValDecl ((id, t), _) -> add_val_to_namespace id t

  (* function that removes names from local scope *)
  and remove_decl (d: decl_t): unit = match d with
    | FunDecl (id, _, _, _) -> Hashtbl.remove funs id
    | ValDecl ((id, _), _) -> Hashtbl.remove vals id

  (*
  and add_generic_to_namespace (id: id_t) (nmspc: (id_t, 'a) Hashtbl.t) (t: 'a) (errmsg: string): unit =
    if Option.is_some (Hashtbl.find_opt nmspc id)
      then raise (NameError (errmsg^" "^id^" already exists"))
      else Hashtbl.add nmspc id t
  
  and add_fun_to_namespace (id: id_t) (ts: typed_id_t list) (rt: type_t): unit =
    add_generic_to_namespace id funs (ts, rt) "Function"
  
  and add_val_to_namespace (id: id_t) (t: type_t): unit =
    add_generic_to_namespace id vals t "Value"
  
  and add_alias_to_namespace (id: id_t) (t: type_t): unit =
    add_generic_to_namespace id types t "Type"
  *)

  and add_fun_to_namespace (id: id_t) (ts: typed_id_t list) (rt: type_t): unit =
    if Option.is_some (Hashtbl.find_opt funs id) 
      then raise (NameError ("Function "^id^" already exists"))
      else Hashtbl.add funs id (List.map snd ts, rt)
  
  and add_val_to_namespace (id: id_t) (t: type_t): unit =
    if Option.is_some (Hashtbl.find_opt vals id) 
      then raise (NameError ("Value "^id^" already exists"))
      else Hashtbl.add vals id t

  and add_alias_to_namespace (id: id_t) (t: type_t): unit =
    if Option.is_some (Hashtbl.find_opt types id)
      then raise (NameError ("Type "^id^" already exists"))
      else Hashtbl.add types id t

  and add_type_to_namespace (id: id_t) (ut: user_type_t): unit =
    match ut with
    | Alias (t) -> add_alias_to_namespace id t
    | Sum (prods) -> add_adt_to_namespace id prods

  and add_adt_to_namespace (_: id_t) (_: prod_t list): unit = ()


  and add_fun_arg_to_namespace (arg: id_t * type_t): unit = 
    let (id, t) = arg in match t with
    | FunT (ts, rt) -> add_fun_to_namespace id (List.map (fun t -> ("", t)) ts) rt
    | _ -> add_val_to_namespace id t
  

  and ne_types (t1: type_t) (t2: type_t): bool =
    not (eq_types t1 t2)
  
  and eq_types (t1: type_t) (t2: type_t): bool = match (t1, t2) with
    | (UserT (id), tt) | (tt, UserT (id)) -> 
      (match Hashtbl.find_opt types id with
      | Some t -> t = tt
      | None -> false)
    | _ -> t1 = t2

  and check_type (t: type_t): unit = match t with
    | FunT (ts, rt) -> List.iter check_type ts; check_type rt
    | UserT (id) -> 
      if not (Hashtbl.mem types id)
        then raise (TypeError ("Type "^id^" does not exist"))
    | _ -> ()

  in
  (* body *)

  try
    List.iter global_predeclare ds;
    List.iter check_gdecl ds; 
    true
  with 
  | TypeError e -> print_endline ("TYPE ERROR: " ^ e); false
  | NameError e -> print_endline ("NAME ERROR: " ^ e); false