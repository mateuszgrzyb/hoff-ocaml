open Ast
open Errors

let rec ne_types (t1 : type_t) (t2 : type_t) : bool = not (eq_types t1 t2)
and eq_types (t1 : type_t) (t2 : type_t) : bool = t1 = t2
(*
  match (t1, t2) with
    | (UserT (id), tt) | (tt, UserT (id)) -> 
      (match Hashtbl.find_opt types id with
      | Some t -> t = tt
      | None -> false)
    | _ -> t1 = t2
  *)

class ['v] hashtable (name : string) =
  object
    val name = name
    val _table : (id_t, 'v) Hashtbl.t = Hashtbl.create 10
    method add (k : id_t) (v : 'v) : unit = Hashtbl.add _table k v
    method remove (k : id_t) : unit = Hashtbl.remove _table k

    method find (k : id_t) : 'v =
      try Hashtbl.find _table k with
      | Not_found ->
        let scope : string = Hashtbl.fold (fun k _v z -> k ^ "\n" ^ z) _table "" in
        raise
          (NameError (name ^ " " ^ k ^ " does not exist within this scope: \n" ^ scope))

    method find_opt (k : id_t) : 'v option = Hashtbl.find_opt _table k
  end

let typecheck (ds : g_decl_t list) : unit =
  (*
  let vals: (id_t, type_t) Hashtbl.t = Hashtbl.create 10 in
  let funs: (id_t, fun_t) Hashtbl.t = Hashtbl.create 10 in
  let types: (id_t, type_t) Hashtbl.t = Hashtbl.create 10 in
  *)
  let vals = new hashtable "Values" in
  let funs = new hashtable "Funs" in
  let types = new hashtable "Types" in
  funs#add "read_int" ([], IntT);
  funs#add "read_bool" ([], BoolT);
  funs#add "read_float" ([], FloatT);
  funs#add "read_string" ([], StringT);
  funs#add "print_int" ([ IntT ], IntT);
  funs#add "print_bool" ([ BoolT ], IntT);
  funs#add "print_float" ([ FloatT ], IntT);
  funs#add "print_string" ([ StringT ], IntT);
  (* type checker for global declarations *)
  let rec check_gdecl (d : g_decl_t) : unit =
    match d with
    | GFunDecl (id, ts, rt, e) -> check_fundecl id ts rt e
    | GValDecl ((id, t), e) ->
      check_valdecl id t e;
      add_val_to_namespace id t
    | GTypeDecl (id, user_type) -> add_type_to_namespace id user_type
  (* type checker for local declarations *)
  and check_decl (d : decl_t) : unit =
    match d with
    | FunDecl (id, ts, rt, e) -> check_fundecl id ts rt e
    | ValDecl ((id, t), e) -> check_valdecl id t e
  (* type checker for function declaration *)
  and check_fundecl (id : id_t) (ts : typed_id_t list) (rt : type_t) (e : expr_t) : unit =
    (* check if arg types exist *)
    List.iter (fun (_, t) -> check_type t) ts;
    (* check if result type exists *)
    check_type rt;
    (* add arguments to namespace *)
    List.iter
      (fun arg ->
        let id, t = arg in
        match t with
        | FunT (ts, rt) -> add_fun_to_namespace id (List.map (fun t -> "", t) ts) rt
        | _ -> add_val_to_namespace id t)
      ts;
    (* check if declared result type and type of body expression are equal *)
    if ne_types rt (check_expr e)
    then raise (TypeError ("Result type of function " ^ id ^ " is mismatched"));
    (* remove arguments from namespace *)
    (*
    List.iter (fun (i, _) -> Hashtbl.remove vals i) ts
    *)
    List.iter (fun (i, _) -> vals#remove i) ts
  (* type checker for value declaration *)
  and check_valdecl (id : id_t) (t : type_t) (e : expr_t) : unit =
    (* check if value type exists *)
    check_type t;
    (* check if declared type and expression type are equal*)
    if ne_types t (check_expr e)
    then raise (TypeError ("Declared type of " ^ id ^ " is mismatched"))
    else ()
  (* type checker for all expressions *)
  and check_expr (e : expr_t) : type_t =
    match e with
    | BinOp (lh, op, rh) -> check_binop lh op rh
    | UnOp (op, e) -> check_unop op e
    | ConvOp (e, t) -> check_convop e t
    | ChainOp (_, e) -> check_expr e
    | GetOp (e, i) -> check_get e i
    | If (be, e1, e2) -> check_if be e1 e2
    | Let (ds, e) -> check_let ds e
    | Lit lit -> check_lit lit
    | Val id -> check_val id
    | Fun (n, args) -> check_fun n args
  (* type checker for binary operators *)
  and check_binop (lh : expr_t) (op : binop_t) (rh : expr_t) : type_t =
    (* check types for left and right sides of operator *)
    let lt = check_expr lh in
    let rt = check_expr rh in
    (* helper *)
    let op_from_pred (pred : bool) (v : 'a) : 'a option = if pred then Some v else None in
    (* matching operators with valid types *)
    let match_op (t : type_t) : type_t option =
      match op with
      | Add | Sub | Mul | Div -> op_from_pred (t == FloatT || t == IntT) t
      | Rem -> op_from_pred (eq_types t IntT) t
      | Lt | Le | Ge | Gt | Eq | Ne -> Some BoolT
      | And | Or -> op_from_pred (t == BoolT) t
    in
    (* checking if types for left and right sides are equal and matching with operator *)
    match match_op lt with
    | None -> raise (TypeError ("Types do not match operator " ^ show_binop_t op))
    | Some t ->
      if eq_types lt rt
      then t
      else raise (TypeError ("Unequal types in operator " ^ show_binop_t op))
  (* type checker for unary operators *)
  and check_unop (op : unop_t) (e : expr_t) : type_t =
    (* checking operand type *)
    let t = check_expr e in
    (* checking if operand type matches operator *)
    match op with
    | Not ->
      if eq_types t BoolT
      then t
      else raise (TypeError ("Types do not match operator " ^ show_unop_t op))
    | Neg ->
      if eq_types t FloatT || eq_types t IntT
      then t
      else raise (TypeError ("Types do not match operator " ^ show_unop_t op))
  and check_convop (_ : expr_t) (rt : type_t) : type_t = rt
  and check_get (expr : expr_t) (i : int) : type_t =
    let t = check_expr expr in
    (*
    let t = vals#find id in
       *)
    match t with
    | UserT tid ->
      let ut : user_type_t = types#find tid in
      (match ut with
      | Record types -> List.nth types i
      | _ -> raise (TypeError ("Cannot dereference value of type " ^ show_user_type_t ut)))
    | _ -> raise (TypeError ("Cannot dereference value of type " ^ show_type_t t))
  (* type checker for if expression *)
  and check_if (be : expr_t) (e1 : expr_t) (e2 : expr_t) : type_t =
    (* checking types of comparator and branches  *)
    let tb = check_expr be in
    let t1 = check_expr e1 in
    let t2 = check_expr e2 in
    (* type of comparator expr must be boolean and types of branches must be equal *)
    if ne_types BoolT tb || ne_types t1 t2
    then raise (TypeError "Mismatched types in if expression")
    else t1
  (* type checker for let expression *)
  and check_let (ds : decl_t list) (e : expr_t) : type_t =
    (* predeclaration inside whole let scope *)
    List.iter
      (function
        | FunDecl (id, ts, rt, _) -> add_fun_to_namespace id ts rt
        | ValDecl ((id, t), _) -> add_val_to_namespace id t)
      ds;
    (* checking declaration types *)
    List.iter check_decl ds;
    (* calculating type of body expression *)
    let result = check_expr e in
    (* removing locally declared names *)
    List.iter
      (function
        | FunDecl (id, _, _, _) -> funs#remove id
        | ValDecl ((id, _), _) -> vals#remove id)
      ds;
    (* type of let expression is the type of it's body *)
    result
  (* type checker for literal type *)
  and check_lit (lit : lit_t) : type_t =
    match lit with
    | Int _ -> IntT
    | Bool _ -> BoolT
    | Float _ -> FloatT
    | String _ -> StringT
    | Struct (id, _) -> UserT id
    | Lambda (args, rt, e) ->
      check_fundecl "LAMBDA FUNCTION" args rt e;
      FunT (List.map snd args, check_expr e)
  (*
      let t = check_expr e in
      if eq_types t rt 
        then FunT (List.map snd args, check_expr e)
        else raise (TypeError ("Lambda types do not match"))
      *)
  (* name checker for value references *)
  and check_val (id : id_t) : type_t =
    let opt_v = vals#find_opt id in
    if Option.is_some opt_v
    then Option.get opt_v
    else (
      let opt_f = funs#find_opt id in
      if Option.is_some opt_f
      then (
        let args, rt = Option.get opt_f in
        FunT (args, rt))
      else raise (NameError (id ^ " does not exist within this scope")))
  (*
    try
      (* checking if given value was declared in current scope *)
      Hashtbl.find vals id
    with Not_found -> (
      try
        let (args, rt) = Hashtbl.find funs id in FunT (args, rt)
      with Not_found ->
        raise (NameError (id^" does not exist within this scope"))
      )
    *)
  (* name and type checker for function call *)
  and check_fun (expr : expr_t) (args : expr_t list) : type_t =
    try
      let ft = check_expr expr in
      (* checking if given function was declared in current scope *)
      (*
      let ft = funs#find id in
       *)
      match ft with
      | FunT (ts, rt) ->
        (* checking if given arguments match types with declared argument types *)
        if List.map check_expr args <> ts
        then
          raise
            (TypeError
               ("Mismatched argument types in " ^ show_expr_t expr ^ " function call"))
        else rt
      | _ -> raise (TypeError ("Expression " ^ show_expr_t expr ^ " is not callable"))
    with
    | Not_found ->
      raise (NameError (show_expr_t expr ^ " does not exist within this scope"))
  (* misc helpers *)
  and add_fun_to_namespace (id : id_t) (ts : typed_id_t list) (rt : type_t) : unit =
    if Option.is_some (funs#find_opt id)
    then raise (NameError ("Function " ^ id ^ " already exists"))
    else funs#add id (List.map snd ts, rt)
  and add_val_to_namespace (id : id_t) (t : type_t) : unit =
    if Option.is_some (vals#find_opt id)
    then raise (NameError ("Value " ^ id ^ " already exists"))
    else vals#add id t
  and add_type_to_namespace (id : id_t) (t : user_type_t) : unit =
    if Option.is_some (types#find_opt id)
    then raise (NameError ("Value " ^ id ^ " already exists"))
    else types#add id t
  and check_type (t : type_t) : unit =
    match t with
    | FunT (ts, rt) ->
      List.iter check_type ts;
      check_type rt
    (*
    | UserT (id) -> 
      if not (Hashtbl.mem types id)
        then raise (TypeError ("Type "^id^" does not exist"))
    *)
    | _ -> ()
  in
  (* body *)
  List.iter
    (function
      | GFunDecl (id, ts, rt, _) -> add_fun_to_namespace id ts rt
      | _ ->
        ()
        (*
    | GValDecl ((id, t), _) -> add_val_to_namespace id t
    | GTypeDecl (id, t) -> add_type_to_namespace id t
    *))
    ds;
  List.iter check_gdecl ds
;;
