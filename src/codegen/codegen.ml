open Ast
open Funname
open Misc

let generate (name : string) (ds : g_decl_t list) =
  let context = Llvm.global_context () in
  let module_ = Llvm.create_module context name in
  let builder = Llvm.builder context in
  let is_local = ref false in
  let inline_attr = Llvm.create_enum_attr context "alwaysinline" 0L in
  (*let unit_t = Llvm.void_type context in*)
  let int_t = Llvm.i64_type context in
  let bool_t = Llvm.i1_type context in
  let float_t = Llvm.double_type context in
  let string_t = Llvm.pointer_type (Llvm.i8_type context) in
  let function_t (ts : Llvm.lltype list) (rt : Llvm.lltype) : Llvm.lltype =
    Llvm.function_type rt (Array.of_list ts) |> Llvm.pointer_type
  in
  let local_functions = Hashtbl.create 10 in
  let local_values = Hashtbl.create 10 in
  let user_types = Hashtbl.create 10 in
  let lambda_name = new fun_name "$HOFF_LAMBDA_" in
  (*
  let local_name = new fun_name "HOFF_LOCAL_" in 
  let local_name = "HOFF_LOCAL_" in
     *)
  let parent_name = ref [] in
  let get_local_name (name : id_t) : id_t =
    name :: !parent_name |> List.rev |> String.concat "$" |> fun s -> "$" ^ s ^ "$"
  in
  let global_name = "$HOFF_GLOBAL_" in
  let rec get_llvm_t (t : type_t) : Llvm.lltype =
    match t with
    | IntT -> int_t
    | BoolT -> bool_t
    | FloatT -> float_t
    | StringT -> string_t
    | FunT (ts, rt) -> function_t (List.map get_llvm_t ts) (get_llvm_t rt)
    | UserT t ->
      (match Hashtbl.find_opt user_types t with
      | None -> failwith "type not found"
      | Some t -> t)
  (*
  and get_type (lt: Llvm.lltype): type_t = 
    if      lt = int_t    then IntT
    else if lt = bool_t   then BoolT
    else if lt = float_t  then FloatT
    else if lt = string_t then StringT
    else failwith "Unknown llvm type"
  *)
  and generate_generic_funpredecl (name : id_t) (args : typed_id_t list) (result : type_t)
    =
    let arg_types =
      args |> List.map (fun tid -> tid |> snd |> get_llvm_t) |> Array.of_list
    in
    let r_type = get_llvm_t result in
    let f_t = Llvm.function_type r_type arg_types in
    Llvm.declare_function name f_t module_
  and generate_generic_valpredecl (name : id_t) (type_ : type_t) =
    let f = generate_generic_funpredecl (global_name ^ name) [] type_ in
    Llvm.add_function_attr f inline_attr Llvm.AttrIndex.Function
  and generate_generic_typepredecl (name : id_t) =
    let t = Llvm.named_struct_type context name in
    Hashtbl.add user_types name t
  and generate_generic_fundecl
      (f : Llvm.llvalue)
      (name : id_t)
      (args : typed_id_t list)
      (body : expr_t)
    =
    (*
  and generate_generic_fundecl (name: id_t) (args: typed_id_t list) (result: type_t) (body: expr_t) =
    (* convert function types to first class function pointers *)
    let arg_types = args |> List.map (fun tid -> tid |> snd |> get_llvm_t) |> Array.of_list in

    let f_t = Llvm.function_type (get_llvm_t result) arg_types in
    let f = Llvm.declare_function name f_t module_ in 
       *)

    (*
    if not public 
    then Llvm.set_visibility Llvm.Visibility.Hidden f 
    *)
    List.iter
      (fun ((name, type_), arg) ->
        Llvm.set_value_name name arg;
        match type_ with
        | FunT _ -> Hashtbl.add local_functions name arg
        | _ -> Hashtbl.add local_values name arg)
      (zip args (Array.to_list (Llvm.params f)));
    let bb = Llvm.append_block context "entry" f in
    Llvm.position_at_end bb builder;
    parent_name := name :: !parent_name;
    let llvm_body =
      if !is_local
      then generate_expr body
      else (
        is_local := true;
        let result = generate_expr body in
        is_local := false;
        result)
    in
    parent_name := List.tl !parent_name;
    ignore (Llvm.build_ret llvm_body builder);
    List.iter
      (fun (name, type_) ->
        match type_ with
        | FunT _ -> Hashtbl.remove local_functions name
        | _ -> Hashtbl.remove local_values name)
      args;
    f
  and generate_g_decl g_decl =
    match g_decl with
    | GValDecl ((name, type_), expr) ->
      (match expr with
      | Lit (Int _) | Lit (Bool _) | Lit (Float _) -> ()
      | _ -> generate_g_valdecl name type_ expr)
    | GFunDecl (name, args, return, body) -> generate_g_fundecl name args return body
    | GTypeDecl (name, type_) -> generate_g_typedecl name type_
  and generate_g_valdecl name type_ expr =
    let f = generate_generic_funpredecl (global_name ^ name) [] type_ in
    ignore (generate_generic_fundecl f name [] expr)
  and generate_g_fundecl name args result body =
    let f = generate_generic_funpredecl name args result in
    ignore (generate_generic_fundecl f name args body)
  and generate_g_typedecl name type_ =
    let find_biggest_type (types : Llvm.lltype list) : Llvm.lltype = types |> List.hd in
    (*
      types |> List.map (fun t -> t 
        |> Llvm.size_of 
        |> Llvm.string_of_llvalue 
        |> int_of_string 
        |> (fun s -> (t, s))) 
        |> List.sort (fun (_, sa) (_, sb) -> (compare sa sb) * -1) 
        |> List.hd 
        |> fst in
         *)
    match type_ with
    | Alias _ -> ()
    | Sum [] -> failwith "Sum type error"
    | Sum prods ->
      let llvm_types = prods |> List.map generate_prod in
      let biggest_type = find_biggest_type llvm_types in
      let s = Hashtbl.find user_types name in
      Llvm.struct_set_body s [| biggest_type |] false
  and generate_prod (prod : prod_t) : Llvm.lltype =
    let n, ts =
      match prod with
      | Empty n -> n, [||]
      | Product (n, ts) ->
        ( n
        , ts
          |> List.map (fun t -> t |> generate_type |> Llvm.pointer_type)
          |> Array.of_list )
    in
    let s = Llvm.named_struct_type context n in
    Llvm.struct_set_body s ts false;
    s
  and generate_type t = get_llvm_t t
  and generate_expr = function
    | If (bexpr, expr1, expr2) -> generate_if bexpr expr1 expr2
    | Let (decls, expr) -> generate_let decls expr
    | BinOp (lh, op, rh) -> generate_binop lh op rh
    | UnOp (op, expr) -> generate_unop op expr
    | ConvOp (expr, type_) -> generate_convop expr type_
    | ChainOp (e1, e2) -> generate_chainop e1 e2
    | GetOp (e, i) -> generate_getop e i
    | Val name -> generate_val name
    | Fun (name, args) -> generate_fun name args
    | Lit lit -> generate_lit lit
  and generate_if bexpr expr1 expr2 =
    let if_bb = Llvm.insertion_block builder in
    let f = Llvm.block_parent if_bb in
    let llvm_bexpr_bool = generate_expr bexpr in
    let then_bb = Llvm.append_block context "thenblock" f in
    Llvm.position_at_end then_bb builder;
    let llvm_expr1 = generate_expr expr1 in
    let new_then_bb = Llvm.insertion_block builder in
    let else_bb = Llvm.append_block context "elseblock" f in
    Llvm.position_at_end else_bb builder;
    let llvm_expr2 = generate_expr expr2 in
    let new_else_bb = Llvm.insertion_block builder in
    let fi_bb = Llvm.append_block context "fiblock" f in
    Llvm.position_at_end fi_bb builder;
    let result = [ llvm_expr1, new_then_bb; llvm_expr2, new_else_bb ] in
    let phi = Llvm.build_phi result "phi" builder in
    Llvm.position_at_end if_bb builder;
    ignore (Llvm.build_cond_br llvm_bexpr_bool then_bb else_bb builder);
    Llvm.position_at_end new_then_bb builder;
    ignore (Llvm.build_br fi_bb builder);
    Llvm.position_at_end new_else_bb builder;
    ignore (Llvm.build_br fi_bb builder);
    Llvm.position_at_end fi_bb builder;
    phi
  and generate_let decls expr =
    let predeclare (d : decl_t) : unit =
      match d with
      | FunDecl (name, ts, rt, _) ->
        Hashtbl.add
          local_functions
          name
          (generate_generic_funpredecl (get_local_name name) ts rt)
      | ValDecl _ -> ()
    in
    let declare (d : decl_t) : unit =
      match d with
      | ValDecl ((name, _), expr) -> Hashtbl.add local_values name (generate_expr expr)
      | FunDecl (name, args, _, body) ->
        ignore
          (generate_generic_fundecl (Hashtbl.find local_functions name) name args body)
    in
    let remove_decl (d : decl_t) : unit =
      match d with
      | ValDecl ((name, _), _) -> Hashtbl.remove local_values name
      | FunDecl (name, _, _, _) -> Hashtbl.remove local_functions name
    in
    let let_bb = Llvm.insertion_block builder in
    List.iter predeclare decls;
    List.iter declare decls;
    Llvm.position_at_end let_bb builder;
    let llvm_expr = generate_expr expr in
    List.iter remove_decl decls;
    llvm_expr
  and generate_binop lh op rh =
    let lh_value = generate_expr lh in
    let rh_value = generate_expr rh in
    let lt = Llvm.type_of lh_value in
    let rt = Llvm.type_of rh_value in
    let generate_int_binop l op r =
      match op with
      | Add -> Llvm.build_add l r "addexpr" builder
      | Sub -> Llvm.build_sub l r "subexpr" builder
      | Mul -> Llvm.build_mul l r "mulexpr" builder
      | Div -> Llvm.build_sdiv l r "divexpr" builder
      | Rem -> Llvm.build_srem l r "remexpr" builder
      | Lt -> Llvm.build_icmp Llvm.Icmp.Slt l r "ltexpr" builder
      | Le -> Llvm.build_icmp Llvm.Icmp.Sle l r "leexpr" builder
      | Ge -> Llvm.build_icmp Llvm.Icmp.Sge l r "geexpr" builder
      | Gt -> Llvm.build_icmp Llvm.Icmp.Sgt l r "gtexpr" builder
      | Eq -> Llvm.build_icmp Llvm.Icmp.Eq lh_value rh_value "eqexpr" builder
      | Ne -> Llvm.build_icmp Llvm.Icmp.Ne lh_value rh_value "neexpr" builder
    in
    let generate_float_binop l op r =
      match op with
      | Add -> Llvm.build_fadd l r "addexpr" builder
      | Sub -> Llvm.build_fsub l r "subexpr" builder
      | Mul -> Llvm.build_fmul l r "mulexpr" builder
      | Div -> Llvm.build_fdiv l r "divexpr" builder
      | Lt -> Llvm.build_fcmp Llvm.Fcmp.Olt l r "ltexpr" builder
      | Le -> Llvm.build_fcmp Llvm.Fcmp.Ole l r "leexpr" builder
      | Ge -> Llvm.build_fcmp Llvm.Fcmp.Oge l r "geexpr" builder
      | Gt -> Llvm.build_fcmp Llvm.Fcmp.Ogt l r "gtexpr" builder
      | Eq -> Llvm.build_fcmp Llvm.Fcmp.Oeq l r "eqexpr" builder
      | Ne -> Llvm.build_fcmp Llvm.Fcmp.One l r "neexpr" builder
      | _ -> failwith "float binop error"
    in
    let generate_bool_binop l op r =
      match op with
      | Eq -> Llvm.build_fcmp Llvm.Fcmp.Oeq l r "eqexpr" builder
      | Ne -> Llvm.build_fcmp Llvm.Fcmp.One l r "neexpr" builder
      | _ -> failwith "bool binop error"
    in
    if lt = int_t
    then generate_int_binop lh_value op rh_value
    else if lt = bool_t
    then generate_bool_binop lh_value op rh_value
    else if lt = float_t
    then generate_float_binop lh_value op rh_value
    else
      failwith
        ("binop type error in llvm: "
        ^ Llvm.string_of_lltype lt
        ^ " "
        ^ Llvm.string_of_lltype rt)
  and generate_unop (_ : unop_t) (_ : expr_t) = failwith "NotImplemented"
  and generate_convop (_ : expr_t) (_ : type_t) = failwith "NotImplemented"
  and generate_chainop (e1 : expr_t) (e2 : expr_t) =
    ignore (generate_expr e1);
    generate_expr e2
  and generate_getop _e _i = failwith "NotImplemented"
  and generate_lambda args result body =
    let bb = Llvm.insertion_block builder in
    let name = lambda_name#generate in
    let f = generate_generic_funpredecl name args result in
    let f = generate_generic_fundecl f name args body in
    ignore (Llvm.position_at_end bb builder);
    f
  and generate_val name =
    match Llvm.lookup_global name module_ with
    | Some g -> Llvm.build_load g "globalvalue" builder
    | None ->
      (match Llvm.lookup_function (global_name ^ name) module_ with
      | Some g -> Llvm.build_call g [||] "globalvalue" builder
      | None ->
        let opt_v = Hashtbl.find_opt local_values name in
        if Option.is_some opt_v
        then Option.get opt_v
        else (
          match Llvm.lookup_function name module_ with
          | Some f -> f
          | None ->
            (match Hashtbl.find_opt local_functions name with
            | Some f -> f
            | None -> raise Not_found)))
  and generate_fun name args =
    let f =
      match Llvm.lookup_function name module_ with
      | Some f -> f
      | None -> Hashtbl.find local_functions name
    in
    let llvm_args = Array.of_list (List.map generate_expr args) in
    Llvm.build_call f llvm_args "callexpr" builder
  and generate_lit (lit : lit_t) =
    let generate_string s = Llvm.build_global_stringptr s "string" builder in
    match lit with
    | Int i -> Llvm.const_int (get_llvm_t IntT) i
    | Bool b -> Llvm.const_int (get_llvm_t BoolT) (if b then 1 else 0)
    | Float f -> Llvm.const_float (get_llvm_t IntT) f
    | String s -> generate_string s
    | Lambda (args, result, body) -> generate_lambda args result body
  (* misc *)
  and global_predeclare (d : g_decl_t) : unit =
    match d with
    | GFunDecl (id, ts, rt, _) -> ignore (generate_generic_funpredecl id ts rt)
    | GValDecl ((id, t), e) ->
      (match e with
      | Lit ((Int _ | Bool _ | Float _) as lit) ->
        ignore (Llvm.define_global id (generate_lit lit) module_)
      | _ -> ignore (generate_generic_valpredecl id t))
    | GTypeDecl (id, _) -> generate_generic_typepredecl id
  in
  ignore (Llvm.declare_function "read_int" (Llvm.function_type int_t [||]) module_);
  ignore (Llvm.declare_function "read_bool" (Llvm.function_type bool_t [||]) module_);
  ignore (Llvm.declare_function "read_float" (Llvm.function_type float_t [||]) module_);
  ignore (Llvm.declare_function "read_string" (Llvm.function_type string_t [||]) module_);
  ignore
    (Llvm.declare_function "print_int" (Llvm.function_type int_t [| int_t |]) module_);
  ignore
    (Llvm.declare_function "print_bool" (Llvm.function_type int_t [| bool_t |]) module_);
  ignore
    (Llvm.declare_function "print_float" (Llvm.function_type int_t [| float_t |]) module_);
  ignore
    (Llvm.declare_function
       "print_string"
       (Llvm.function_type int_t [| string_t |])
       module_);
  (*
  Llvm.set_target_triple "x86_64-pc-linux-gnu" module_;
   *)
  Llvm.set_target_triple "arm64-apple-macosx12.0.0" module_;
  List.iter global_predeclare ds;
  List.iter generate_g_decl ds;
  (*
  match Llvm_analysis.verify_module module_ with
  | None -> Llvm.string_of_llmodule module_
  | Some error -> error
  
  *)
  Llvm.string_of_llmodule module_
;;
