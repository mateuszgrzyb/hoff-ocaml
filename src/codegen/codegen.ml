
open Ast
open Funname
open Misc


let generate (name: string) (ds: g_decl_t list) =
  let context = Llvm.global_context () in
  let module_ = Llvm.create_module context name in
  let builder = Llvm.builder context in 


  let inline_attr = Llvm.create_enum_attr context "alwaysinline" 0L in
 
  (*let unit_t = Llvm.void_type context in*)
  let int_t = Llvm.i16_type context in
  let bool_t = Llvm.i1_type context in
  let float_t = Llvm.float_type context in
  let string_t = Llvm.pointer_type (Llvm.i8_type context) in
  let function_t (ts: Llvm.lltype list) (rt: Llvm.lltype): Llvm.lltype = Llvm.function_type rt (Array.of_list ts) in

  let local_functions = Hashtbl.create 10 in 
  let local_values =    Hashtbl.create 10 in 
  (*
  let global_values =   Hashtbl.create 10 in
  *)

  let lambda_name = new fun_name "HOFF_LAMBDA_" in
  let local_name = new fun_name "HOFF_LOCAL_" in 
  let global_name = "HOFF_GLOBAL_" in
  
  let rec get_llvm_t (t: type_t): Llvm.lltype = match t with
    | IntT ->          int_t
    | BoolT ->         bool_t
    | FloatT ->        float_t
    | StringT ->       string_t
    | FunT (ts, rt) -> function_t (List.map get_llvm_t ts) (get_llvm_t rt)
    | UserT _ ->       failwith "UserT Not Implemented"

  (*
  and get_type (lt: Llvm.lltype): type_t = 
    if      lt = int_t    then IntT
    else if lt = bool_t   then BoolT
    else if lt = float_t  then FloatT
    else if lt = string_t then StringT
    else failwith "Unknown llvm type"
  *)

  and generate_generic_funpredecl (name: id_t) (args: typed_id_t list) (result: type_t) =
    (* convert function types to first class function pointers *)
    let arg_types = Array.of_list (List.map (fun (_, t) -> 
      match t with
      | FunT _ -> Llvm.pointer_type (get_llvm_t t)
      | _ -> get_llvm_t t
    ) args) in

    let f_t = Llvm.function_type (get_llvm_t result) arg_types in
    Llvm.declare_function name f_t module_ 

  and generate_generic_valpredecl (name: id_t) (type_: type_t) =
    let f = generate_generic_funpredecl (global_name ^ name) [] type_ in
    Llvm.add_function_attr f inline_attr Llvm.AttrIndex.Function
  
  and generate_generic_fundecl (name: id_t) (args: typed_id_t list) (result: type_t) (body: expr_t) =
    (* convert function types to first class function pointers *)
    let arg_types = Array.of_list (List.map (fun (_, t) -> 
      match t with
      | FunT _ -> Llvm.pointer_type (get_llvm_t t)
      | _ -> get_llvm_t t
    ) args) in

    let f_t = Llvm.function_type (get_llvm_t result) arg_types in
    let f = Llvm.declare_function name f_t module_ in 
   
    (*
    if not public 
    then Llvm.set_visibility Llvm.Visibility.Hidden f 
    *)

    List.iter (fun ((name, type_), arg) ->
      Llvm.set_value_name name arg;
      match type_ with
      | FunT _ -> Hashtbl.add local_functions name arg
      | _ -> Hashtbl.add local_values name arg
    ) (zip args (Array.to_list (Llvm.params f)));

    let bb = Llvm.append_block context "entry" f in
    Llvm.position_at_end bb builder;    
    let llvm_body = generate_expr body in

    ignore (Llvm.build_ret llvm_body builder);
    
    List.iter (fun (name, type_) -> match type_ with
      | FunT _ -> Hashtbl.remove local_functions name
      | _ -> Hashtbl.remove local_values name
    ) args;

    f

  and generate_g_decl = function 
    | GValDecl ((name, type_), expr) -> (match expr with
      | Lit _ -> ()
      | _ -> generate_g_valdecl name type_ expr)
    | GFunDecl (name, args, return, body) -> generate_g_fundecl name args return body
    | GTypeDecl (name, type_) -> generate_g_typedecl name type_

  and generate_g_valdecl name type_ expr = 
    ignore (generate_generic_fundecl (global_name ^ name) [] type_ expr)

  and generate_g_fundecl name args result body =
    ignore (generate_generic_fundecl name args result body)

  and generate_g_typedecl name type_ = 
    match type_ with
    | Alias _ -> ()
    | Sum (prod :: prods) -> 
      let t = generate_prod prod in
      ignore (List.map generate_prod prods);
      let s = Llvm.named_struct_type context name in
      Llvm.struct_set_body s [|t|] false
    | Sum [] -> failwith "Sum type error"

  and generate_prod (prod: prod_t) : Llvm.lltype = 
      let (n, ts) = match prod with
      | Empty n -> (n, [||])
      | Product (n, ts) -> (n, Array.of_list (List.map generate_type ts)) in
      let s = Llvm.named_struct_type context n in
      Llvm.struct_set_body s ts false;
      s
  
  and generate_type t = get_llvm_t t

  and generate_expr = function
    | If (bexpr, expr1, expr2) -> generate_if bexpr expr1 expr2
    | Let (decls, expr) ->        generate_let decls expr
    | BinOp (lh, op, rh) ->       generate_binop lh op rh
    | UnOp (op, expr) ->          generate_unop op expr
    | ConvOp (expr, type_) ->     generate_convop expr type_
    | Val (name) ->               generate_val name
    | Fun (name, args) ->         generate_fun name args
    | Lit (lit) ->                generate_lit lit

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
    let result = [(llvm_expr1, new_then_bb); (llvm_expr2, new_else_bb)] in
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
    
    let let_bb = Llvm.insertion_block builder in
    
    List.iter (fun decl -> match decl with
      | ValDecl ((name, _), expr) -> Hashtbl.add local_values name (generate_expr expr)
      | FunDecl (name, args, result, body) -> Hashtbl.add local_functions name (generate_fundecl args result body)
    ) decls;

    Llvm.position_at_end let_bb builder;
    let llvm_expr = generate_expr expr in

    List.iter (fun decl -> match decl with
      | ValDecl ((name, _), _) -> Hashtbl.remove local_values name
      | FunDecl (name, _, _, _) -> Hashtbl.remove local_functions name
    ) decls;

    llvm_expr
  
  and generate_binop lh op rh = 
    let lh_value = generate_expr lh in
    let rh_value = generate_expr rh in
    let lt = Llvm.type_of lh_value in
    let rt = Llvm.type_of rh_value in

    let generate_int_binop l op r = match op with
    | Add -> Llvm.build_add  l r "addexpr" builder 
    | Sub -> Llvm.build_sub  l r "subexpr" builder
    | Mul -> Llvm.build_mul  l r "mulexpr" builder
    | Div -> Llvm.build_sdiv l r "divexpr" builder
    | Rem -> Llvm.build_srem l r "remexpr" builder
    | Lt  -> Llvm.build_icmp Llvm.Icmp.Slt l r "ltexpr" builder 
    | Le  -> Llvm.build_icmp Llvm.Icmp.Sle l r "leexpr" builder
    | Ge  -> Llvm.build_icmp Llvm.Icmp.Sge l r "geexpr" builder
    | Gt  -> Llvm.build_icmp Llvm.Icmp.Sgt l r "gtexpr" builder
    | Eq  -> Llvm.build_icmp Llvm.Icmp.Eq lh_value rh_value "eqexpr" builder
    | Ne  -> Llvm.build_icmp Llvm.Icmp.Ne lh_value rh_value "neexpr" builder in

    let generate_float_binop l op r = match op with
    | Add -> Llvm.build_fadd l r "addexpr" builder 
    | Sub -> Llvm.build_fsub l r "subexpr" builder
    | Mul -> Llvm.build_fmul l r "mulexpr" builder
    | Div -> Llvm.build_fdiv l r "divexpr" builder
    | Lt  -> Llvm.build_fcmp Llvm.Fcmp.Olt l r "ltexpr" builder 
    | Le  -> Llvm.build_fcmp Llvm.Fcmp.Ole l r "leexpr" builder
    | Ge  -> Llvm.build_fcmp Llvm.Fcmp.Oge l r "geexpr" builder
    | Gt  -> Llvm.build_fcmp Llvm.Fcmp.Ogt l r "gtexpr" builder
    | Eq  -> Llvm.build_fcmp Llvm.Fcmp.Oeq lh_value rh_value "eqexpr" builder
    | Ne  -> Llvm.build_fcmp Llvm.Fcmp.One lh_value rh_value "neexpr" builder
    | _ -> failwith "float binop error" in
    
    let generate_bool_binop l op r = match op with
    | Eq  -> Llvm.build_fcmp Llvm.Fcmp.Oeq l r "eqexpr" builder
    | Ne  -> Llvm.build_fcmp Llvm.Fcmp.One l r "neexpr" builder
    | _ -> failwith "bool binop error" in

    if lt = int_t        then generate_int_binop lh_value op rh_value
    else if lt = bool_t  then generate_bool_binop lh_value op rh_value
    else if lt = float_t then generate_float_binop lh_value op rh_value
    else failwith ("binop type error in llvm: " ^ (Llvm.string_of_lltype lt) ^ " " ^ (Llvm.string_of_lltype rt))
    
  and generate_unop (_: unop_t) (_: expr_t) = failwith "NotImplemented"
  
  and generate_convop (_: expr_t) (_: type_t) = failwith "NotImplemented"
  
  and generate_fundecl args result body =
    generate_generic_fundecl (local_name#generate) args result body
  
  and generate_lambda args result body = 
      let bb = Llvm.insertion_block builder in
      let f = generate_generic_fundecl (lambda_name#generate) args result body in
      ignore (Llvm.position_at_end bb builder);
      f


  and generate_val name = 
    match (Llvm.lookup_global name module_) with
    | Some g -> Llvm.build_load g "globalvalue" builder
    | None -> (match (Llvm.lookup_function (global_name ^ name) module_) with
      | Some g -> Llvm.build_call g [||] "globalvalue" builder
      | None -> Hashtbl.find local_values name)
    (*
    match (Llvm.lookup_global name module_) with
    | Some g -> g 
    | None -> Hashtbl.find local_values name
    *)

  and generate_fun name args =
    let f = match (Llvm.lookup_function name module_) with 
    | Some f -> f 
    | None -> Hashtbl.find local_functions name in
    
    let llvm_args = Array.of_list (List.map generate_expr args) in
    Llvm.build_call f llvm_args "callexpr" builder

  and generate_lit (lit: lit_t) = match lit with
    | Int i -> Llvm.const_int (get_llvm_t IntT) i
    | Bool b -> Llvm.const_int (get_llvm_t BoolT) (if b then 1 else 0)
    | Float f -> Llvm.const_float (get_llvm_t IntT) f
    | String s -> Llvm.const_stringz context s
    | Lambda (args, result, body) -> generate_lambda args result body


  (* misc *)

  and global_predeclare (d: g_decl_t): unit = match d with
    | GFunDecl (id, ts, rt, _) -> ignore (generate_generic_funpredecl id ts rt)
    | GValDecl ((id, t), e) -> (match e with
        | Lit lit -> ignore (Llvm.define_global id (generate_lit lit) module_)
        | _ -> ignore (generate_generic_valpredecl id t))
    | _ -> ()
    (*
    | GTypeDecl (_, _) -> ()
    *)
  
  (*
  and predeclare (d: decl_t): unit = match d with
    | FunDecl (id, ts, rt, e) -> Hashtbl.add local_functions id (generate_fundecl ts rt e)
    | ValDecl ((id, _), e) -> Hashtbl.add local_variables id (generate_expr e)

  *)

  in

  ignore (Llvm.declare_function "read_int" (Llvm.function_type int_t [||]) module_);
  ignore (Llvm.declare_function "read_bool" (Llvm.function_type bool_t [||]) module_);
  ignore (Llvm.declare_function "read_float" (Llvm.function_type float_t [||]) module_);

  ignore (Llvm.declare_function "print_int" (Llvm.function_type int_t [|int_t|]) module_);
  ignore (Llvm.declare_function "print_bool" (Llvm.function_type int_t [|bool_t|]) module_);
  ignore (Llvm.declare_function "print_float" (Llvm.function_type int_t [|float_t|]) module_);

  Llvm.set_target_triple "x86_64-pc-linux-gnu" module_;

  List.iter global_predeclare ds;
  List.iter generate_g_decl ds;

  (*
  match Llvm_analysis.verify_module module_ with
  | None -> Llvm.string_of_llmodule module_
  | Some error -> error
  
  *)
  Llvm.string_of_llmodule module_