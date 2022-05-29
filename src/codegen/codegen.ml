open Ast
open Funname
open Misc

type types_t =
  { int : Llvm.lltype
  ; bool : Llvm.lltype
  ; float : Llvm.lltype
  ; string : Llvm.lltype
  ; fn : Llvm.lltype list -> Llvm.lltype -> Llvm.lltype
  }

type namespace_t =
  { functions : (id_t, Llvm.llvalue) Hashtbl.t
  ; values : (id_t, Llvm.llvalue) Hashtbl.t
  ; types : (id_t, Llvm.lltype) Hashtbl.t
  ; record_names : (Llvm.lltype, id_t list) Hashtbl.t
  ; union_types : (Llvm.lltype, (Llvm.llvalue * Llvm.lltype) list) Hashtbl.t
  }

type attrs_t = { inline : Llvm.llattribute }

type context_t =
  { c : Llvm.llcontext
  ; b : Llvm.llbuilder
  ; mutable is_local : bool
  ; mutable parent_name : string list
  ; types : types_t
  ; attrs : attrs_t
  ; namespace : namespace_t
  }

let global_name = "$HOFF_GLOBAL_"
let lambda_name = new fun_name "$HOFF_LAMBDA_"

let get_local_name (c : context_t) (name : id_t) : id_t =
  name :: c.parent_name |> List.rev |> String.concat "$" |> fun s -> "$" ^ s ^ "$"
;;

let rec get_llvm_t (c : context_t) (t : type_t) : Llvm.lltype =
  match t with
  | IntT -> c.types.int
  | BoolT -> c.types.bool
  | FloatT -> c.types.float
  | StringT -> c.types.string
  | FunT (ts, rt) -> c.types.fn (List.map (get_llvm_t c) ts) (get_llvm_t c rt)
  | UserT t ->
    (match Hashtbl.find_opt c.namespace.types t with
    | None -> failwith "type not found"
    | Some t -> Llvm.pointer_type t)
;;

let initialize () : context_t =
  let c = Llvm.global_context () in
  let int_t = Llvm.i64_type c in
  let bool_t = Llvm.i1_type c in
  let float_t = Llvm.double_type c in
  let string_t = Llvm.pointer_type (Llvm.i8_type c) in
  let function_t (ts : Llvm.lltype list) (rt : Llvm.lltype) : Llvm.lltype =
    Llvm.function_type rt (Array.of_list ts) |> Llvm.pointer_type
  in
  { c
  ; b = Llvm.builder c
  ; is_local = false
  ; parent_name = []
  ; types =
      { int = int_t; bool = bool_t; float = float_t; string = string_t; fn = function_t }
  ; namespace =
      { functions = Hashtbl.create 10
      ; values = Hashtbl.create 10
      ; types = Hashtbl.create 10
      ; record_names = Hashtbl.create 10
      ; union_types = Hashtbl.create 10
      }
  ; attrs = { inline = Llvm.create_enum_attr c "alwaysinline" 0L }
  }
;;

let predeclare_stdlib (c : context_t) (m : Llvm.llmodule) : unit =
  let predeclare (name : string) (args : Llvm.lltype array) (rt : Llvm.lltype) =
    ignore (Llvm.declare_function name (Llvm.function_type rt args) m)
  in
  List.iter
    (fun (name, args, rt) -> predeclare name args rt)
    [ "read_int", [||], c.types.int
    ; "read_bool", [||], c.types.bool
    ; "read_float", [||], c.types.float
    ; "read_string", [||], c.types.string
    ; "print_int", [| c.types.int |], c.types.int
    ; "print_bool", [| c.types.bool |], c.types.int
    ; "print_float", [| c.types.float |], c.types.int
    ; "print_string", [| c.types.string |], c.types.int
    ; "GC_malloc", [| Llvm.i32_type c.c |], c.c |> Llvm.i8_type |> Llvm.pointer_type
    ]
;;

let rec generate_module_predecl (c : context_t) (m : Llvm.llmodule) (ds : g_decl_t list)
    : unit
  =
  predeclare_stdlib c m;
  Llvm.set_target_triple (Llvm_target.Target.default_triple ()) m;
  List.iter (generate_g_predecl c m) ds

and generate_module (c : context_t) (m : Llvm.llmodule) (ds : g_decl_t list) : unit =
  List.iter (generate_g_decl c m) ds;
  match Llvm.lookup_function "malloc" m with
  | None -> ()
  | Some malloc ->
    Llvm.replace_all_uses_with malloc (Llvm.lookup_function "GC_malloc" m |> Option.get);
    (match Llvm_analysis.verify_module m with
    | None -> ()
    | Some error -> Errors.LLVMError error |> raise)

and generate_generic_funpredecl
    (c : context_t)
    (m : Llvm.llmodule)
    (name : id_t)
    (args : typed_id_t list)
    (result : type_t)
    : Llvm.llvalue
  =
  let arg_types =
    args |> List.map (fun tid -> tid |> snd |> get_llvm_t c) |> Array.of_list
  in
  let r_type = get_llvm_t c result in
  let f_t = Llvm.function_type r_type arg_types in
  Llvm.declare_function name f_t m

and generate_generic_fundecl
    (c : context_t)
    (m : Llvm.llmodule)
    (f : Llvm.llvalue)
    (name : id_t)
    (args : typed_id_t list)
    (body : expr_t)
  =
  List.iter
    (fun ((name, type_), arg) ->
      Llvm.set_value_name name arg;
      match type_ with
      | FunT _ -> Hashtbl.add c.namespace.functions name arg
      | _ -> Hashtbl.add c.namespace.values name arg)
    (zip args (Array.to_list (Llvm.params f)));
  let bb = Llvm.append_block c.c "entry" f in
  Llvm.position_at_end bb c.b;
  c.parent_name <- name :: c.parent_name;
  let llvm_body =
    if c.is_local
    then generate_expr c m body
    else (
      c.is_local <- true;
      let result = generate_expr c m body in
      c.is_local <- false;
      result)
  in
  c.parent_name <- List.tl c.parent_name;
  ignore (Llvm.build_ret llvm_body c.b);
  List.iter
    (fun (name, type_) ->
      match type_ with
      | FunT _ -> Hashtbl.remove c.namespace.functions name
      | _ -> Hashtbl.remove c.namespace.values name)
    args;
  f

and generate_g_predecl (c : context_t) (m : Llvm.llmodule) (d : g_decl_t) : unit =
  (* fun *)
  let generate_g_funpredecl (name : id_t) (args : typed_id_t list) (result : type_t)
      : unit
    =
    let f = generate_generic_funpredecl c m name args result in
    Hashtbl.add c.namespace.functions name f
  in
  (* val *)
  let generate_g_valpredecl (name : id_t) (type_ : type_t) (expr : expr_t) : unit =
    let name, args, result, inline =
      match expr with
      | Lit (Lambda (args, result, _)) -> name, args, result, false
      | _ -> global_name ^ name, [], type_, true
    in
    let f = generate_generic_funpredecl c m name args result in
    if inline then Llvm.add_function_attr f c.attrs.inline Llvm.AttrIndex.Function
  in
  (* type *)
  let generate_g_typepredecl (name : id_t) type_ =
    let s = Llvm.named_struct_type c.c name in
    Hashtbl.add c.namespace.types name s;
    match type_ with
    | Alias _ -> ()
    | Record types ->
      let llvm_types =
        types |> List.map snd |> List.map (get_llvm_t c) |> Array.of_list
      in
      Llvm.struct_set_body s llvm_types false;
      Hashtbl.add c.namespace.record_names s (types |> List.map fst)
    | Union types ->
      let llvm_types = types |> List.map (get_llvm_t c) in
      Llvm.struct_set_body s [| Llvm.i16_type c.c; llvm_types |> List.hd |] false;
      Hashtbl.add
        c.namespace.union_types
        s
        (llvm_types |> List.mapi (fun i t -> Llvm.const_int (Llvm.i16_type c.c) i, t))
  in
  (* main *)
  match d with
  | Import _ -> ()
  | GFunDecl (id, ts, rt, _) -> generate_g_funpredecl id ts rt
  | GValDecl ((id, t), e) ->
    (match e with
    | Lit ((Int _ | Bool _ | Float _) as lit) ->
      ignore (Llvm.define_global id (generate_lit c m lit) m)
    | _ -> ignore (generate_g_valpredecl id t e))
  | GTypeDecl (id, type_) -> generate_g_typepredecl id type_

and generate_g_decl (c : context_t) (m : Llvm.llmodule) (d : g_decl_t) : unit =
  (* val *)
  let generate_g_valdecl name type_ expr =
    (* if value is lambda, inline it at compile time *)
    let name, args, result, expr =
      match expr with
      | Lit (Lambda (args, result, body)) -> name, args, result, body
      | _ -> global_name ^ name, [], type_, expr
    in
    let f = generate_generic_funpredecl c m name args result in
    ignore (generate_generic_fundecl c m f name args expr)
  in
  (* fun *)
  let generate_g_fundecl name args _result body =
    (*
    let f = generate_generic_funpredecl c m name args result in
       *)
    let f = Hashtbl.find c.namespace.functions name in
    ignore (generate_generic_fundecl c m f name args body)
  in
  (* main *)
  match d with
  | Import _ -> ()
  | GTypeDecl _ -> ()
  | GFunDecl (name, args, return, body) -> generate_g_fundecl name args return body
  | GValDecl ((name, type_), expr) ->
    (match expr with
    | Lit (Int _) | Lit (Bool _) | Lit (Float _) -> ()
    | _ -> generate_g_valdecl name type_ expr)

and generate_expr (c : context_t) (m : Llvm.llmodule) (expr : expr_t) : Llvm.llvalue =
  match expr with
  | If (bexpr, expr1, expr2) -> generate_if c m bexpr expr1 expr2
  | Case (expr, pms) -> generate_case c m expr pms
  | Let (decls, expr) -> generate_let c m decls expr
  | BinOp (lh, op, rh) -> generate_binop c m lh op rh
  | UnOp (op, expr) -> generate_unop c m op expr
  | ConvOp (expr, type_) -> generate_convop c m expr type_
  | ChainOp (e1, e2) -> generate_chainop c m e1 e2
  | GetOp (e, i) -> generate_getop c m e i
  | NamedGetOp (e, n) -> generate_namedgetop c m e n
  | Val name -> generate_val c m name
  | Fun (name, args) -> generate_fun c m name args
  | Lit lit -> generate_lit c m lit

and generate_if
    (c : context_t)
    (m : Llvm.llmodule)
    (bexpr : expr_t)
    (expr1 : expr_t)
    (expr2 : expr_t)
    : Llvm.llvalue
  =
  (* if block *)
  let if_bb = Llvm.insertion_block c.b in
  let f = Llvm.block_parent if_bb in
  let llvm_bexpr_bool = generate_expr c m bexpr in
  (* then block *)
  let then_bb = Llvm.append_block c.c "thenblock" f in
  Llvm.position_at_end then_bb c.b;
  let llvm_expr1 = generate_expr c m expr1 in
  let new_then_bb = Llvm.insertion_block c.b in
  (* else block *)
  let else_bb = Llvm.append_block c.c "elseblock" f in
  Llvm.position_at_end else_bb c.b;
  let llvm_expr2 = generate_expr c m expr2 in
  let new_else_bb = Llvm.insertion_block c.b in
  (* fi block *)
  let fi_bb = Llvm.append_block c.c "fiblock" f in
  Llvm.position_at_end fi_bb c.b;
  let result = [ llvm_expr1, new_then_bb; llvm_expr2, new_else_bb ] in
  let phi = Llvm.build_phi result "phi" c.b in
  (* fix blocks position *)
  Llvm.position_at_end if_bb c.b;
  ignore (Llvm.build_cond_br llvm_bexpr_bool then_bb else_bb c.b);
  Llvm.position_at_end new_then_bb c.b;
  ignore (Llvm.build_br fi_bb c.b);
  Llvm.position_at_end new_else_bb c.b;
  ignore (Llvm.build_br fi_bb c.b);
  Llvm.position_at_end fi_bb c.b;
  (* return *)
  phi

and generate_case (c : context_t) (m : Llvm.llmodule) (expr : expr_t) (_pms : pm_t list)
    : Llvm.llvalue
  =
  let v = generate_expr c m expr in
  let t = v |> Llvm.type_of |> Llvm.element_type in
  let union_types = Hashtbl.find c.namespace.union_types t in
  let union_tag = Llvm.build_struct_gep v 0 "geptag" c.b in
  let _pattern = union_types |> List.find (fun (tag, _) -> tag = union_tag) |> snd in
  failwith "a"
(*
  let m = pms |> List.find (fun (p, _) -> pattern = get_llvm_t c p) |> snd in
  generate_expr c m
     *)

and generate_let (c : context_t) (m : Llvm.llmodule) (decls : decl_t list) (expr : expr_t)
    : Llvm.llvalue
  =
  (* predeclare *)
  let predeclare (d : decl_t) : unit =
    match d with
    | FunDecl (name, ts, rt, _) ->
      Hashtbl.add
        c.namespace.functions
        name
        (generate_generic_funpredecl c m (get_local_name c name) ts rt)
    | ValDecl _ -> ()
  in
  (* declare *)
  let declare (d : decl_t) : unit =
    match d with
    | ValDecl ((name, _), expr) ->
      Hashtbl.add c.namespace.values name (generate_expr c m expr)
    | FunDecl (name, args, _, body) ->
      ignore
        (generate_generic_fundecl
           c
           m
           (Hashtbl.find c.namespace.functions name)
           name
           args
           body)
  in
  (* remove decl *)
  let remove_decl (d : decl_t) : unit =
    match d with
    | ValDecl ((name, _), _) -> Hashtbl.remove c.namespace.values name
    | FunDecl (name, _, _, _) -> Hashtbl.remove c.namespace.functions name
  in
  (* main *)
  let let_bb = Llvm.insertion_block c.b in
  List.iter predeclare decls;
  List.iter declare decls;
  Llvm.position_at_end let_bb c.b;
  let llvm_expr = generate_expr c m expr in
  List.iter remove_decl decls;
  llvm_expr

and generate_binop
    (c : context_t)
    (m : Llvm.llmodule)
    (lh : expr_t)
    (op : binop_t)
    (rh : expr_t)
    : Llvm.llvalue
  =
  let lh_value = generate_expr c m lh in
  let rh_value = generate_expr c m rh in
  let lt = Llvm.type_of lh_value in
  let rt = Llvm.type_of rh_value in
  (* int *)
  let generate_int_binop l op r =
    match op with
    | Add -> Llvm.build_add l r "addexpr" c.b
    | Sub -> Llvm.build_sub l r "subexpr" c.b
    | Mul -> Llvm.build_mul l r "mulexpr" c.b
    | Div -> Llvm.build_sdiv l r "divexpr" c.b
    | Rem -> Llvm.build_srem l r "remexpr" c.b
    | Lt -> Llvm.build_icmp Llvm.Icmp.Slt l r "ltexpr" c.b
    | Le -> Llvm.build_icmp Llvm.Icmp.Sle l r "leexpr" c.b
    | Ge -> Llvm.build_icmp Llvm.Icmp.Sge l r "geexpr" c.b
    | Gt -> Llvm.build_icmp Llvm.Icmp.Sgt l r "gtexpr" c.b
    | Eq -> Llvm.build_icmp Llvm.Icmp.Eq lh_value rh_value "eqexpr" c.b
    | Ne -> Llvm.build_icmp Llvm.Icmp.Ne lh_value rh_value "neexpr" c.b
    | _ -> failwith "int binop error"
  in
  (* float *)
  let generate_float_binop l op r =
    match op with
    | Add -> Llvm.build_fadd l r "addexpr" c.b
    | Sub -> Llvm.build_fsub l r "subexpr" c.b
    | Mul -> Llvm.build_fmul l r "mulexpr" c.b
    | Div -> Llvm.build_fdiv l r "divexpr" c.b
    | Lt -> Llvm.build_fcmp Llvm.Fcmp.Olt l r "ltexpr" c.b
    | Le -> Llvm.build_fcmp Llvm.Fcmp.Ole l r "leexpr" c.b
    | Ge -> Llvm.build_fcmp Llvm.Fcmp.Oge l r "geexpr" c.b
    | Gt -> Llvm.build_fcmp Llvm.Fcmp.Ogt l r "gtexpr" c.b
    | Eq -> Llvm.build_fcmp Llvm.Fcmp.Oeq l r "eqexpr" c.b
    | Ne -> Llvm.build_fcmp Llvm.Fcmp.One l r "neexpr" c.b
    | _ -> failwith "float binop error"
  in
  (* bool *)
  let generate_bool_binop l op r =
    match op with
    | And -> Llvm.build_and l r "andexpr" c.b
    | Or -> Llvm.build_or l r "orexpr" c.b
    | Eq -> Llvm.build_fcmp Llvm.Fcmp.Oeq l r "eqexpr" c.b
    | Ne -> Llvm.build_fcmp Llvm.Fcmp.One l r "neexpr" c.b
    | _ -> failwith "bool binop error"
  in
  (* main *)
  if lt = c.types.int
  then generate_int_binop lh_value op rh_value
  else if lt = c.types.bool
  then generate_bool_binop lh_value op rh_value
  else if lt = c.types.float
  then generate_float_binop lh_value op rh_value
  else
    failwith
      ("binop type error in llvm: "
      ^ Llvm.string_of_lltype lt
      ^ " "
      ^ Llvm.string_of_lltype rt)

and generate_unop (_c : context_t) (_m : Llvm.llmodule) (_ : unop_t) (_ : expr_t) =
  failwith "NotImplemented"

and generate_convop (_c : context_t) (_m : Llvm.llmodule) (expr : expr_t) (t : type_t)
    : Llvm.llvalue
  =
  let conv_to_int (_expr : expr_t) : Llvm.llvalue = failwith "Not Implemented" in
  match t with
  | IntT -> conv_to_int expr
  | FloatT -> failwith ""
  | BoolT -> failwith ""
  | StringT -> failwith ""
  | FunT _ -> failwith ""
  | UserT _ -> failwith ""

and generate_chainop (c : context_t) (m : Llvm.llmodule) (e1 : expr_t) (e2 : expr_t) =
  ignore (generate_expr c m e1);
  generate_expr c m e2

and generate_getop (c : context_t) (m : Llvm.llmodule) (expr : expr_t) (i : int) =
  let s = generate_expr c m expr in
  let t = s |> Llvm.type_of |> Llvm.element_type in
  let ptr = Llvm.build_struct_gep s i "gep" c.b in
  match
    t
    |> Llvm.struct_element_types
    |> Array.to_list
    |> fun l -> List.nth l i |> Llvm.classify_type
  with
  | Llvm.TypeKind.Struct | Llvm.TypeKind.Function -> ptr
  | _ -> Llvm.build_load ptr "load" c.b

and generate_namedgetop (c : context_t) (m : Llvm.llmodule) (expr : expr_t) (name : id_t)
    : Llvm.llvalue
  =
  let s = generate_expr c m expr in
  let t = s |> Llvm.type_of |> Llvm.element_type in
  let names = Hashtbl.find c.namespace.record_names t in
  let i = names |> Misc.find_index name in
  let ptr = Llvm.build_struct_gep s i "gep" c.b in
  match
    t
    |> Llvm.struct_element_types
    |> Array.to_list
    |> fun l -> List.nth l i |> Llvm.classify_type
  with
  | Llvm.TypeKind.Struct | Llvm.TypeKind.Function -> ptr
  | _ -> Llvm.build_load ptr "load" c.b

and generate_val (c : context_t) (m : Llvm.llmodule) (name : id_t) =
  match Llvm.lookup_global name m with
  | Some g -> Llvm.build_load g "globalvalue" c.b
  | None ->
    (match Llvm.lookup_function (global_name ^ name) m with
    | Some g -> Llvm.build_call g [||] "globalvalue" c.b
    | None ->
      let opt_v = Hashtbl.find_opt c.namespace.values name in
      if Option.is_some opt_v
      then Option.get opt_v
      else (
        match Llvm.lookup_function name m with
        | Some f -> f
        | None ->
          (match Hashtbl.find_opt c.namespace.functions name with
          | Some f -> f
          | None -> raise Not_found)))

and generate_fun (c : context_t) (m : Llvm.llmodule) (expr : expr_t) (args : expr_t list)
    : Llvm.llvalue
  =
  let f = generate_expr c m expr in
  (*
  let f =
    match Llvm.lookup_function name c.m with
    | Some f -> f
    | None -> Hashtbl.find c.namespace.functions name
  in
     *)
  let llvm_args = Array.of_list (List.map (generate_expr c m) args) in
  Llvm.build_call f llvm_args "callexpr" c.b

and generate_lit (c : context_t) (m : Llvm.llmodule) (lit : lit_t) : Llvm.llvalue =
  let generate_string s = Llvm.build_global_stringptr s "string" c.b in
  let generate_struct (id : id_t) (vals : expr_t list) : Llvm.llvalue =
    let t = Hashtbl.find c.namespace.types id in
    let s = Llvm.build_malloc t "malloc" c.b in
    List.iteri
      (fun i val_ ->
        let val_ = generate_expr c m val_ in
        let p = Llvm.build_struct_gep s i "gep" c.b in
        ignore (Llvm.build_store val_ p c.b))
      vals;
    s
  in
  let generate_namedstruct (id : id_t) (named_vals : named_expr_t list) : Llvm.llvalue =
    let t = Hashtbl.find c.namespace.types id in
    let names = Hashtbl.find c.namespace.record_names t in
    let s = Llvm.build_malloc t "malloc" c.b in
    List.iter
      (fun (name, expr) ->
        let v = generate_expr c m expr in
        let i = Misc.find_index name names in
        let p = Llvm.build_struct_gep s i "gep" c.b in
        ignore (Llvm.build_store v p c.b))
      named_vals;
    s
  in
  match lit with
  | Int i -> Llvm.const_int (get_llvm_t c IntT) i
  | Bool b -> Llvm.const_int (get_llvm_t c BoolT) (if b then 1 else 0)
  | Float f -> Llvm.const_float (get_llvm_t c FloatT) f
  | String s -> generate_string s
  | Struct (id, vals) -> generate_struct id vals
  | NamedStruct (id, named_vals) -> generate_namedstruct id named_vals
  | Lambda (args, result, body) -> generate_lambda c m args result body

and generate_lambda
    (c : context_t)
    (m : Llvm.llmodule)
    (args : typed_id_t list)
    (result : type_t)
    (body : expr_t)
    : Llvm.llvalue
  =
  let bb = Llvm.insertion_block c.b in
  let name = lambda_name#generate in
  let f = generate_generic_funpredecl c m name args result in
  let f = generate_generic_fundecl c m f name args body in
  ignore (Llvm.position_at_end bb c.b);
  f
;;
