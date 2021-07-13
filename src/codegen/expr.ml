
let rec generate (c: Misc.context_t) (expr: Ast.expr_t): Misc.tv_t =
  match expr with
  | Val (name) -> _generate_value c name
  | Lit (lit) -> Lit.generate c lit
  | BinOp (lh, op, rh) -> _generate_binop c lh op rh
  | UnOp (op, expr) -> _generate_unop c op expr 
  | If (bexpr, expr1, expr2) -> _generate_if c bexpr expr1 expr2
  | Let (decls, expr) -> _generate_let c decls expr 
  | Case (expr, patterns) -> _generate_case c expr patterns 
  | Call (expr, exprs) -> _generate_call c expr exprs

and _generate_value (c: Misc.context_t) (name: string): Misc.tv_t = 
  c.names#get name

and _generate_binop (_c: Misc.context_t) (_lh: Ast.expr_t) (_op: Ast.binop_t) (_rh: Ast.expr_t): Misc.tv_t = 
  failwith ""

and _generate_unop (_c: Misc.context_t) (_op: Ast.unop_t) (_expr: Ast.expr_t): Misc.tv_t = 
  failwith ""

and _generate_if (c: Misc.context_t) (bexpr: Ast.expr_t) (expr1: Ast.expr_t) (expr2: Ast.expr_t): Misc.tv_t = 
  let if_bb = Llvm.insertion_block c.b in
  let f = Llvm.block_parent if_bb in
  let llvm_bexpr_bool = generate c bexpr in
  
  let llvm_then_bb = Llvm.append_block c.c "thenblock" f in
  Llvm.position_at_end llvm_then_bb c.b;
  let expr1_tv = generate c expr1 in 
  let llvm_new_then_bb = Llvm.insertion_block c.b in

  let llvm_else_bb = Llvm.append_block c.c "elseblock" f in
  Llvm.position_at_end llvm_else_bb c.b;
  let expr2_tv = generate c expr2 in 
  let llvm_new_else_bb = Llvm.insertion_block c.b in

  let llvm_fi_bb = Llvm.append_block c.c "fiblock" f in
  Llvm.position_at_end llvm_fi_bb c.b;
  let result = [(expr1_tv.v, llvm_new_then_bb); (expr2_tv.v, llvm_new_else_bb)] in
  let llvm_phi = Llvm.build_phi result "phi" c.b in
  
  Llvm.position_at_end if_bb c.b;
  ignore (Llvm.build_cond_br llvm_bexpr_bool.v llvm_then_bb llvm_else_bb c.b);
  
  Llvm.position_at_end llvm_new_then_bb c.b;
  ignore (Llvm.build_br llvm_fi_bb c.b);
  Llvm.position_at_end llvm_new_else_bb c.b;
  ignore (Llvm.build_br llvm_fi_bb c.b);

  Llvm.position_at_end llvm_fi_bb c.b;

  (* typecheck *)
  if Misc.compare_types c expr1_tv.t expr2_tv.t then raise (Errors.TypeError "if");

  { t = expr1_tv.t
  ; v = llvm_phi
  }


and _generate_let (c: Misc.context_t) (decls: Ast.decl_t list) (expr: Ast.expr_t): Misc.tv_t = 
  let llvm_bb = Llvm.insertion_block c.b in 

  List.iter (fun _decl -> ()) decls;
  List.iter (function 
  | Ast.ConstDecl (_name, _type_, _expr) -> ()
  | Ast.FunDecl (name, args, types, body) -> let f = Helpers.generate_funcdecl generate c (Helpers.local_name [name]) args types body in c.names#add name f
  ) decls;

  Llvm.position_at_end llvm_bb c.b;
  let expr_tv = generate c expr in 

  List.iter (function 
  | Ast.ConstDecl (_name, _type_, _expr) -> ()
  | Ast.FunDecl (name, _args, _types, _body) -> c.names#remove name
  ) decls;

  expr_tv

and _generate_case (c: Misc.context_t) (expr: Ast.expr_t) (patterns: (Ast.pattern_t * Ast.expr_t) list): Misc.tv_t = 
  let expr_tv = generate c expr in
  let _constructors = c.constructors#get expr_tv.t in
  let (pattern, match_) = List.find (fun (pattern, _) -> 
    let (name, _bound_vars) = pattern in
    match Misc.StringMap.find_opt name _constructors with 
    | None -> false 
    | Some _ -> true
  ) patterns in 

  
  let match_tv = generate c match_ in

  ignore (pattern);

  match_tv


and _generate_call (c: Misc.context_t) (expr: Ast.expr_t) (args: Ast.expr_t list): Misc.tv_t = 
  (* 
  generate call can either generate value returned from function
  or another function which is partially applied
  *)
  let rec split: Misc.tv_t list -> Llvm.llvalue list * Ast.type_t list = function
    | [] -> ([], [])
    | tv :: tvs -> 
      let (vs, ts) = split tvs in 
      (tv.v :: vs, tv.t :: ts) in

  let expr_tv = generate c expr in
  let (values, _types) = split (List.map (generate c) args) in

  (* typecheck *)
  { t = expr_tv.t
  ; v = Llvm.build_call expr_tv.v (Array.of_list values) "call" c.b
  }