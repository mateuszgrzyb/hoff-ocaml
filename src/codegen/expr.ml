
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

and _generate_if (_c: Misc.context_t) (_bexpr: Ast.expr_t) (_expr1: Ast.expr_t) (_expr2: Ast.expr_t): Misc.tv_t = 
  failwith ""

and _generate_let (c: Misc.context_t) (decls: Ast.decl_t list) (expr: Ast.expr_t): Misc.tv_t = 
  let llvm_bb = Llvm.insertion_block c.b in 

  List.iter (Decl.predeclare c) decls;
  List.iter (Decl.add c) decls;

  Llvm.position_at_end llvm_bb c.b;
  let expr_tv = generate c expr in 

  List.iter (Decl.remove c) decls;

  expr_tv

and _generate_case (_c: Misc.context_t) (_expr: Ast.expr_t) (_patterns): Misc.tv_t = 
  failwith ""

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