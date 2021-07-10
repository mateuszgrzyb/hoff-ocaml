
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

and _generate_value (_c: Misc.context_t) (_name: string): Misc.tv_t = 
  failwith ""

and _generate_binop (_c: Misc.context_t) (_lh: Ast.expr_t) (_op: Ast.binop_t) (_rh: Ast.expr_t): Misc.tv_t = 
  failwith ""

and _generate_unop (_c: Misc.context_t) (_op: Ast.unop_t) (_expr: Ast.expr_t): Misc.tv_t = 
  failwith ""

and _generate_if (_c: Misc.context_t) (_bexpr: Ast.expr_t) (_expr1: Ast.expr_t) (_expr2: Ast.expr_t): Misc.tv_t = 
  failwith ""

and _generate_let (_c: Misc.context_t) (_decls: Ast.decl_t list) (_expr: Ast.expr_t): Misc.tv_t = 
  failwith ""

and _generate_case (_c: Misc.context_t) (_expr: Ast.expr_t) (_patterns): Misc.tv_t = 
  failwith ""

and _generate_call (_c: Misc.context_t) (_expr: Ast.expr_t) (_exprs: Ast.expr_t list): Misc.tv_t = 
  failwith ""