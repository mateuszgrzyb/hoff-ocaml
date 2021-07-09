
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
  ()

and _generate_binop (c: Misc.context_t) (lh: Ast.expr_t) (op: Ast.binop_t) (rh: Ast.expr_t): Misc.tv_t = 
  let l_tv = generate c lh in 
  let r_tv = generate c rh in 
  ()