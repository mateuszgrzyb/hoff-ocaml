
let rec predeclare (_c: Misc.context_t) (_g_decl: Ast.g_decl_t): unit = ()
and generate (c: Misc.context_t) (g_decl: Ast.g_decl_t): unit =
  match g_decl with
  | GConstDecl (name, type_, expr) -> _generate_gconstdecl c name type_ expr
  | GFunDecl (name, argnames, types, expr) -> _generate_gfundecl c name argnames types expr
  | GTypeDecl (name, constructors) -> _generate_gtypedecl c name constructors

and _generate_gconstdecl (c: Misc.context_t) (name: string) (type_: Ast.type_t) (expr: Ast.expr_t): unit = 
  let expr_tv = Expr.generate c expr in
  if expr_tv.t <> type_ then raise (Errors.TypeError "")
  else ignore (Llvm.define_global name expr_tv.v c.m)

and _generate_gfundecl (c: Misc.context_t) (name: string) (argnames: string list) (types: Ast.type_t list) (body: Ast.expr_t): unit = 
  c.global <- false;
  ignore (Helpers.generate_funcdecl c name argnames types body);
  c.global <- true;

and _generate_gtypedecl (c: Misc.context_t) (_name: string) (constructors: Ast.constructor_t list): unit = 

  let generate_constructor (constructor: Ast.constructor_t) = 
    let (n, ts) = constructor in
    let llvm_struct = Llvm.named_struct_type c.c n in
    Llvm.struct_set_body llvm_struct (Array.of_list (List.map (Misc.get_llvm_type c) ts)) false;

    llvm_struct
  in

    let _llvm_constructors = List.map generate_constructor constructors in 
    failwith ""


