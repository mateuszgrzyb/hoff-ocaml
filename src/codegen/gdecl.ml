
let rec predeclare (_c: Misc.context_t) (_g_decl: Ast.g_decl_t): unit = ()
and generate (c: Misc.context_t) (g_decl: Ast.g_decl_t): unit =
  match g_decl with
  | GConstDecl (name, type_, expr) -> _generate_gconstdecl c name type_ expr
  | GFunDecl (name, argnames, types, expr) -> _generate_gfundecl c name argnames types expr
  | GTypeDecl (name, constructors) -> _generate_gtypedecl c name constructors

and _generate_gconsdecl (c: Misc.context_t) (name: string) (type_: Ast.type_t) (expr: Ast.expr_t): unit = 
  ()

and _generate_gfundecl (c: Misc.context_t) (name: string) (argnames: string list) (types: Ast.type_t list) (expr: Ast.expr_t): unit = 
  ()


