
let rec predeclare (c: Misc.context_t) (g_decl: Ast.g_decl_t): unit =
  match g_decl with
  | GConstDecl (name, type_, _) ->
      c.names#add name 
      { t = type_
      ; v = Llvm.declare_global (Misc.get_llvm_type c type_) name c.m
      }
    
  | GFunDecl (name, _, types, _) -> 
      c.names#add name 
      { t = FunT types
      ; v = Llvm.declare_global (Misc.get_llvm_type c (FunT types)) name c.m
      }

  | GTypeDecl (name, _) -> 
    c.types#add name (Llvm.named_struct_type c.c name)

and generate (c: Misc.context_t) (g_decl: Ast.g_decl_t): unit =
  match g_decl with
  | GConstDecl (name, type_, expr) -> _generate_gconstdecl c name type_ expr
  | GFunDecl (name, argnames, types, expr) -> _generate_gfundecl c name argnames types expr
  | GTypeDecl (name, user_type) -> _generate_gtypedecl c name user_type

and _generate_gconstdecl (c: Misc.context_t) (name: string) (type_: Ast.type_t) (expr: Ast.expr_t): unit = 
  let expr_tv = Expr.generate c expr in
  if (Misc.compare_types c expr_tv.t type_) then raise (Errors.TypeError "Const declared type and expression type does not match")
  else ignore (Llvm.define_global name expr_tv.v c.m)

and _generate_gfundecl (c: Misc.context_t) (name: string) (argnames: string list) (types: Ast.type_t list) (body: Ast.expr_t): unit = 
  c.global <- false;
  match (Llvm.lookup_function name c.m) with 
  | Some v -> ignore (Helpers.generate_funcdef Expr.generate c v argnames types body);
  | None -> ignore (Helpers.generate_funcdecl Expr.generate c name argnames types body);
  c.global <- true;

and _generate_gtypedecl (c: Misc.context_t) (name: string) (user_type: Ast.user_type_t): unit = 
  let llvm_type = Usertype.generate c name user_type in 
  c.types#add name llvm_type

