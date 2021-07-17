
let generate (c: Misc.context_t) (module_: Ast.module_t): unit =
  let _, gdecls = module_ in 
  List.iter (Gdecl.predeclare c) gdecls ;
  List.iter (Gdecl.generate c) gdecls