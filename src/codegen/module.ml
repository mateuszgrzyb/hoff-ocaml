
let generate (c: Misc.context_t) (module_: Ast.module_t): unit =
  List.iter (Gdecl.predeclare c) module_ ;
  List.iter (Gdecl.generate c) module_