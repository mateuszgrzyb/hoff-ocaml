
let generate (module_: Ast.module_t): unit =
  List.iter Gdecl.predeclare module_ ;
  List.iter Gdecl.generate module_