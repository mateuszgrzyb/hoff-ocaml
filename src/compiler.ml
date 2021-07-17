
let _convert_name (name: string): string = 
  String.split_on_char '.' name 
    |> List.hd 
    |> String.map (function 
      | '/' -> '.'
      | c -> c
    )

let _to_syntaxtree (name: string): Ast.module_t = 
  let inx = Core.In_channel.create name in
  let lexbuf = Lexing.from_channel inx in
  (_convert_name name, Parser.g_decls Lexer.token lexbuf)

let to_syntaxtree (name: string): string = 
  _to_syntaxtree name |> Ast.show_module_t

let _to_llvmir (name: string): Misc.context_t = 
  let ast = _to_syntaxtree name in 
  let c = Misc.initialize name in
  ignore (Module.generate c ast);
  Llvm_analysis.assert_valid_module c.m;
  c

let to_llvmir (name: string): string  = 
  let c = _to_llvmir name in 
  let result = Llvm.string_of_llmodule c.m in 
  Misc.finalize c;
  result


let link (names: string list): Llvm.llmodule = 
  let modules = List.map _to_syntaxtree names in 
  let c = Misc.initialize "main" in

  List.iter (fun m -> m |> Ast.show_module_t |> print_endline) modules;

  List.iter (fun (name, decls) -> 
    c.in_module <- Some name; 
    List.iter (Gdecl.predeclare c) decls
  ) modules;

  List.iter (fun (name, decls) -> 
    c.in_module <- Some name; 
    List.iter (Gdecl.generate c) decls
  ) modules;

  c.m