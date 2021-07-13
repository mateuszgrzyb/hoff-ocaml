
let _to_syntaxtree (name: string): Ast.module_t = 
  let inx = Core.In_channel.create name in
  let lexbuf = Lexing.from_channel inx in
  Parser.module_ Lexer.token lexbuf

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

