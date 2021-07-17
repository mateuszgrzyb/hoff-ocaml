



let () = 
  let args = Cli.parse_args () in
 
  (*
  let compile = match args.result with
  | LlvmIr -> Compiler.to_llvmir
  | SyntaxTree -> Compiler.to_syntaxtree
  | _ -> failwith "Not Implemented" in

  let modules = List.map compile args.modules in
  
  List.iter print_endline modules;
  *)

  let module_ = Compiler.link args.modules in
  Llvm.dump_module module_