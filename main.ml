


let parse_file (name: string): Ast.module_t = 
  let inx = Core.In_channel.create name in
  let lexbuf = Lexing.from_channel inx in
  Parser.module_ Lexer.token lexbuf


let main () = 
  if Array.length Sys.argv == 1 then 
    print_endline ("Usage: " ^ Sys.argv.(0) ^ " <source file name>")

  else

    try
      let name = Sys.argv.(1) in
      let compiled_ast = parse_file name in 
      (*print_endline (Ast.show_module_t compiled_ast)*)
      let c = Misc.initialize name in
      ignore (Module.generate c compiled_ast);
      Llvm_analysis.assert_valid_module c.m;
      Llvm.dump_module c.m
    with exn -> print_endline (Printexc.to_string exn)


let generate_string (c: Misc.context_t) (value: string): Llvm.llvalue = 
  let generate_global () =
    Llvm.const_stringz c.c value 
  in
  let generate_local () =
    Llvm.build_global_stringptr value "" c.b
  in
  try
    ignore (Llvm.insertion_block c.b);
    generate_local ()
  with Not_found ->
    generate_global ()

let test (): unit =
  let c = Misc.initialize "test" in 

  ignore (Llvm.define_global "global" (generate_string c "global") c.m);

  let ft = Llvm.function_type (Llvm.i8_type c.c |> Llvm.pointer_type) [||] in
  let f = Llvm.declare_function "function1" ft c.m in
  let bb = Llvm.append_block c.c "entry" f in 
  
  Llvm.position_at_end bb c.b;
  let _s = generate_string c "local" in 

  ignore (Llvm.build_ret _s c.b);

  Llvm_analysis.assert_valid_module c.m;
  Llvm.dump_module c.m

let () = main ()