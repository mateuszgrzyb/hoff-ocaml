
let parse_file (name: string): Ast.module_t = 
  let inx = Core.In_channel.create name in
  let lexbuf = Lexing.from_channel inx in
  Parser.module_ Lexer.token lexbuf

let run () = 
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
      Llvm.dump_module c.m;
      Misc.finalize c
    with exn -> 
      match String.split_on_char '.' (Printexc.to_string exn) with
      | (_::msg::_) -> print_endline msg
      | _ -> failwith "Unknown error msg"


