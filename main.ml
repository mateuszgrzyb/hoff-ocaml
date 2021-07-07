

let parse_file (name: string): Ast.module_t = 
  let inx = Core.In_channel.create name in
  let lexbuf = Lexing.from_channel inx in
  Parser.module_ Lexer.token lexbuf

let () = 
  if Array.length Sys.argv == 1 then 
    print_endline ("Usage: " ^ Sys.argv.(0) ^ " <source file name>")
  else
    try
      let compiled_ast = parse_file Sys.argv.(1) in 
      print_endline (Ast.show_module_t compiled_ast)
    with 
      | Errors.LexingError le -> print_endline ("Lexing error: " ^ le)
      | Errors.ParsingError pe -> print_endline ("Parsing error: " ^ pe)
