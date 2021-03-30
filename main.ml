
let loop () =
  while true do
    print_endline "hoff> ";
    let str = read_line () in
    let lexbuf = Lexing.from_string str in

    try
      while true do
        ignore(Lexer.token lexbuf)
      done
    with Exit -> ()

  done

let rec print_ast (ast: Ast.g_decl_t list): unit =
  List.iter (fun d -> print_endline (Ast.show_g_decl_t d)) ast;

and compile () =
  let file = Core.In_channel.create "./test.hff" in
  let filebuf = Lexing.from_channel file in
  try
    let ast = Parser.main Lexer.token filebuf in
    print_ast ast;
    ignore (Typecheck.typecheck ast);
    print_endline (Codegen.generate "test" ast)
  with
    | Errors.LexingError e -> Printf.printf "Lexing error %s\n" e
    | Errors.ParseError e -> Printf.printf "Parse error %s\n" e

let () = 
  ignore (Parsing.set_trace true);
  compile ()