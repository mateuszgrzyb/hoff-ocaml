let loop () =
  while true do
    print_endline "hoff> ";
    let str = read_line () in
    let lexbuf = Lexing.from_string str in
    try
      while true do
        ignore (Lexer.token lexbuf)
      done
    with
    | Exit -> ()
  done
;;

let rec print_ast (ast : Ast.g_decl_t list) : unit =
  List.iter (fun d -> print_endline (Ast.show_g_decl_t d)) ast

and compile ?(debug = false) () =
  let file = Core.In_channel.create "./misc/test.hff" in
  let filebuf = Lexing.from_channel file in
  try
    (*
    let ast = Parser.main Lexer.token filebuf in
    *)
    let ast = Parserengine.main filebuf in
    if debug
    then (
      print_ast ast;
      ignore (Parsing.set_trace false));
    Typecheck.typecheck ast;
    print_endline (Codegen.generate "test" ast)
  with
  | Errors.LexingError e -> Printf.printf "Lexing error %s\n" e
  | Errors.ParseError e -> Printf.printf "Parse error %s\n" e
  | Errors.NameError e -> Printf.printf "Name error %s\n" e
  | Errors.TypeError e -> Printf.printf "Type error %s\n" e
  | e -> Printf.printf "Unknown exception: %s\n" (Base.Exn.to_string e)
;;

(*
let time (f: unit -> unit): unit =
  let t1 = Sys.time () in
  f ();
  let t2 = Sys.time () in
  Printf.printf "Execution time: %f\n" (t2 -. t1)

let with_preprocess () = 
  let file = Core.In_channel.create "./misc/test.hff" in
  Preprocess.main file;
  let file = Core.In_channel.create "./misc/test.hff" in
  compile file

let without_preprocess () =
  let file = Core.In_channel.create "./misc/test.hff" in
  compile file

let main () = 
  ignore (time with_preprocess);
  ignore (time without_preprocess)

let () = main ()
*)

(*
let () = compile ~debug:true ()
   *)
let () = compile ~debug:false ()
