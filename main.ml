open Printf

type module_t =
  { llvm : Llvm.llmodule
  ; ast : Ast.g_decl_t list
  ; name : string
  }

let create_module (c : Codegen.context_t) (name : string) : module_t =
  let file = Core.In_channel.create name in
  let filebuf = Lexing.from_channel file in
  let ast = Parserengine.main filebuf in
  let llvm = Llvm.create_module c.c (Filename.basename name) in
  { ast; llvm; name }
;;

let convert_names (m : module_t) : module_t =
  let prepare_module_name (name : string) : string =
    name
    |> Filename.chop_extension
    |> Core.String.substr_replace_all ~pattern:"/" ~with_:"."
  in
  let name = prepare_module_name m.name in
  let ast = Names.qualify_module name m.ast in
  { llvm = m.llvm; ast; name = m.name }
;;

let compile_module_predecl (c : Codegen.context_t) (m : module_t) : module_t =
  Typecheck.typecheck m.ast;
  Codegen.generate_module_predecl c m.llvm m.ast;
  m
;;

let compile_module (c : Codegen.context_t) (m : module_t) : module_t =
  Codegen.generate_module c m.llvm m.ast;
  m
;;

let link_modules (ms : module_t list) : module_t =
  let link (dst : module_t) (src : module_t) : module_t =
    Llvm_linker.link_modules' dst.llvm src.llvm;
    dst
  in
  match ms with
  | [] -> failwith "empty module list"
  | [ m ] -> m
  | m :: ms -> List.fold_left link m ms
;;

let emit_machine_code (cli_args : Cli.args_t) (m : module_t) : unit =
  let name = cli_args.output_file in
  let ll_name = name |> Filename.chop_extension |> fun n -> n ^ ".ll" in
  Llvm.print_module ll_name m.llvm;
  if cli_args.emit_llvm
  then ()
  else Sys.command ("./helper/clang.sh " ^ ll_name ^ " " ^ name) |> ignore
;;

let run_optimizations (cli_args : Cli.args_t) (m : module_t) : module_t =
  if cli_args.no_opt
  then m
  else (
    let pass_mgr = Llvm.PassManager.create () in
    Llvm_ipo.add_argument_promotion pass_mgr;
    Llvm_ipo.add_constant_merge pass_mgr;
    Llvm_ipo.add_dead_arg_elimination pass_mgr;
    Llvm_ipo.add_function_attrs pass_mgr;
    Llvm_ipo.add_function_inlining pass_mgr;
    Llvm_ipo.add_global_dce pass_mgr;
    Llvm_ipo.add_global_optimizer pass_mgr;
    Llvm_ipo.add_ipc_propagation pass_mgr;
    Llvm_ipo.add_prune_eh pass_mgr;
    Llvm_ipo.add_ipsccp pass_mgr;
    Llvm_ipo.add_internalize ~all_but_main:true pass_mgr;
    Llvm_ipo.add_strip_dead_prototypes pass_mgr;
    Llvm_ipo.add_strip_symbols pass_mgr;
    if not (Llvm.PassManager.run_module m.llvm pass_mgr)
    then Errors.LLVMError "PassManager error" |> raise
    else m)
;;

let compile () =
  let c = Codegen.initialize () in
  try
    let cli_args = Cli.parse () in
    cli_args.input_files
    |> List.map (create_module c)
    |> List.map convert_names
    |> List.map (compile_module_predecl c)
    |> List.map (compile_module c)
    |> link_modules
    |> run_optimizations cli_args
    |> emit_machine_code cli_args
  with
  | Errors.LexingError e -> eprintf "Lexing error %s\n" e
  | Errors.ParseError e -> eprintf "Parse error %s\n" e
  | Errors.NameError e -> eprintf "Name error %s\n" e
  | Errors.TypeError e -> eprintf "Type error %s\n" e
  | Errors.LLVMError e -> eprintf "LLVM error %s\n" e
  | Errors.CLIError e -> eprintf "CLI error %s\n" e
  | e -> eprintf "Unknown exception: %s\n" (Base.Exn.to_string e)
;;

let () = compile ()
