type cli_args_t =
  { input_files : string list
  ; output_file : string
  ; emit_llvm : bool
  ; no_opt : bool
  }

type module_t =
  { llvm : Llvm.llmodule
  ; ast : Ast.g_decl_t list
  }

let parse_cli () : cli_args_t =
  let input_files = ref [] in
  let output_file = ref "main.x" in
  let workdir = ref "" in
  let emit_llvm = ref false in
  let no_opt = ref false in
  let pos_args arg = input_files := arg :: !input_files in
  let spec_list =
    [ "-o", Arg.Set_string output_file, "Set output file name"
    ; "-dir", Arg.Set_string workdir, "Set project workdir"
    ; "-emit-llvm", Arg.Set emit_llvm, "Emit LLVM IR"
    ; "-no-opt", Arg.Set no_opt, "Disable optimisations"
    ]
  in
  let usage_msg = "hoff <-o output> <-dir workdir> <-emit-llvm> <-no-opt> [files]" in
  Arg.parse spec_list pos_args usage_msg;
  input_files := !input_files |> List.map (Filename.concat !workdir);
  { input_files = !input_files
  ; output_file = !output_file
  ; emit_llvm = !emit_llvm
  ; no_opt = !no_opt
  }
;;

let create_module (c : Codegen.context_t) (name : string) : module_t =
  let file = Core.In_channel.create name in
  let filebuf = Lexing.from_channel file in
  let ast = Parserengine.main filebuf in
  let llvm = Llvm.create_module c.c (Filename.basename name) in
  { ast; llvm }
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

let emit_machine_code (cli_args : cli_args_t) (m : module_t) : unit =
  let name = cli_args.output_file in
  let ll_name = name |> Filename.chop_extension |> fun n -> n ^ ".ll" in
  Llvm.print_module ll_name m.llvm;
  if cli_args.emit_llvm
  then ()
  else Sys.command ("./helper/clang.sh " ^ ll_name ^ " " ^ name) |> ignore
;;

let run_optimizations (cli_args : cli_args_t) (m : module_t) : module_t =
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
  let cli_args = parse_cli () in
  let c = Codegen.initialize () in
  try
    cli_args.input_files
    |> List.map (create_module c)
    |> List.map (compile_module_predecl c)
    |> List.map (compile_module c)
    |> link_modules
    |> run_optimizations cli_args
    |> emit_machine_code cli_args
  with
  | Errors.LexingError e -> Printf.eprintf "Lexing error %s\n" e
  | Errors.ParseError e -> Printf.eprintf "Parse error %s\n" e
  | Errors.NameError e -> Printf.eprintf "Name error %s\n" e
  | Errors.TypeError e -> Printf.eprintf "Type error %s\n" e
  | Errors.LLVMError e -> Printf.eprintf "LLVM error %s\n" e
  | e -> Printf.eprintf "Unknown exception: %s\n" (Base.Exn.to_string e)
;;

let () = compile ()
