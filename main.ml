open Core
open Printf

type hoff_module_t =
  { llvm : Llvm.llmodule
  ; ast : Ast.g_decl_t list
  ; name : string
  }

type llvm_module_t =
  { llvm : Llvm.llmodule
  ; name : string
  }

type module_t =
  | Hoff of hoff_module_t
  | LLVM of llvm_module_t

let create_module (c : Codegen.context_t) (file : Cli.file_t) : module_t =
  let name = file.name in
  let filechan = In_channel.create name in
  let filebuf = Lexing.from_channel filechan in
  let llvm = Llvm.create_module c.c (Filename.basename name) in
  match file.ext with
  | Cli.Hoff ->
    let ast = Parserengine.main filebuf in
    Hoff { ast; llvm; name }
  | Cli.IR -> LLVM { llvm; name }
;;

let convert_names (m : module_t) : module_t =
  match m with
  | LLVM _ -> m
  | Hoff { ast; llvm; name } ->
    let prepare_module_name (name : string) : string =
      name |> Filename.chop_extension |> String.substr_replace_all ~pattern:"/" ~with_:"."
    in
    let name = prepare_module_name name in
    let ast = Names.qualify_module name ast in
    Hoff { llvm; ast; name }
;;

let compile_module_predecl (c : Codegen.context_t) (m : module_t) : module_t =
  match m with
  | LLVM _ -> m
  | Hoff { ast; llvm; _ } ->
    Typecheck.typecheck ast;
    Codegen.generate_module_predecl c llvm ast;
    m
;;

let compile_module (c : Codegen.context_t) (m : module_t) : module_t =
  match m with
  | LLVM _ -> m
  | Hoff { ast; llvm; _ } ->
    Codegen.generate_module c llvm ast;
    m
;;

let link_modules (ms : module_t list) : Llvm.llmodule =
  let get_llvm (m : module_t) : Llvm.llmodule =
    match m with
    | LLVM { llvm; _ } | Hoff { llvm; _ } -> llvm
  in
  let link (dst : Llvm.llmodule) (src : Llvm.llmodule) : Llvm.llmodule =
    Llvm_linker.link_modules' dst src;
    dst
  in
  match ms |> List.map ~f:get_llvm with
  | [] -> failwith "empty module list"
  | [ m ] -> m
  | m :: ms -> ms |> List.fold_left ~f:link ~init:m
;;

let emit_machine_code (cli_args : Cli.args_t) (llvm : Llvm.llmodule) : unit =
  let name = cli_args.output_file in
  let ll_name = name |> Filename.chop_extension |> fun n -> n ^ ".ll" in
  Llvm.print_module ll_name llvm;
  if cli_args.emit_llvm
  then ()
  else Sys.command ("./helper/clang.sh " ^ ll_name ^ " " ^ name) |> ignore
;;

let run_optimizations (cli_args : Cli.args_t) (llvm : Llvm.llmodule) : Llvm.llmodule =
  if cli_args.no_opt
  then llvm
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
    if not (Llvm.PassManager.run_module llvm pass_mgr)
    then Errors.LLVMError "PassManager error" |> raise
    else llvm)
;;

let compile () =
  let c = Codegen.initialize () in
  try
    let cli_args = Cli.parse () in
    cli_args.input_files
    |> List.map ~f:(create_module c)
    |> List.map ~f:convert_names
    |> List.map ~f:(compile_module_predecl c)
    |> List.map ~f:(compile_module c)
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
